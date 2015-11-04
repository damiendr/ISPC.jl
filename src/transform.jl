# Transformations of Julia ASTs containing ISPC fragments.

"""
Removes every `LineNumberNode` in the AST (eg. for clearer output).
"""
function strip_lineno(obj)
    if isa(obj, Expr)
        args = map(strip_lineno, obj.args)
        filter!((o) -> !isa(o, LineNumberNode), args)
        filter!((o) -> !(isa(o, Expr) && (o.head == :line)), args)
        return Expr(obj.head, args...)
    end
    return obj
end


"""
Replaces all calls to `Base.box()` with the unboxed value.
"""
function strip_box(obj)
    if isa(obj, Expr)
        if obj.head == :call
            # Unbox all values:
            if obj.args[1] == GlobalRef(Base, :box)
                # args[3] holds the boxed value:
                obj = obj.args[3]
            end
        end
        args = map(strip_box, obj.args)
        return Expr(obj.head, args...)
    end
    return obj
end


"""
Calls `func()` on every node in `expr`.
"""
function walk(func, expr)
    func(expr)
    if isa(expr, Expr)
        func(expr.head)
        for o in expr.args
            walk(func, o)
        end
    end
end


"""
Traverses `expr` and replaces nodes according to the
substitutions in dict `subst`.
""" 
function substitute(expr, subst)
    if haskey(subst, expr)
        return subst[expr]
    end
    if isa(expr, Expr)
        head = substitute(expr.head, subst)
        args = [substitute(a, subst) for a in expr.args]
        return Expr(head, args...)
    end
    return expr
end


"""
Returns the jump target if `stmt` is a branch, `nothing` otherwise.
"""
function branch_target(stmt)
    if isa(stmt, GotoNode)
        return stmt.label
    elseif isa(stmt, Expr)
        if stmt.head == :gotoifnot
            return stmt.args[2]
        end
    end
    return nothing
end


"""
Replaces all jump targets in `expr` with the associated values in `new_labels`.
"""
function replace_labels(expr, new_labels)
    if isa(expr, GotoNode)
        return GotoNode(new_labels[expr.label])
    end
    if isa(expr, Expr)
        if expr.head == :gotoifnot
            block, goto = expr.args
            return Expr(expr.head, replace_labels(block, new_labels), new_labels[goto])
        end
        return Expr(expr.head, [replace_labels(a, new_labels) for a in expr.args]...)
    end
    return expr
end


"""
Replaces jumps to labels with jumps to statement indices (aka line numbers or
program counters) and removes all `LabelNodes` from the AST. This transform
makes it easier to analyse branching patterns.
"""
function labels_to_lines(block)
    labels = Dict()
    program = []
    for stmt in block.args
        if isa(stmt, LabelNode)
            labels[stmt.label] = length(program) + 1
        else
            push!(program, stmt)
        end
    end
    newblock = Expr(block.head, program...)
    replace_labels(newblock, labels)
end


"""
Adds explicit labels for jumps to line/statement numbers. If the AST had been
transformed by `labels_to_lines()`, this turns it back into a form that Julia
can handle.
"""
function lines_to_labels(statements)
    targets = Set()
    program = []
    for stmt in statements
        target = branch_target(stmt)
        if target != nothing
            @assert 1 <= target <= length(statements)
            push!(targets, target)
        end
    end
    for (i, stmt) in enumerate(statements)
        if i in targets
            push!(program, LabelNode(i))
        end
        push!(program, stmt)
    end
    program
end


"""
Raises the goto branches found in lowered AST to if/while constructs.
"""
function raise_ast(statements, range)
    # In general it might seem very hard to transform gotos back to structured
    # control statements, but we're not dealing with that general problem here:
    # the only gotos in the expression tree are those that were introduced by
    # the AST lowering pass. In these conditions it turns out that simple rules 
    # can recover `if`, `if-else` and `while` statements (`for` statements are
    # recovered as an equivalent `while`).
    program = []
    line = start(range)
    while !done(range, line)
        stmt = statements[line]
        if isa(stmt, Expr) && stmt.head == :gotoifnot
            # We have an if, if-else or while construct.
            condition, target = stmt.args

            # Let's look at the statement just before the jump target, this will tell
            # us what kind of control flow we are dealing with:
            tail = statements[target-1]
            after = branch_target(tail)
            if after != nothing
                # The branch body ends with another branch. Let's investigate more:
                if isa(tail, GotoNode)
                    @assert after > target > line
                    # The branch body ends with a forward goto:
                    # if-else construct
                    if_block = raise_ast(statements, line+1:target-2)
                    else_block = raise_ast(statements, target:after-1)
                    if_expr = Expr(:if, condition, if_block, else_block)
                    push!(program, if_expr)
                    line = after

                elseif isa(tail, Expr) && tail.head == :gotoifnot
                    @assert target > line
                    @assert after == line + 1
                    # The branch body ends with a gotoifnot back to the start:
                    # while construct
                    while_block = raise_ast(statements, line+1:target-2)
                    while_expr = Expr(:while, condition, while_block)
                    push!(program, while_expr)
                    line = target
                end
            else
                # The branch body does not end with another branch:
                # if construct
                if_block = raise_ast(statements, line+1:target-2)
                if_expr = Expr(:if, condition, if_block)
                push!(program, if_expr)
                line = target
            end
        else
            @assert branch_target(stmt) == nothing
            push!(program, stmt)
            cur, line = next(range, line)
        end
    end
    Expr(:block, program...)
end


"""
Finds the symbols in `statements` that are defined outside of `range`.
"""
function extract_params(statements, range)
    decls = Set()
    for stmt in statements[range]
        walk(stmt) do o
            if isa(o, Expr) && o.head == :(=)
                push!(decls, o.args[1])
            end
        end
    end

    symbols = Set()
    for stmt in statements[range]
        walk(stmt) do o
            if isa(o, SymbolNode)
                if !(o.name in decls)
                    push!(symbols, (o.name, o.typ))
                end
            end
        end
    end
    [symbols...] # we need the ordering to be consistent from now on.
end


"""
Finds the ranges of statements that correspond to ISPC fragments.
"""
function ispc_ranges(statements)
    ranges = []
    cur_id = nothing
    start = 0
    for (i, stmt) in enumerate(statements)
        if isa(stmt, Expr) && stmt.head == :meta
            if stmt.args[1] == :ispc
                identifier = stmt.args[2]
                if cur_id == nothing && identifier != :coherent
                    # opening tag
                    cur_id = identifier
                    start = i
                elseif cur_id == identifier
                    # closing tag
                    cur_id = nothing
                    push!(ranges, (start:i, identifier))
                end
            end
        end
    end
    return ranges
end


@noinline function ispc_call(sym, args...)
    println("Calling $sym$args")
    nothing
end


"""
Registers a new ISPC fragment.
"""
function add_ispc_function!(funcs, identifier, params, ast)
    id = length(funcs)
    func_name = Symbol("ispc_func_$id")
    func_args = [sym for (sym, typ) in params]
    func_call = Expr(:call, :(ISPC.ispc_call), QuoteNode(func_name), func_args...)
    funcs[identifier] = (func_call, params, ast)
end


"""
Extracts ISPC fragments in `body` and return a dict of ISPC functions.
"""
function extract_ispc(body)
    # Simplify jump analysis by replacing labels with line numbers:
    statements = labels_to_lines(body).args

    # Extract all top-level spans of ISPC code and replace them with
    # calls to generated ISPC functions:
    functions = Dict()
    for (range, identifier) in ispc_ranges(statements)
        # Recover a structured control flow:
        ast = raise_ast(statements, range)
        # Get rid of the nodes that we won't need for ISPC generation:
        ast = strip_box(strip_lineno(ast)) # yield a much cleaner AST
        # Find out which outer variables are used within that fragment,
        # they will become arguments to the ISPC function:
        params = extract_params(statements, range)
        # Turn the fragment into an ISPC function:
        add_ispc_function!(functions, identifier, params, ast)
    end
    functions
end

"""
Replaces ISPC fragments in `body` with calls to the corresponding
ISPC function in `functions`.
"""
function replace_calls(body, functions)
    if isa(body, Expr)
        statements = copy(body.args)
        for (range, identifier) in ispc_ranges(body.args)
            func_call = functions[identifier][1]
            statements[range] = nothing
            statements[range.start] = func_call
        end
        filter!(o -> o != nothing, statements)
        return Expr(body.head, [replace_calls(o, functions) for o in statements]...)
    else
        return body
    end
end

