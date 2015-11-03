
function print_lambda(expr)
    @assert expr.head == :lambda
    arg_symbols = expr.args[1]
    local_vars = expr.args[2][1]
    env_vars = expr.args[2][2]
    gen_syms = expr.args[2][3]
    param_symbols = expr.args[2][4]
    tree = expr.args[3]
    
    println("Lambda function:")
    println("\t arguments: $arg_symbols")
    println("\t local variables: ")
    println(join(map(o -> "\t\t$((o...,))", local_vars), "\n"))
    println("\t closure variables: ")
    println(join(map(o -> "\t\t$((o...,))", env_vars), "\n"))
    println("\t SSA types: $gen_syms")
    println("\t parameters: $param_symbols")
    println(strip_box(strip_lineno(tree)))
end

"""
Returns a typed AST for function `f`.
"""
function typed_ast(f::Function, argtype::Type{Tuple})
    (ast, ret_type) = Core.Inference.typeinf(f.code, argtype, f.env)
    ast_typed = ccall(:jl_uncompress_ast, Any, (Any,Any), f.code, ast)
end

"""
Removes every `LineNumberNode` in the AST (eg. for clearer output).
"""
function strip_lineno(obj)
    if isa(obj, Expr)
        args = map(strip_lineno, obj.args)
        filter!((o) -> !isa(o, LineNumberNode), args)
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
Finds the ranges of statements that correspond to ISPC programs.
"""
function ispc_ranges(statements)
    ranges = []
    cur_id = nothing
    start = 0
    for (i, stmt) in enumerate(statements)
        if isa(stmt, Expr) && stmt.head == :meta
            if stmt.args[1] == :ispc
                if cur_id == nothing
                    cur_id = stmt.args[2]
                    start = i
                elseif cur_id == stmt.args[2]
                    cur_id = nothing
                    push!(ranges, start:i)
                end
            end
        end
    end
    return ranges
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

function walk(func, expr)
    func(expr)
    if isa(expr, Expr)
        for o in expr.args
            walk(func, o)
        end
    end
end

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
    [symbols...] # we need the ordering to be consistent.
end


function gen_ispc_function!(dest, params, ast)
    id = length(dest)
    func_name = Symbol("ispc_func_$id")
    func_args = [sym for (sym, typ) in params]
    push!(dest, (func_name, params, ast))
    return Expr(:call, func_name, func_args...)
end


function extract_ispc(block)
    # Simplify jump analysis by replacing labels with line numbers:
    block = labels_to_lines(block)

    # Extract all top-level spans of ISPC code and replace them with
    # calls to generated ISPC functions:
    statements = block.args
    new_statements = copy(statements)
    functions = []
    for range in ispc_ranges(statements)
        ast = raise_ast(statements, range)
        ast = strip_box(strip_lineno(ast))
        params = extract_params(statements, range)
        call = gen_ispc_function!(functions, params, ast)
        new_statements[range.start] = call
        new_statements[range.start+1:range.stop] = nothing
    end

    # Turn the AST back into a form that Julia can handle:
    new_statements = lines_to_labels(new_statements)
    filter!(o -> o != nothing, new_statements)
    new_block = Expr(block.head, new_statements...)
    new_block, functions
end



function process_ast(func, argtypes)
    typed = code_typed(func, argtypes)
    func_code = typed[1].args[3]
    block = strip_box(strip_lineno(func_code))
    dontgoto(block.args)
end

