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
function labels_to_lines(statements)
    labels = Dict()
    program = []
    for stmt in statements
        if isa(stmt, LabelNode)
            labels[stmt.label] = length(program) + 1
        else
            push!(program, stmt)
        end
    end
    newblock = Expr(:block, program...)
    replace_labels(newblock, labels).args
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
function raise_ast(statements, range=1:length(statements))
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


function meta_to_trees(expr::Expr)
    branches = []
    statements = expr.args
    for (range, identifier) in ispc_ranges(statements)
        if identifier == nothing
            append!(branches, statements[range])
        else
            meta = statements[range.start]
            @assert meta.head == :meta
            meta_op = meta.args[3]
            meta_args = (meta.args[4:end]...)
#            println("$meta $meta_op, $meta_args")
            body = statements[(range.start+1):(range.stop-1)]
            tree = meta_to_trees(Expr(:block, body...))
            branch_expr = Expr(meta_op, meta_args..., tree)
            push!(branches, branch_expr)
        end
    end
    Expr(expr.head, branches...)
end
    

collect_foreach_indices(expr::Any) = expr
function collect_foreach_indices(expr::Expr)
    args = expr.args
    if expr.head == :foreach
        iters = []
        arrays, block = args
        block = collect_foreach_indices(block)
        statements = block.args
        filter!(statements) do expr
            if expr.head == :(=)
                lhs, rhs = expr.args
                if isa(rhs, Expr) && rhs.head == :call
                    if rhs.args[1] == GlobalRef(ISPC, :foreachindex)
                        push!(iters, rhs.args[2] => lhs)
                        return false
                    end
                end
            end
            true
        end
        return Expr(:foreach, iters, Expr(:block, statements...))
    end
    args = [collect_foreach_indices(arg) for arg in args]
    Expr(expr.head, args...)
end


function resolve_topnodes(ast::Expr)
    # We are likely to encounter two types of getfield calls:
    # call(TopNode(getfield), GlobalRef, QuoteNode):
    #       accesses a function in a module
    # call(TopNode(getfield), GenSym, QuoteNode):
    #       accesses a field in a local object
    expr
end


