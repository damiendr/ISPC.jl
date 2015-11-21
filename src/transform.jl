# Transformations of Julia ASTs containing ISPC fragments.

"""
Removes every `LineNumberNode` in the AST (eg. for clearer output).
"""
function strip_lineno(expr::Expr)
    args = strip_lineno(expr.args)
    newargs = map(strip_lineno, args)
    return Expr(expr.head, newargs...)
end

function strip_lineno(array::Vector{Any})
    filter(array) do obj
        !(isa(obj, LineNumberNode) || isa(obj, Expr) && (obj.head == :line))
    end
end

function strip_lineno(obj::Any)
    obj
end

"""
Replaces all calls to `Base.box()` with the unboxed value.
"""
strip_box(obj::Any) = obj
function strip_box(expr::Expr)
    if expr.head == :call
        # Unbox all values:
        if expr.args[1] == GlobalRef(Base, :box)
            # args[3] holds the boxed value:
            return strip_box(expr.args[3])
        end
    end
    args = map(strip_box, expr.args)
    return Expr(expr.head, args...)
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


import Graphs: add_edge!, in_degree, out_degree, simple_graph, in_neighbors

function to_graph(statements, i=1, g=simple_graph(length(statements)))
    while i <= length(statements)
        if out_degree(i, g) > 0
            break
        end
        stmt = statements[i]
        target = branch_target(stmt)
        if isa(stmt, GotoNode)
            add_edge!(g, i, target)
            i = target
            continue
        end
        if (i < length(statements))
            add_edge!(g, i, i+1)
        end
        if target != nothing
            add_edge!(g, i, target)
            to_graph(statements, target, g)
        end
        i += 1
    end

    return g
end


function print_statements(io, statements)
    for (i, stmt) in enumerate(statements)
        println(io, "$i\t$stmt")
    end
end


function push_visited!(s::Set, items...)
    # -- debugging code goes here --
    for i in items
        Base.push!(s, i)
    end
end


"""
Try to lift patterns such as this:
    if condition
      do
         block
      while !!condition
    end

into a simpler form:
    while condition
       block
    end

"""
function raise_if_then_else(condition, ifthen, ifelse)
    if length(ifthen) == 1 && length(ifelse) == 0
        expr, = ifthen
        if isa(expr, Expr) && expr.head == :dowhile
            docondition, doblock = expr.args
            # Test that the conditions match via their string representation.
            # Not the most elegant way but the most maintainable that I've
            # found so far.
            # They should be logically identical but differ syntactically by 
            # a double negation:
            expected = ("!((Base.box)(Base.Bool,(Base.not_int)($condition)))",
                        "!((top(!))($condition))")
            if string(docondition) in expected
                return Expr(:while, condition, doblock)
            end
        end
    end

    # Create a structured `if` block:
    if isempty(ifelse)
        return Expr(:if, condition, Expr(:block, ifthen...))
    else
        return Expr(:if, condition, Expr(:block, ifthen...),
                                    Expr(:block, ifelse...))
    end
end


"""
Transform unstructured control flow into structured statements.

We make a number of assumptions about the control flow:
- a backwards conditional jump always correspond
  to the end of a loop
- there are no unconditional backwards jumps.

So far these assumptions seem to hold for everything that Julia
generates.
"""
function raise_ast(statements, first=1, visited=Set(),
                    graph=to_graph(statements),
                    continue_to=nothing,
                    break_to=nothing)
    body = []
    i = first
    while i <= length(statements)
        stmt = statements[i]

        # println("@ $i, first: $first, visited: $visited")
        # println("continue: $continue_to, break: $break_to")

        # Any convergent flow?
        # =============================

        # Distinguish between forward and backward jumps to this
        # statement:
        antecedents = Set(in_neighbors(i, graph))
        incoming = filter(b -> b < i, antecedents)
        loopback = filter(b -> b > i, antecedents)

        if length(incoming) > 1
            # More than one branch converge onto this statement.
            # Don't enter until they have all been followed:
            if !(issubset(incoming, visited))
                # flow has not reconverged yet.
                # println("not converged at $i.")
                return body, i
            end
        end

        # println("enter")

        if !isempty(loopback)
            # A future statement points back here, that must be a
            # do-while loop. Make sure that the `continue_to` and
            # `break_to` arguments are set accordingly:
            cont, = loopback
            brk = cont + 1
            if (continue_to, break_to) != (cont, brk)
                # That's a new do-while block. Recurse with the new
                # break/continue arguments:
                # println("start do-while")
                stmt, tail = raise_ast(statements, i, visited, graph,
                                       cont, brk)
                push!(body, stmt...)

                if tail != nothing
                    # println("end do-while: continue $tail")
                    i = tail
                    continue
                else
                    # println("end do-while: return $brk")
                    return body, brk
                end
            else
                # We're already inside that do-while block.
            end
        end

        # Any divergent flow?
        # =============================

        if isa(stmt, GotoNode)
            # Unconditional branch.
            target = branch_target(stmt)
            push_visited!(visited, i)

            # If the branch target matches one of our `continue`
            # or `break` targets, insert the corresponding statement:
            if target == break_to
                push!(body, Expr(:break))
                return body, nothing # don't follow breaks, they take
                                     # us out of the current do-while

            elseif target == continue_to
                push!(body, Expr(:continue))
                return body, continue_to # follow that one to the
                                         # do-while loop check

            elseif target <= i
                print_statements(STDERR, statements)
                error("Unhandled backward goto: $i -> $target")

            else
                # Just follow the jump (and discard the
                # goto statement).
                i = target
                continue
            end
        end

        if isa(stmt, Expr) && stmt.head == :gotoifnot
            # Conditional branch: transform it to a structured
            # control flow statement.
            condition, target = stmt.args

            if target < i
                # This looks like the end of a do-while block...

                if target != first || i != continue_to
                    # ... but not of the one we're processing at
                    # the moment! Let the caller handle it:
                    # println("return to parent do-while at $i")
                    return body, i
                end

                # The body is formed by all previous statements
                # since the start of the current branch:
                push_visited!(visited, i)

                # We need to negate the condition to match do-while
                # semantics (the goto statements form a do-while-not)
                while_condition = :(!$condition)
                stmt = Expr(:dowhile, while_condition, Expr(:block, body...))
                return [stmt], break_to

            else
                # This is an `if-then-else` block.

                # Don't add the `if` statement itself to the set of
                # visited statements until all the branches have been
                # explored, otherwise the last branch to be followed
                # might swallow whatever comes after the `if` block
                # (eg. when the `else` branch is empty).

                # Follow the two branches independently until they
                # reconverge.
                visitedA = Set()
                ifthen, tailsA = raise_ast(statements, i+1, visitedA,
                                            graph, continue_to, break_to)

                visitedB = Set()
                ifelse, tailsB = raise_ast(statements, target, visitedB,
                                            graph, continue_to, break_to)

                # Merge the branch histories:
                push_visited!(visited, visitedA...)
                push_visited!(visited, visitedB...)
                push_visited!(visited, i)

                # println("IF at $i converged to $tailsA, $tailsB")

                # Build an `if` or `while` structured control flow:
                stmt = raise_if_then_else(condition, ifthen, ifelse)
                push!(body, stmt)

                # The tails should be all equal to each other or
                # `nothing`:
                tails = Set((tailsA, tailsB))
                delete!(tails, nothing)
                if length(tails) > 1
                    # The flow did not reconverge properly. This might be
                    # caused by a bug or by gotos that we don't know how
                    # to handle.
                    error("Invalid reconvergence at $i: $tailsA, $tailsB")
                elseif length(tails) == 1
                    # One of the branches has a non-void tail, follow up
                    # on that:
                    i, = tails
                    continue
                else
                    # Both tails are void.
                    return body, nothing
                end
            end
        end

        # Branchless statement:
        # =============================

        push!(visited, i)
        push!(body, stmt)
        i += 1
    end

    # We reached the end of the flow:
    return body, nothing
end

function meta_to_trees(expr::Any)
    expr
end

function meta_to_trees(expr::Expr)
    branches = []
    statements = expr.args
    for (range, identifier) in ispc_ranges(statements)
        # println("@ $range, $identifier")
        if identifier == nothing
            for stmt in statements[range]
                push!(branches, meta_to_trees(stmt))
            end
        else
            meta = statements[range.start]
            @assert meta.head == :meta
            meta_op = meta.args[3]
            meta_args = (meta.args[4:end]...)
            # println("$meta $meta_op, $meta_args")
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
        # for arr in arrays
        #     if isa(arr, Expr) && arr.head == :quote
        #         push!(iters, arr.args[1])
        #     end
        # end
        block = collect_foreach_indices(block)
        statements = block.args
        filter!(statements) do expr
            if isa(expr, Expr) && expr.head == :(=)
                lhs, rhs = expr.args
                if isa(rhs, Expr) && rhs.head == :call
                    if rhs.args[1] == GlobalRef(ISPC, :foreachindex)
                        idx = eval(rhs.args[2])
                        push!(iters, arrays[idx] => lhs)
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


