# Transformations of Julia ASTs containing ISPC fragments.

"""
Removes every `LineNumberNode` in the AST (eg. for clearer output).
"""
function strip_lineno(expr::Expr)
    return Expr(expr.head, strip_lineno(expr.args)...)
end

function strip_lineno(array::Vector{Any})
    filter(array) do obj
        !(isa(obj, LineNumberNode) || isa(obj, Expr) && (obj.head == :line))
    end
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

import Graphs: add_edge!, in_degree, out_degree, simple_graph, in_neighbors

type FlowEdge
    visited::Bool
end

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


function push_visited!(s::Set, i...)
    # if 16 in i
    #     error("16")
    # end
    Base.push!(s, i...)
end


function raise_ast(statements, first=1, visited=Set(),
                    graph=to_graph(statements),
                    continue_to=nothing,
                    break_to=nothing)
    body = []
    i = first
    while i < length(statements)
        stmt = statements[i]

        # Any convergent flow?
        # =============================

        # Distinguish between forward and backward jumps to this
        # statement:
        antecedants = Set(in_neighbors(i, graph))
        incoming = filter(b -> b < i, antecedants)
        loopback = filter(b -> b > i, antecedants)

        # println("#### visited: $visited")
        # println("@@@@ $i converge $incoming $loopback")
        # println("@@@@ head: $first continue: $continue_to, break: $break_to")
        # flush(STDOUT)
        # timedwait(0.1) do
        #     false
        # end

        if length(incoming) > 1
            # More than one branch converge onto this statement.
            # Don't enter until they have all been followed:
            if !(issubset(incoming, visited))
                # flow has not reconverged yet.
                # println("return")
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
                stmt, tail = raise_ast(statements, i, visited, graph,
                                       cont, brk)
                push!(body, stmt...)
                i = tail
                continue
            else
                # We're already inside that do-while block.
            end
        end

        # Any divergent flow?
        # =============================

        if isa(stmt, GotoNode)
            # Unconditional branch, not a divergence.
            push_visited!(visited, i)
            target = branch_target(stmt)
            if target == break_to
                push!(body, Expr(:break))
                return body, nothing # don't follow
            elseif target == continue_to
                push!(body, Expr(:continue))
                return body, nothing # don't follow
            elseif target <= i
                print_statements(STDERR, statements)
                error("Unhandled backwards goto: $i -> $target")
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

            if target == first
                # This is a `do-while` block. Its body is formed
                # by all previous statements since the start of
                # the current branch:
                println("DOWHILE at $i since $first")
                push_visited!(visited, i)
                stmt = Expr(:dowhile, condition, Expr(:block, body...))
                return [stmt], break_to

            else
                # This is an `if-then-else` block.
                println("IF at $i: $visited")

                # Don't add the `if` statement itself to the set of
                # visited statements until all the branches have been
                # explored, otherwise the last branch to be followed
                # would swallow whatever comes after the `if` block
                # whenever one of the `if` branches is empty.

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
                
                # Create a structured `if` block:
                stmt = if isempty(ifelse)
                    Expr(:if, condition, Expr(:block, ifthen...))
                else
                    Expr(:if, condition, Expr(:block, ifthen...),
                                         Expr(:block, ifelse...))
                end
                push!(body, stmt)


                tails = Set((tailsA, tailsB))
                delete!(tails, nothing)


                println("IF at $i")
                println("$tails")

                if length(tails) > 1
                    error("Invalid if convergence at $i: $tails")
                elseif length(tails) == 1
                    i, = tails
                    continue
                else
                    return body, nothing
                end
            end

        else
            # Branchless statement:
            push!(visited, i)
            push!(body, stmt)
            i += 1
        end
    end

    # Either we reached a convergence point or the end of the flow:
    return body, nothing
end


#         # # First check if we reached a reconvergence point:
#         # if i != range.start && infos.flow_degree[i] > 1
#         #     @assert infos.jump_degree[i] == 0
#         #     # Stop there and let the caller decide what to do:
#         #     break
#         # end

#         # if i != range.start && infos.jump_degree[i] > 0
#         #     # We reached a jump target: collapse the nested branch
#         #     @assert infos.flow_degree[i] == 1
#         #     stmt, tail = raise_control_flow(statements, i:range.stop, infos)
#         #     push!(body, stmt)
#         #     i = tail
#         #     continue
#         # end


# function convergent(statements, incident, range::UnitRange)
#     body = []
#     i = range.start
#     while i < range.stop
#         stmt = statements[i]
#         target = branch_target(stmt)
#         if isa(stmt, GotoNode)
#             i = target
#             continue
#         elseif isa(branch, Expr) && branch.head == :gotoifnot
#             if target == range.start
#                 # do-while loop

#                 return 
#             else
#                 # if-else
#             end
#         end
#         push!(body, stmt)
#     end
# end




# function reconverge(statements, incident, range::UnitRange)
#     branch = statements[range.start]
#     @assert isa(branch, Expr) && branch.head == :gotoifnot
#     condition, target = branch.args

#     # Follow each of the divergent paths:
#     spans = = (range.start+1:range.stop, target:range.stop)
#     paths = [follow_branch(statements, incident, span) for span in spans]
#     branches, tails = zip(paths...)

#     # Where did the divergent paths end up?
#     # This will tell us more about what kind of control flow
#     # we are dealing with.

#     if length(unique(tails)) == 1
#         # The paths reconverged. This is an `if` construct.
#         expr = Expr(:if, condition, branches[1], branches[2])
#         return expr, tails[1]

#     elseif length(unique(tails)) == 2
#         # The paths did not converge.
#         # Do the two tail nodes have the same predecessors?
#         if incident[tails[1]] == incident[tails[2]]
#             # This looks suspiciously like a `while` construct!
#             if range.start in incident[tails[1]]
#             end
#         end
#     end

#     # What the hell was that?
#     print_statements(STDERR, statements)
#     error("Unhandled control flow: $range")
# end

# function reconverge(statements, incident, branches)
#     tails = [node for (branch, node) in branches]
#     while length(unique(tails)) > 1
#         next_branch = indmin(tails)

#     end
#     tails = [follow_branch(statements, incident, path) for path in paths]
# end

# """
# Removes unreachable statements by tracing execution paths
# and removing unvisited lines:
# """
# function remove_dead_code!(statements)
#     visited = Set()
#     follow_branches(statements, 1, visited)
#     for (i, stmt) in enumerate(statements)
#         if !(i in visited)
#             print("@@@")
#         end
#         print("$i\t")
#         println(stmt)
#     end
#     unreachable = setdiff(1:length(statements), visited)
#     deleteat!(statements, sort(unreachable))
# end

# function follow_branches(statements, pc, visited)
#     i = pc
#     while i <= length(statements)
#         if i in visited
#             return
#         end
#         stmt = statements[i]
#         push!(visited, i)
#         if isa(stmt, GotoNode)
#             i = branch_target(stmt)
#             continue
#         end
#         if isa(stmt, Expr) && stmt.head == :gotoifnot
#             follow_branches(statements, branch_target(stmt), visited)
#         end
#         i += 1
#     end
# end


# """
# Raises the goto branches found in lowered AST to if/while constructs.
# """
# function raise_ast(statements, range=1:length(statements))
#     # In general it might seem very hard to transform gotos back to structured
#     # control statements, but we're not dealing with that general problem here:
#     # the only gotos in the expression tree are those that were introduced by
#     # the AST lowering pass. In these conditions it turns out that simple rules 
#     # can recover `if`, `if-else` and `while` statements (`for` statements are
#     # recovered as an equivalent `while`).
#     program = []
#     line = start(range)
#     while line <= range.stop
#         stmt = statements[line]
#         if isa(stmt, Expr) && stmt.head == :gotoifnot
#             # We have an if, if-else or while construct.
#             condition, target = stmt.args

#             # Let's look at the statement just before the jump target, this will tell
#             # us what kind of control flow we are dealing with:
#             tail = statements[target-1]
#             after = branch_target(tail)
#             if after != nothing
#                 # The branch body ends with another branch. Let's investigate more:
#                 if isa(tail, GotoNode)
#                     @assert after > target > line
#                     # The branch body ends with a forward goto:
#                     # if-else construct
#                     if_block = raise_ast(statements, line+1:target-2)
#                     else_block = raise_ast(statements, target:after-1)
#                     if_expr = Expr(:if, condition, if_block, else_block)
#                     push!(program, if_expr)
#                     line = after

#                 elseif isa(tail, Expr) && tail.head == :gotoifnot
#                     @assert target > line
#                     @assert after == line + 1
#                     # The branch body ends with a gotoifnot back to the start:
#                     # while construct
#                     while_block = raise_ast(statements, line+1:target-2)
#                     while_expr = Expr(:while, condition, while_block)
#                     push!(program, while_expr)
#                     line = target
#                 end
#             else
#                 # The branch body does not end with another branch:
#                 # if construct
#                 if_block = raise_ast(statements, line+1:target-2)
#                 if_expr = Expr(:if, condition, if_block)
#                 push!(program, if_expr)
#                 line = target
#             end
#         else
#             if isa(stmt, GotoNode)
#                 # Follow the goto:
#                 line = branch_target(stmt)
#             else
#                 push!(program, stmt)
#                 cur, line = next(range, line)
#             end
#         end
#     end
#     Expr(:block, program...)
# end


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


"""
Julia lowers some mathematical functions to ccals to libm.
We need to trap these cases and substitute something that
is easier for the code generator to bind to ISPC's stdlib.
"""
function rewrite_libm_calls(ast::Expr)
    substitute(ast) do expr
        if isa(expr, Expr) && expr.head == :call
            func = expr.args[1]
            args = expr.args[2:end]
            if func == TopNode(:ccal)
                dump(args)
            end
        end
    end
end

