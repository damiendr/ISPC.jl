
using Base.Meta

"""

"""
function make_ispc_main(func_lowered::Expr)
    substitute(func_lowered) do expr
#        println(expr)
        if isa(expr, Expr) && expr.head == :call
            func = expr.args[1]
            if func == GlobalRef(ISPC, :ispc_fragment)
                identifier, lambda, options = expr.args[2:end]

                arg_names = lambda.ast.args[1]
                captured = lambda.ast.args[2][2]
                body = lambda.ast.args[3]

                captured_names = [varinfo[1] for varinfo in captured]
                kernel_argnames = Any[arg_names..., captured_names...]

                println("Extracted fragment $identifier")
                println("Arguments: $(kernel_argnames)")
                println("Body: $(strip_lineno(body))")

                # identifier is a Val{symbol}, unbox the symbol:
                id = identifier.parameters[1]
                ISPC.ispc_fragments[id] = lambda
                ISPC.ispc_fragment_opts[id] = options

                kernel_call = :(ISPC.call_fragment($identifier,
                                                   $(kernel_argnames...)))
                 # Don't forget we're dealing with lowered ASTs, expand:
                return expand(kernel_call)
            end
        end
        nothing
    end
end



"""
Finds the ranges of statements that correspond to ISPC fragments.
"""
function ispc_ranges(statements)
    ranges = []
    cur_id = nothing
    start = 1
    for (i, stmt) in enumerate(statements)
        if isa(stmt, Expr) && stmt.head == :meta
            if stmt.args[1] == :ispc
                identifier = stmt.args[2]
                if cur_id == nothing && identifier != :coherent
                    # opening tag
                    prev_range = start:i-1
                    if !isempty(prev_range)
                        push!(ranges, (prev_range, nothing))
                    end
                    cur_id = identifier
                    start = i
                elseif cur_id == identifier
                    # closing tag
                    cur_id = nothing
                    push!(ranges, (start:i, identifier))
                    start = i+1
                end
            end
        end
    end
    @assert cur_id == nothing
    tail_range = start:length(statements)
    if !isempty(tail_range)
        push!(ranges, (tail_range, nothing))
    end
    return ranges
end

