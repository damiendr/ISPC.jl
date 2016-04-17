
using Base.Meta

"""
Finds ISPC kernels in `func`, extracts them, and
replaces them with calls to the compiled ISPC functions.
"""
function make_ispc_main(func::Expr)
    substitute(func) do expr
        if isa(expr, Expr) && expr.head == :call
            func = expr.args[1]
            if func == GlobalRef(ISPC, :ispc_kernel)
                identifier, lambda, options = expr.args[2:end]
                
                ast = get_ast(lambda)
                arg_names = ast.args[1]
                captured = ast.args[2][2]
                body = ast.args[3]

                # Find out which arguments are modified by the
                # kernel call:
                modified = []
                kernel_argnames = Any[arg_names...]
                for (name, typ, flags) in captured
                    push!(kernel_argnames, name)
                    if (flags & 0x4) != 0
                        # Fortunately, this is already set after lowering,
                        # before type inference.
                        push!(modified, name)
                    end
                end

                println("Extracted kernel $identifier$((kernel_argnames...)) -- modified: $((modified...))")
                # println(strip_lineno(body))

                # identifier is a Val{symbol}, unbox the symbol:
                id = identifier.parameters[1]

                # Register the fragment:
                ISPC.ispc_fragments[id] = lambda
                ISPC.ispc_fragment_opts[id] = options

                # Generate the corresponding call.
                 # Don't forget we're dealing with lowered ASTs, expand:
                kernel_call = expand(:(ISPC.kernel_call($identifier,
                                             $((kernel_argnames...,)))))

                # Don't forget to handle any returned values:
                if isempty(modified)
                    return kernel_call
                else
                    return :(
                        $(modified...) = $kernel_call
                    )
                end
            end
        end
        nothing # not matched, don't substitute
    end
end



"""
Finds ranges of statements inside opening and closing ISPC tags.
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

