
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


"""
Registers a new ISPC fragment.
"""
function add_ispc_function!(func_calls, identifier, func_def)
    func_call = new_ispc_func(func_def...)
    func_calls[identifier] = func_call
end


"""
Extracts ISPC fragments in `body` and return a dict of ISPC functions.
"""
function extract_ispc(typed_ast, opts=``)
    var_types = Dict()
    local_vars = typed_ast.args[2][1]
    env_vars = typed_ast.args[2][2]
    gen_syms = typed_ast.args[2][3]
    # Collate all type information into a single dict.
    # (1) Local and closure vars:
    for (symbol, typ, bits) in (local_vars..., env_vars...)
        var_types[symbol] = typ
    end
    # (2) GenSym SSA targets:
    for (i, typ) in enumerate(gen_syms)
        var_types[GenSym(i-1)] = typ # remember GenSym() starts at 0
    end

    body = typed_ast.args[3]
    statements = body.args

    # Extract all top-level spans of ISPC code and replace them with
    # calls to generated ISPC functions:
    func_calls = Dict()
    for (range, identifier) in ispc_ranges(statements)
        if identifier == nothing; continue; end
        # Find out which outer variables are used within that fragment,
        # they will become arguments to the ISPC function:
        params = extract_params(statements, range)
        # Turn the fragment into an ISPC function:
        fragment = statements[range]
        add_ispc_function!(func_calls, identifier, (params, var_types, fragment, opts))
    end
    func_calls
end

"""
Replaces ISPC fragments in `body` with calls to the corresponding
ISPC call in `func_calls`.
"""
function replace_calls(body, func_calls)
    if isa(body, Expr)
        statements = copy(body.args)
        for (range, identifier) in ispc_ranges(body.args)
            if identifier == nothing; continue; end
            func_call = func_calls[identifier]
            statements[range] = nothing
            statements[range.start] = func_call
        end
        filter!(o -> o != nothing, statements)
        return Expr(body.head, [replace_calls(o, func_calls) for o in statements]...)
    else
        return body
    end
end