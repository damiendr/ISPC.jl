
const ispc_fptr = Array(Ptr{Void},0)
const ispc_funcs = Array(Any, 0)


function compile_functions(range)
    code_io = IOBuffer()
    new_funcs = []
    for idx in range
        func = ispc_funcs[idx]
        name = ispc_codegen(code_io, func, idx)
        push!(new_funcs, (idx, name))
    end

    ispc_code = takebuf_string(code_io)

    println(ispc_code)
    
    lib = load_ispc(ispc_code)
    for (idx, name) in new_funcs
        fptr = Libdl.dlsym(lib, name)
        ispc_fptr[idx] = fptr # note resize!() earlier
    end
end


function compile_all()
    if length(ispc_funcs) > length(ispc_fptr)
        new_range = length(ispc_fptr)+1:length(ispc_funcs)
        resize!(ispc_fptr, length(ispc_funcs))
        compile_functions(new_range)
    end
end


function new_ispc_func(params, var_types, fragment)
    argtypes = [] # ccall argument types
    argexprs = [] # ccall argument values
    signature = []

    for (sym, T) in params
        E = eltype(T)
        if !(isbits(E))
            error("Unsupported non-isbits() type: $E")
        end
        if E != T
            # We have an array type, pass it as a pointer:
            push!(argtypes, Ptr{E})
            push!(argexprs, sym)
            argument = [sym]
            # Also pass the array size(s):
            for d in ndims(T)
                push!(argtypes, Int64)
                push!(argexprs, :(size($sym, $d)))
                size_var = Symbol("$(sym)__len__$(d)")
                var_types[size_var] = Int64
                push!(argument, size_var)
            end
            push!(signature, argument)
        else
            push!(argtypes, E)
            push!(argexprs, sym)
            push!(signature, sym)
        end
    end

    push!(ispc_funcs, (signature, var_types, fragment))
    func_idx = length(ispc_funcs)
    quote
        ccall(ISPC.ispc_fptr[$func_idx],
            Void, ($(argtypes...),), $(argexprs...))
    end
end


