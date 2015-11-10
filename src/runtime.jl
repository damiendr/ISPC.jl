
type ISPCFunction
    idx::Integer
    signature::Vector{Any}
    var_types::Dict{Any,DataType}
    fragment::Vector{Any}
end

const ispc_fptr = Array(Ptr{Void},0)
const ispc_funcs = Array(Tuple{ISPCFunction,Cmd}, 0)


using DataStructures

function compile_functions(funcs, opts=``)
    code_io = IOBuffer()
    func_symbols = []
    for func in funcs
        ispc_name = ispc_codegen(code_io, func)
        push!(func_symbols, (func.idx, ispc_name))
    end

    ispc_code = takebuf_string(code_io)
    println(ispc_code)
    
    lib = load_ispc(ispc_code, opts)
    for (idx, ispc_name) in func_symbols
        fptr = Libdl.dlsym(lib, ispc_name)
        ispc_fptr[idx] = fptr
    end
end


function compile_all()
    if length(ispc_funcs) > length(ispc_fptr)
        new_funcs = ispc_funcs[length(ispc_fptr)+1:end]
        resize!(ispc_fptr, length(ispc_funcs))

        # group the functions by compiler options:
        funcs_by_opts = MultiDict()
        for (func, opts) in new_funcs
            push!(funcs_by_opts, opts => func)
        end

        # compile each group of functions separately:
        for (opts, funcs) in funcs_by_opts
            compile_functions(funcs, opts)
        end
    end
    @assert all([isdefined(ispc_fptr, idx) for idx in eachindex(ispc_fptr)])
end


function new_ispc_func(params, var_types, fragment, opts)
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

    func_idx = length(ispc_funcs)+1
    func = ISPCFunction(func_idx, signature, var_types, fragment)
    push!(ispc_funcs, (func, opts))
    quote
        ccall(ISPC.ispc_fptr[$func_idx],
            Void, ($(argtypes...),), $(argexprs...))
    end
end


