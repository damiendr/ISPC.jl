
type ISPCFunction
    idx::Integer
    arg_names::Vector{Any}
    var_types::Dict{Any,Any}
    ast::Expr
    ispc_name::ASCIIString
    ispc_code::ASCIIString
    ispc_opts::Cmd
    compile_failed::Bool
end

const ispc_fptr = Array(Ptr{Void},0)
const ispc_funcs = Array(ISPCFunction, 0)


const ispc_fragments = Dict()
const ispc_fragment_opts = Dict()


function run_typeinf(linfo, argtypes)
    arg_names = linfo.ast.args[1]
    captured = linfo.ast.args[2][2]
    captured_names = [varinfo[1] for varinfo in captured]
    kernel_argnames = Any[arg_names..., captured_names...]
    # local_vars = Any[Any[name, typ, 2] for (name, typ) in zip(kernel_argnames, args)]

    # Captured vars become arguments:
    # Unset the 'captured' flag 0x1 -- doesn't seem to matter, but...
    arg_varinfos = [Any[name, typ, flags & ~0x1]
                    for (name, typ, flags) in captured]
    local_vars = Any[linfo.ast.args[2][1]..., arg_varinfos...]

    # Transform captured vars into parameters:
    new_ast = deepcopy(linfo)
    new_ast.ast.args[1] = kernel_argnames
    new_ast.ast.args[2][1] = local_vars
    new_ast.ast.args[2][2] = Any[]

    typeinf(new_ast, argtypes)
end


@generated function call_fragment{id}(::Type{Val{id}}, args...)
    println("Generating fragment $id for argument types $args")

    opts = eval(ispc_fragment_opts[id])
    println("Compile options: $opts")

    # func = register_ispc_fragment!(ispc_fragments, opts)

    println("Running type inference...")
    linfo = ispc_fragments[id]
    typed_ast = run_typeinf(linfo, args)
    print_lambda(strip_lineno(typed_ast))

    # Register the new fragment:
    argexprs = [:(args[$i]) for i in 1:length(args)]
    func_call, func_idx = register_ispc_fragment!(argexprs, typed_ast, opts)

    # Generate the main body:
    body = quote
        $(Expr(:meta, :inline))
        @inbounds begin
            # First make sure that the function has been compiled:
            if UInt(ISPC.ispc_fptr[$(func_idx)]) == 0
                ISPC.compile_all()
                if UInt(ISPC.ispc_fptr[$(func_idx)]) == 0
                    error("Could not compile ISPC fragment $id")
                end
            end
            # Lazy compilation allows us to compile several fragments
            # with one call to the compiler. This is much faster.

            # Now ccall the function:
            $func_call
        end
    end
    println("@generated function call_fragment{$id}(args...)")
    println(strip_lineno(body))
    body
end


using DataStructures

function compile_functions(funcs, opts=``)
    code_io = IOBuffer()
    func_symbols = []
    write(code_io, ispc_includes)
    for func in funcs
        write(code_io, func.ispc_code)
        push!(func_symbols, (func.idx, func.ispc_name))
    end

    ispc_code = takebuf_string(code_io)

    # println("Compiling ISPC fragments:")
    # println(ispc_code)
#    println(ispc_native(ispc_code, opts))
    
    try
        lib = load_ispc(ispc_code, opts)
        for (idx, ispc_name) in func_symbols
            fptr = Libdl.dlsym(lib, ispc_name)
            ispc_fptr[idx] = fptr
        end
    catch e
        for func in funcs
            func.compile_failed = true
        end
        rethrow(e)
    end
end


function compile_all()
    # group the functions by compiler options:
    funcs_by_opts = MultiDict()
    for func in ispc_funcs
        if UInt(ispc_fptr[func.idx]) == 0 && !func.compile_failed
            push!(funcs_by_opts, func.ispc_opts => func)
        end
    end

    # compile each group of functions separately:
    for (opts, funcs) in funcs_by_opts
        compile_functions(funcs, opts)
    end
end


function register_ispc_fragment!(call_args, ast::Expr, opts::Cmd)

    ast_args = ast.args[1]
    ast_body = ast.args[3]

    # Collate all type information into a single dict.
    var_types = Dict()

    # Local variables:
    local_vars = ast.args[2][1]
    for (name, typ, flag) in local_vars
        var_types[name] = typ
    end

    # SSA vars:
    ssa_vars = ast.args[2][3]
    for (i, typ) in enumerate(ssa_vars)
        var_types[GenSym(i-1)] = typ # remember GenSym() starts at 0
    end

    arg_types = [] # ccall argument types
    arg_values = [] # ccall argument values
    arg_names = [] # function argument names

    for (func_arg, call_arg) in zip(ast_args, call_args)
        T = var_types[func_arg]
        E = eltype(T)
        if !(isbits(E))
            error("Unsupported non-isbits() type: $E")
        end
        if E != T
            # We have an array type, pass it as a ref:
            push!(arg_types, Ref{E})
            push!(arg_values, call_arg)
            argument = Any[func_arg]

            # Also pass the array size(s):
            for d in 1:ndims(T)
                push!(arg_types, Int64)
                push!(arg_values, :(size($call_arg, $d)))
                size_var = Symbol("$(func_arg)__len__$(d)")
                var_types[size_var] = Int64
                push!(argument, size_var)
            end
            push!(arg_names, argument)
        else
            # Atomic isbits type:
            push!(arg_types, T)
            push!(arg_values, call_arg)
            push!(arg_names, func_arg)
        end
    end

    func_idx = length(ispc_funcs)+1
    func = ISPCFunction(func_idx, arg_names, var_types, ast_body, "", "", opts, false)

    ispc_codegen!(func)

    println(func.ispc_code)

    push!(ispc_funcs, func)
    push!(ispc_fptr, Ptr{Void}(0))

    func_call = quote
        ccall(ISPC.ispc_fptr[$func_idx],
            Void, ($(arg_types...),), $(arg_values...))
    end
    func_call, func_idx
end


