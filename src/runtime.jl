
using DataStructures

type ISPCFile
    global_names::Set{ASCIIString}
    func_ids::Vector{Int64}
    compile_opts::Cmd
    compiled::Bool
    error::Bool
end
ISPCFile(opts::Cmd) = ISPCFile(Set{ASCIIString}(), Int64[], opts, false, false)

type ISPCFunction
    idx::Integer

    arg_names::Vector{Any}
    var_types::Dict{Any,Any}
    modified::Set
    ast::Expr

    ispc_name::ASCIIString
    ispc_code::ASCIIString

    file::ISPCFile
end

const ispc_fptr = Array(Ptr{Void},0)
const ispc_funcs = Array(ISPCFunction, 0)
const ispc_files = Dict{AbstractString, ISPCFile}()


const ispc_fragments = Dict()
const ispc_fragment_opts = Dict()


function run_typeinf(linfo, argtypes)
    linfo.ast = get_ast(linfo)
    arg_names = linfo.ast.args[1]
    captured = linfo.ast.args[2][2]
    captured_names = [varinfo[1] for varinfo in captured]
    kernel_argnames = Any[arg_names..., captured_names...]

    # Captured vars become arguments.
    # Unset the flags that applied to closure vars and do not
    # apply any more now that they're function arguments:
    # - 0x1 ('captured') -- does not seem to matter
    # - 0x4 ('assigned by inner function') -- would break type inference
    arg_varinfos = [Any[name, typ, flags & ~0x5]
                    for (name, typ, flags) in captured]
    local_vars = Any[linfo.ast.args[2][1]..., arg_varinfos...]

    # Transform captured vars into parameters:
    new_linfo = deepcopy(linfo)
    new_linfo.ast = get_ast(new_linfo)
    new_linfo.ast.args[1] = kernel_argnames
    new_linfo.ast.args[2][1] = local_vars
    new_linfo.ast.args[2][2] = Any[]

    typeinf(new_linfo, argtypes)
end


@generated function kernel_call{id}(::Type{Val{id}}, args...)
    println("Generating kernel call $id for argument types $args")

    opts = eval(ispc_fragment_opts[id])

    # Collect the modified arguments *before* type inference,
    # because we'll need to remove that information in order
    # for type inference to run correctly:
    linfo = ispc_fragments[id]
    modified = Set()
    for (name, typ, flags) in get_ast(linfo).args[2][2]
        if (flags & 0x4) != 0
            push!(modified, name)
        end
    end

    # println("Running type inference...")
    typed_ast = run_typeinf(linfo, args)
    # print_lambda(strip_lineno(typed_ast))


    # Register the new kernel:
    argexprs = [:(args[$i]) for i in 1:length(args)]
    func_call, func_idx = register_ispc_fragment!(id, argexprs,
                                            modified, typed_ast, opts)

    # Generate the main body:
    body = quote
        $(Expr(:meta, :inline))
#        @inbounds begin
            # First make sure that the function has been compiled:
            if UInt(ISPC.ispc_fptr[$(func_idx)]) == 0
                ISPC.compile_all()
                if UInt(ISPC.ispc_fptr[$(func_idx)]) == 0
                    error("Could not compile ISPC kernel $id")
                end
            end
            # Lazy compilation allows us to compile several fragments
            # with one call to the compiler. This is much faster.

            # Now ccall the function:
            $func_call
#        end
    end
    # println("@generated function kernel_call{$id}(::Type{Val{$id}}, args...)")
    # println(strip_lineno(body))
    body
end


function gen_code(funcs...)
    io = IOBuffer()
    write(io, ispc_includes)
    write(io, "\n\n")
    for func in funcs
        write(io, func.ispc_code)
        write(io, "\n")
    end
    takebuf_string(io)
end


function compile!(file)
    if file.compiled
        return
    end

    # Generate the code for all the functions in the file:
    funcs = [ispc_funcs[idx] for idx in file.func_ids]
    ispc_code = gen_code(funcs...)

#    println("Compiling ISPC file, compile options: ", file.compile_opts)
#    println(ispc_code)

    file.compiled = true
    try
        lib = load_ispc(ispc_code, file.compile_opts)
        # Now set all the function pointers:
        for func in funcs
            fptr = Libdl.dlsym(lib, func.ispc_name)
            ispc_fptr[func.idx] = fptr
            println("Loaded function $(func.ispc_name) at $fptr")
        end
        file.error = false
    catch e
        Base.display_error(e,catch_backtrace())
        file.error = true
    end
end


function compile_all()
    has_error = false

    # Compile all the open files:
    for opts in keys(ispc_files)
        file = ispc_files[opts]
        compile!(file)
        delete!(ispc_files, opts)
        has_error |= file.error
    end

    if has_error
        error("Some files did not compile successfully")
    end
end


function register_ispc_fragment!(id, call_args, modified, ast::Expr, opts::Cmd)

    ast_args = ast.args[1]
    ast_body = ast.args[3]

    # Collate all type information into a single dict.
    var_types = Dict()

    # Local variables:
    local_vars = ast.args[2][1]
    for (name, typ, flags) in local_vars
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

    pass_by_ref = []

    for (func_arg, call_arg) in zip(ast_args, call_args)
        T = var_types[func_arg]
        E = eltype(T)
        if !(isbits(E))
            error("Unsupported element type for $func_arg: $E")
        end
        if issubtype(T, DenseArray)
            if func_arg in modified
                error("Can't reassign arrays inside kernels: $func_arg")
            end

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

        elseif E == T
            # Atomic isbits type:
            if func_arg in modified
                # pass-by-reference
                ref = gensym("ref")
                push!(pass_by_ref, (ref, T, call_arg))
                push!(arg_types, Ref{T})
                push!(arg_values, ref)
            else
                # pass-by-value
                push!(arg_types, T)
                push!(arg_values, call_arg)
            end
            push!(arg_names, func_arg)

        else
            error("Unsupported type for $func_arg: $T")
        end
    end

    # Get an ISPCFile to attach this function to.
    file = get(ispc_files, string(opts), ISPCFile(opts))
    if file.compiled
        # That one was already compiled, start a new one:
        file = ISPCFile(opts)
    end
    ispc_files[string(opts)] = file # use string() because of issue #14030

    # Create the function object:
    func_idx = length(ispc_funcs)+1
    func = ISPCFunction(func_idx, arg_names, var_types, modified,
                        ast_body, "", "", file)

    # Generate the kernel's ISPC code:
    ispc_codegen!(func, "kernel_$id")
    @assert func.ispc_name != ""
    @assert func.ispc_code != ""

    # Wrap all pass-by-reference arguments in Refs before
    # the call:
    wrap_refs = [:($ref = Ref{$T}($val))
                   for (ref, T, val) in pass_by_ref]

    # ... and return the modified value afterwards:
    unwrap_refs = [:($ref.x)
                   for (ref, T, val) in pass_by_ref]
    if isempty(unwrap_refs)
        ret = :(return)
    else
        ret = :(return $((unwrap_refs...,)))
    end

    # Ready! Let's put it all together:
    func_call = quote
        $(wrap_refs...)
        ccall(ISPC.ispc_fptr[$func_idx],
            Void, ($(arg_types...),), $(arg_values...))
        $ret
    end

    push!(ispc_funcs, func)
    push!(ispc_fptr, Ptr{Void}(0))
    push!(file.func_ids, func_idx)

    func_call, func_idx
end

