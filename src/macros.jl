# Macros used to express ISPC concepts in Julia code.

function ispc_esc(expr)
    # All ISPC entry points are escaped as @fastmath.
    # This is to prevent Julia from inserting a number of runtime
    # checks into the lowered AST that are either untranslateable
    # or unnecessary because they'll be done by the ISPC compiler.
    # Note: this does NOT imply that the ISPC compiler itself will
    # use --fastmath.
    return Base.FastMath.make_fastmath(esc(expr))
end

"""
The ISPC `foreach` construct:

@foreach(array) do index
    ...
end

@foreach(:active) do index
    ...
end
"""
macro foreach(func_expr, array_expr...)
    signature, block = func_expr.args
    # Turn the lambda into a let block:
    foreach_id = gensym(:foreach)
    code = ispc_esc(quote
        $(Expr(:meta, :ispc, foreach_id, :foreach, array_expr))
        let $([:($a = ISPC.foreachindex($i, $s))
                    for (i, (s, a)) in enumerate(
                            zip(array_expr, signature.args))]...)
            $block
        end
        $(Expr(:meta, :ispc, foreach_id))
        nothing # otherwise the :meta above becomes a return value
    end)
end


# That function will not be called, its sole purpose
# is to give a type to foreach indices so that type
# inference can work on them:
@noinline foreachindex(args...) = 1


"""
The ISPC `unmasked` construct:

@unmasked begin
    ...
end
"""
macro unmasked(block)
    unmasked_id = gensym(:unmasked)
    ispc_esc(quote
        $(Expr(:meta, :ispc, unmasked_id, :unmasked))
        $(block)
        $(Expr(:meta, :ispc, unmasked_id))
        nothing # otherwise the :meta above becomes a return value
    end)
end


"""
The ISPC `coherent` construct used to mark a test expression
as likely to evaluate uniformly over a whole gang:

if x > 0 @coherent
    ...
elseif x == 0 @coherent
    ...
else
    ...
end

while x >= 0 @coherent
    ...
end
"""
macro coherent()
    return Expr(:meta, :ispc, :coherent)
end


"""
Replaces ISPC fragments inside a function definition with calls
to the compiled ISPC functions:

@ispc [`compiler options`] function test(arr)
    ...
    @foreach(arr) do idx
        ...
    end
    ...
end
"""
macro ispc(args...)
    if length(args) == 2
        opts, func = args
    else
        func, = args
        opts = ``
    end

    signature, body = func.args

    # Expand, but don't lower the body. I don't know yet how
    # to replicate a functional toolchain from AST to lowered
    # AST to method, so we'll have to generate the Julia-side
    # code from the surface AST for now.
    body = macroexpand(body)
    # println("============== body expanded =================")
    # println(body)
    # println("=============================================")
    # println("============== body expanded-lowered =================")
    # println(expand(func.args[2]))
    # println("=============================================")

    # Parse the function signature:
    def = signature.args[1]
    args = signature.args[2:end]

    # We'll need to define the original function into the top level in
    # order to get a type-inferred AST, but we don't want to shadow the
    # @generated function we'll generate later. Generate a copy of it
    # with an escaped name:
    if isa(def, Symbol)
        name = def
    elseif isa(def, Expr) && def.head == :curly
        name = def.args[1]
    else
        error("Can't parse function name in $def")
    end
    literal_def = substitute(Dict(name => gensym(name)), def)
    literal_call = Expr(:call, literal_def, args...)
    literal_func = Expr(:function, literal_call, body)

    # Build the body of the @generated function:
    quoted_body = Expr(:quote, body)
    gen_body = quote
        # Get the typed AST from the original function:
        typed_ast = code_typed($literal_def, ($(args...),))[1]
        # Extract all ISPC fragments:
        ispc_funcs = ISPC.extract_ispc(typed_ast, $opts)
        # Replace these fragments with ISPC calls:
        body = $quoted_body
        main_body = ISPC.replace_calls(body, ispc_funcs)
        # println("=============== main_body ==================")
        # println(main_body)
        # println("===========================================")
        quote
            ISPC.compile_all()
            $main_body
        end
    end
    gen_func = Expr(:function, signature, gen_body)

    # Generate both functions into the top level:
    esc(quote
        $literal_func
        @generated $gen_func
    end)
end

