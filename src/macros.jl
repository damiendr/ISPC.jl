# Macros used to express ISPC concepts in Julia code.

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
    code = esc(quote
        $(Expr(:meta, :ispc, foreach_id, :foreach, array_expr))
        let $([:($a = ISPC.foreachindex($s)) for (s, a) in zip(array_expr, signature.args)]...)
            $block
        end
        $(Expr(:meta, :ispc, foreach_id))
        nothing # otherwise the :meta above becomes a return value
    end)
end


# That function will not be called, its sole purpose
# is to give a type to foreach indices so that type
# inference can work on them:
@noinline foreachindex(arr) = 1


"""
The ISPC `unmasked` construct:

@unmasked begin
    ...
end
"""
macro unmasked(block)
    unmasked_id = gensym(:unmasked)
    esc(quote
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
to the compiled ISPC functions.
"""
macro ispc_function(func)
    signature, body = func.args

    # Expand, but don't lower the body. I don't know yet how
    # to replicate a functional toolchain from AST to lowered
    # AST to method, so we'll have to generate the Julia-side
    # code from the surface AST for now.
    body = macroexpand(body)
    # println("============== body expanded =================")
    # println(body)
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
    literal_def = substitute(def, Dict(name => gensym(name)))
    literal_call = Expr(:call, literal_def, args...)
    literal_func = Expr(:function, literal_call, body)

    # Build the body of the @generated function:
    quoted_body = Expr(:quote, body)
    gen_body = quote
        # Get the typed AST from the original function:
        typed_ast = code_typed($literal_def, ($(args...),))[1].args[3]
        # Extract all ISPC fragments:
        ispc_funcs = ISPC.extract_ispc(typed_ast)
        # Replace these fragments with ISPC calls:
        body = $quoted_body
        gen_body = ISPC.replace_calls(body, ispc_funcs)
        # println("=============== gen_body ==================")
        # println(gen_body)
        # println("===========================================")
        gen_body
    end
    gen_func = Expr(:function, signature, gen_body)

    # Generate both functions into the top level:
    esc(quote
        $literal_func
        @generated $gen_func
    end)
end

