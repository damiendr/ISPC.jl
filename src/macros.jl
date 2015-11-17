# Macros used to express ISPC concepts in Julia code.

using Base.Meta

function ispc_esc(expr)
    # All ISPC entry points are escaped as @fastmath.
    # This is to prevent Julia from inserting a number of runtime
    # checks into the lowered AST that are either untranslateable
    # or unnecessary because they'll be done by the ISPC compiler.
    # Note: this has NO influence on whether the ISPC code will be
    # compiled with the '--fastmath' option!
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
            # We use the iteration expression as an argument to
            # foreachindex() to pull the necessary variables into
            # the closure. The :meta statement does not do that.
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
Marks a function that may contain ISPC kernels:

@ispc function foo(...)
    ...
    @kernel() do
        ...
    end
    ...
end
"""
macro ispc(func::Expr)
    # Lower the function declaration:
    lowered = expand(func)

    # Extract all ISPC kernels and substitute with calls:
    declarations = make_ispc_main(lowered)

    # extract_ispc() produces one or more top-level declarations:
    ispc_esc(declarations)
end

"""
Defines the entry point to an ISPC kernel.
Must be used inside an @ispc function.

@kernel([compile-opts]) do
    ...
end
"""
macro kernel(args...)
    if length(args) == 2
        lambda, opts = args
    elseif length(args) == 1
        lambda, = args
        opts = :(``)
    end

    # Generate a unique identifier for this kernel.
    # Wrap it in a Val{} so that it can be used for
    # multiple dispatch, and also to prevent it from
    # being turned into a GlobalRef when the AST is
    # lowered.
    identifier = Val{gensym()}

    # tree = lambda.args[2]
    # insert!(tree.args, 1, Expr(:boundscheck, false))
    # push!(tree.args, Expr(:boundscheck, :pop))

    return ispc_esc(quote
        ISPC.ispc_kernel($identifier, $lambda, $opts)
    end)
end


function ispc_kernel(args...)
    # Note: this should not be called, it's just a tag that
    # will be picked up when the kernels are extracted.
    # Provide a helpful error message:
    error("Functions containing ISPC kernels must be marked with @ispc")
end

