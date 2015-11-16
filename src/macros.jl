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


macro ispc(func::Expr)
    # Lower the function declaration:
    lowered = expand(func)

    # Extract all ISPC fragments and substitute with calls:
    declarations = make_ispc_main(lowered)

    # extract_ispc() produces one or more top-level declarations:
    sleep(0.1) # FIXME to prevent IJulia from messing up output
    ispc_esc(declarations)
#    eval(declarations)
#    nothing
end


macro kernel(args...)
    if length(args) == 2
        lambda, opts = args
    elseif length(args) == 1
        lambda, = args
        opts = :(``)
    end
    identifier = Val{gensym()}

    # tree = lambda.args[2]
    # insert!(tree.args, 1, Expr(:boundscheck, false))
    # push!(tree.args, Expr(:boundscheck, :pop))

    return ispc_esc(quote
        ISPC.ispc_fragment($identifier, $lambda, $opts)
    end)
    # Note: `ISPC.ispc_fragment()` does not actually exist. It's
    # just a tag that will be picked up when the fragments are
    # extracted.
end
