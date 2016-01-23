# Macros used to express ISPC concepts in Julia code.

using Base.Meta


function ispc_esc(expr)
    # println(expr)
    return esc(expr)
    # All ISPC entry points are escaped as @fastmath.
    # This is to prevent Julia from inserting a number of runtime
    # checks into the lowered AST that are either untranslateable
    # or unnecessary because they'll be done by the ISPC compiler.
    # Note: this has NO influence on whether the ISPC code will be
    # compiled with the '--fastmath' option!
    # return Base.FastMath.make_fastmath(esc(expr))
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
macro ispc(fdef::Expr)
    
    # What kind of function definition do we have?
    func = macroexpand(fdef)
    
    if func.head == :function
        # That's a plain function
        return esc(ISPC.make_ispc_main(expand(func)))

    elseif func.head == :stagedfunction
        # That's a @generated function.

        # This case is a bit harder to handle. We need to call the
        # original @generated function in order to know the actual
        # function body where we want to extract the kernels, and
        # to do that we need to know the argument types.

        # So we'll have to declare the original @generated function, 
        # declare a new @generated function to catch argument types,
        # obtain the actual body from the original function, and run
        # the extraction routine in the new @generated function.

        # First let's escape the name of the @generated block:
        stagedname, stageddef = rename_fdef(func)
        
        # Now build another @generated function to catch the argument types:
        funcspec = func.args[1]
        argspec = func.args[1].args[2:end]
        argnames = map(get_argname, argspec)
        
        ispcbody = quote
            # println($stagedname)
            # println(methods($stagedname))
            # m = methods($stagedname).defs
            # println(m)
            # println(methods($stagedname).defs.func)
            # println(methods($stagedname).defs.func.env)
            # sleep(0.5)
            # ISPC.print_lambda(methods($stagedname).defs.func.code.ast)
            # sleep(0.5)

            flowered = code_typed($stagedname, Tuple{$(argnames...)})
            body = flowered[1]

            # # Get the @generated body for the argument types we have:
            # body = methods($stagedname).defs.func($(argnames...))
            # println(body)
            
            # # Create a plain function with the same signature and the
            # # body we just got, otherwise the code won't be lowered 
            # # right because the arguments aren't there:
            # funcspec = $(Expr(:quote, funcspec))
            
            # # Run it through make_ispc_main():
            # func = Expr(:function, funcspec, body)
            # println(func)
            ispcmain = ISPC.make_ispc_main(body)
            # println(ispcmain)

            # Return the *body* of the transformed function:
            body = ispcmain.args[3]
            quote
                $(body.args...) # splatting avoids "misplaced return" error
            end
        end
        ispcfunc = Expr(:stagedfunction, funcspec, ispcbody)
        # The @ispc macro returns both the original, escaped @generated function
        # and the ispc @generated function:
        esc(quote
            import ISPC
            $stageddef
            @Base.__doc__($ispcfunc)
        end)
    end
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

    # Kernels don't return anything:
    tree = lambda.args[2]
    push!(tree.args, :nothing)

    # insert!(tree.args, 1, Expr(:boundscheck, false))
    # push!(tree.args, Expr(:boundscheck, :pop))

    return ispc_esc(quote
        ISPC.ispc_kernel($identifier, $lambda, $opts)
    end)
end


@noinline function ispc_kernel(args...)
    # Note: this should not be called, it's just a tag that
    # will be picked up when the kernels are extracted.
    # Provide a helpful error message:
    error("Functions containing ISPC kernels must be marked with @ispc")
end

