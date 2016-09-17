
# Towards a cleaner implementation in Julia 0.6+

The main problem: type inference is changing, function types have been introduced, and the current system no longer works in 0.6.

Here we look at a number of possible choices.

## Call-site translation

A clean implementation of call-site translation becomes possible with the new Function types:

```julia
function kernel(a, b)
    ...
end

# A simple call:
result = translated_call(kernel, x, y)

# And with @syntactic sugar:
results = @translated kernel(x, y)
```

This also works with anonymous inner functions:

```julia
function test(a, out)
    translated_call() do 
        out[1] = f(a)
    end
    out
end
```

Translation works as follows:

```julia
# Naive wrapper without caching
function translated_call(func::Function, args...)
    argtypes = map(typeof, args)
    ast = code_typed(func, argtypes)
    newfunc = gen_translated(ast, argtypes)
    newfunc(args...)
end
```

For good performance one must find a way to cache the translated function efficiently. One way is to use a @generated wrapper:

```julia
julia_funcs = []
translated_funcs = []

# Wrapper with lazy compilation
@generated function translated_call(f::Function, args...)
    push!(julia_funcs, (f, args))
    idx = length(julia_funcs)
    quote
        ensure_all_compiled()
        translated_funcs[$idx](f, args...)
        # or something similar eg. ccall
    end
end
```

As long as the trivial path for ensure_all_compiled() is fast, this should give good performance.

Note that we pass the original Function f to the translated function. In the case of anonymous inner functions this object contains the closure. It is up to the translator to generate a translated function that knows how to use that info.

With the @generated wrapper we need to modify the way we obtain the typed ast, because we're getting the function type, NOT the function instance. See https://github.com/JuliaLang/julia/pull/16000 (RIP). So far I do not know of a way to instanciate a Function type from Julia.

Here's a solution that hooks into the type inference machinery:

```julia
function ensure_all_compiled()
    for (ft, argtypes) in julia_funcs[length(translated_funcs)+1:length(julia_funcs)]
        # Extract a type-inferred AST:
        method = Base._methods_by_ftype(Tuple{ft,argtypes...}, -1)[1]
        codeinfo = Core.Inference.typeinf(method[3], method[1], method[2], true)[1]
        ast = Base.uncompressed_ast(method[3], codeinfo)
        
        # Now we have everything we need to translate the function:
        # - typed call signature
        # - typed AST
        newfunc = gen_translated(ast, argtypes)
        push!(translated_funcs, newfunc)
    end
end
```

## Function-level translation

With the new function types, function-level translation becomes just a special case of callsite translation where a wrapper calls the original function.

```julia
macro translated(func)
    orig_func = rename(func)
    wrapper = gen_wrapper(func, orig_func)
    quote
        declare orig_func...
        declare wrapper...
    end
end
```
