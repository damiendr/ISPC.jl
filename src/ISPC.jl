module ISPC

include("compile.jl")
export load_ispc, ispc_native, ispc_llvm

include("codegen.jl")

include("transform.jl")
export raise_ast, strip_lineno, strip_box


@noinline foreachindex(arr) = 1


macro foreach(func_expr, array_expr...)
    signature, block = func_expr.args
    # Turn the lambda into a let block:
    foreach_id = gensym(:foreach)
    code = esc(quote
        $(Expr(:meta, :ispc, foreach_id, :foreach, array_expr))
        let $([:($a = foreachindex($s)) for (s, a) in zip(array_expr, signature.args)]...)
            $block
        end
        $(Expr(:meta, :ispc, foreach_id))
    end)
end
export @foreach


macro unmasked(block)
    unmasked_id = gensym(:unmasked)
    esc(quote
        $(Expr(:meta, :ispc, unmasked_id, :unmasked))
        $(block)
        $(Expr(:meta, :ispc, unmasked_id))
    end)
end
export @unmasked


macro coherent()
    return Expr(:meta, :ispc, :coherent)
end
export @coherent


macro ispc_function(ret_type, expr)
    defcall = expr.args[1]
    codeblocks = filter((node) -> isa(node, ASCIIString), expr.args[2].args)
    codestring = join(codeblocks, "\n")
    println(codestring)

    func_name = defcall.args[1]
    if !isa(func_name, Symbol)
        func_name = func_name.args[1]
    end
    func_args = map(defcall.args[2:end]) do arg
        if isa(arg, Symbol)
            return arg
        elseif arg.head == :(::)
            return arg.args[1]
        end
    end
    code = quote
        @generated function $func_name($(func_args...))
            ispc_body = $codestring
            func_name = $("$func_name")
            arg_types = ($(func_args...),)
            arg_symbols = $func_args
            ispc_func = gen_ispc_func(func_name, $ret_type, ispc_body, arg_types, arg_symbols...)
            lib_name = gen_ispc_lib(ispc_func)
            return gen_ispc_call((func_name, lib_name), $ret_type, arg_types, arg_symbols...)
        end
    end
    println(code)
end


end # module
