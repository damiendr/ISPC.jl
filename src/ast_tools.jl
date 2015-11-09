
"""
Pretty-prints a `:lambda` expression.
"""
function print_lambda(expr)
    @assert expr.head == :lambda
    arg_symbols = expr.args[1]
    local_vars = expr.args[2][1]
    env_vars = expr.args[2][2]
    gen_syms = expr.args[2][3]
    param_symbols = expr.args[2][4]
    tree = expr.args[3]
    
    println("Lambda function:")
    println("\t arguments: $arg_symbols")
    println("\t local variables: ")
    println(join(map(o -> "\t\t$((o...,))", local_vars), "\n"))
    println("\t closure variables: ")
    println(join(map(o -> "\t\t$((o...,))", env_vars), "\n"))
    println("\t SSA types: $gen_syms")
    println("\t parameters: $param_symbols")
    println(strip_box(strip_lineno(tree)))
end

"""
Returns a typed AST for function `f`.
"""
function typed_ast(f::Function, argtype::Type{Tuple})
    (ast, ret_type) = Core.Inference.typeinf(f.code, argtype, f.env)
    ast_typed = ccall(:jl_uncompress_ast, Any, (Any,Any), f.code, ast)
end


"""
Calls `func()` on every node in `expr`.
"""
function walk(func, expr)
    func(expr)
    if isa(expr, Expr)
        func(expr.head)
        for o in expr.args
            walk(func, o)
        end
    end
end


"""
Traverses `expr` and replaces nodes according to the
substitutions in dict/function `subst`.
""" 

function substitute(subst::Dict, obj::Any)
    get(subst, obj, obj)
end

function substitute(subst::Dict, expr::Expr)
    if haskey(subst, expr)
        return subst[expr]
    else
        head = substitute(subst, expr.head)
        args = [substitute(subst, a) for a in expr.args]
        return Expr(head, args...)
    end
end

function substitute(subst::Function, obj::Any)
    result = subst(obj)
    if result != nothing
        return result
    else
        return obj
    end
end

function substitute(subst::Function, expr::Expr)
    result = subst(expr)
    if result != nothing
        return result
    else
        head = substitute(subst, expr.head)
        args = [substitute(subst, a) for a in expr.args]
        return Expr(head, args...)
    end
end

function sexpr(obj)
    io = IOBuffer()
    Base.Meta.show_sexpr(io, obj)
    takebuf_string(io)
end

