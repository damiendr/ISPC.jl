
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
function get_typed_ast(f::Function, argtype::Type{Tuple})
    (ast, ret_type) = Core.Inference.typeinf(f.code, argtype, f.env)
    ast_typed = ccall(:jl_uncompress_ast, Any, (Any,Any), f.code, ast)
end

function get_ast(linfo::LambdaStaticData, ast=linfo.ast)
    if !isa(ast, Expr)
        ast = ccall(:jl_uncompress_ast, Any, (Any,Any), linfo, ast)
    end
    ast::Expr
end

"""
Performs type inference on a lambda AST.
"""
function typeinf(linfo::LambdaStaticData, argtypes)
    (ast, ret_type) = Core.Inference.typeinf_uncached(linfo, Tuple{argtypes...}, Base.svec())
    get_ast(linfo, ast)
end


"""
Sets the types of closed vars to those found in the given Dict.
"""
function set_closure_types!(linfo::LambdaStaticData,
                            vartypes::Dict{Symbol, DataType})
    # Make sure to uncompress the AST:
    linfo.ast = get_ast(linfo)

    # Set the types of outer variables:
    closures = linfo.ast.args[2][2]
    linfo.ast.args[2][2] = Any[Any[symbol, get(vartypes, symbol, typ), kind]
                               for (symbol, typ, kind) in closures]
    linfo
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

function substitute(subst::Function, obj::LambdaStaticData)
    new_obj = deepcopy(obj)
    new_obj.ast = substitute(subst, get_ast(new_obj))
    new_obj
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


function rename_fdef(func)
    funcname = func.args[1].args[1]
    argspec = func.args[1].args[2:end]
    body = func.args[2]
    if isa(funcname, Expr) && funcname.head == :curly
        newsym = gensym(funcname.args[1])
        newname = Expr(:curly, newsym, funcname.args[2:end]...)
    elseif isa(funcname, Symbol)
        newsym = gensym(funcname)
        newname = newsym
    else
        error("Couldn't parse function name: $funcname")
    end
    stageddef = Expr(func.head, Expr(:call, newname, argspec...), body)
#    stagedcall = Expr(:call, newsym, argspec...)
    return newsym, stageddef
end

function get_argname(e::Expr)
    if e.head == :(::)
        return e.args[1]
    elseif e.head == :kw
        return e.args[1]
    end
    error("argname($(sexpr(e))")
end
get_argname(s::Symbol) = s

