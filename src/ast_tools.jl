
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
