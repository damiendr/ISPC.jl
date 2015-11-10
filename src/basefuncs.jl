

function fastmath_variants(pairs...)
    result = []
    for (func_name, func_def) in pairs
        push!(result, eval(parse(func_name)) => func_def)
        try
            fast = eval(parse("$(func_name)_fast"))
            push!(result, fast => func_def)
        end
    end
    result
end

function openlibm_variants(pairs...)
    result = []
    for (func_name, func_def) in pairs
        push!(result, ("$(func_name)", "libopenlibm") => func_def)
        push!(result, ("$(func_name)f", "libopenlibm") => func_def)
    end
    result
end    

basefuncs = Dict(

    Base.arrayref => (array, I...) -> "$array[$(I...)]",
    Base.arrayset => (array, value, I...) -> "$array[$(I...)] = $value;",

    Base.box => (typ, value) -> "$value",
    Base.sitofp => (typ, value) -> "(($typ)$value)",
    Base.fptosi => (typ, value) -> "(($typ)$value)",
    Base.checked_trunc_sint => (typ, value) -> "(($typ)$value)",
    Base.fpext => (typ, value) -> "(($typ)$value)",
    Base.fptrunc => (typ, value) -> "(($typ)$value)",

    (<) => (a, b) -> "($a < $b)",
    (!) => (a) -> "(!$a)",

    fastmath_variants(
        "Base.not_int" => (a) -> "(!$a)",
        "Base.or_int" => (a,b) -> "($a || $b)",
        "Base.and_int" => (a,b) -> "($a && $b)",
        "Base.slt_int" => (a,b) -> "($a < $b)",
        "Base.sle_int" => (a,b) -> "($a <= $b)",
        "Base.add_int" => (a,b) -> "($a + $b)",
        "Base.sub_int" => (a,b) -> "($a - $b)",
        "Base.mul_int" => (a,b) -> "($a * $b)",

        "Base.eq_float" => (a,b) -> "($a == $b)",
        "Base.ne_float" => (a,b) -> "($a != $b)",
        "Base.lt_float" => (a,b) -> "($a < $b)",
        "Base.le_float" => (a,b) -> "($a <= $b)",
        "Base.neg_float" => (a) -> "(-$a)",
        "Base.abs_float" => (a) -> "abs($a)",
        "Base.add_float" => (a,b) -> "($a + $b)",
        "Base.sub_float" => (a,b) -> "($a - $b)",
        "Base.mul_float" => (a,b) -> "($a * $b)",
        "Base.div_float" => (a,b) -> "($a / $b)",

        "Base.sqrt_llvm" => (a) -> "sqrt($a)",
        "Base.sqrt" => (a) -> "sqrt($a)",
    )...,

    openlibm_variants(
        "sin" => (a) -> "sin($a)",
        "cos" => (a) -> "cos($a)",
        "tan" => (a) -> "tan($a)",
        "asin" => (a) -> "sin($a)",
        "acos" => (a) -> "cos($a)",
        "atan" => (a) -> "tan($a)",
        "exp" => (a) -> "exp($a)",
        "log" => (a) -> "log($a)",
        "pow" => (a,b) -> "pow($a,$b)",
    )...
)

