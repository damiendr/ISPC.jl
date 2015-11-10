

function fast_variants(pairs...)
    result = []
    for pair in pairs
        func_name = pair.first
        func_def = pair.second
        push!(result, eval(parse(func_name)) => func_def)
        try
            fast = eval(parse(func_name * "_fast"))
            push!(result, fast => func_def)
        end
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

    (<) => (val1, val2) -> "($val1 < $val2)",
    (!) => (val1) -> "(!$val1)",

    fast_variants(
        "Base.not_int" => (val1) -> "(!$val1)",
        "Base.or_int" => (val1, val2) -> "($val1 || $val2)",
        "Base.and_int" => (val1, val2) -> "($val1 && $val2)",
        "Base.slt_int" => (val1, val2) -> "($val1 < $val2)",
        "Base.sle_int" => (val1, val2) -> "($val1 <= $val2)",
        "Base.add_int" => (val1, val2) -> "($val1 + $val2)",
        "Base.sub_int" => (val1, val2) -> "($val1 - $val2)",
        "Base.mul_int" => (val1, val2) -> "($val1 * $val2)",

        "Base.eq_float" => (val1, val2) -> "($val1 == $val2)",
        "Base.ne_float" => (val1, val2) -> "($val1 != $val2)",
        "Base.lt_float" => (val1, val2) -> "($val1 < $val2)",
        "Base.le_float" => (val1, val2) -> "($val1 <= $val2)",
        "Base.neg_float" => (val) -> "(-$val)",
        "Base.abs_float" => (val) -> "abs($val)",
        "Base.add_float" => (val1, val2) -> "($val1 + $val2)",
        "Base.sub_float" => (val1, val2) -> "($val1 - $val2)",
        "Base.mul_float" => (val1, val2) -> "($val1 * $val2)",
        "Base.div_float" => (val1, val2) -> "($val1 / $val2)",

        "Base.sqrt_llvm" => (val) -> "sqrt($val)",
        "Base.sqrt" => (val) -> "sqrt($val)",
        "Base.log" => (val) -> "log($val)",
    )...,
)

