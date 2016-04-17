

function fastmath_variants(pairs...)
    result = []
    for (func_name, func_def) in pairs
        push!(result, eval(parse(func_name)) => func_def)
        try # not all functions have a fast variant
            fast = eval(parse("$(func_name)_fast"))
            push!(result, fast => func_def)
        end
    end
    result
end

function openlibm_variants(pairs...)
    result = []
    for (func_name, func_def) in pairs
        push!(result, (Symbol("$(func_name)"), "libopenlibm") => func_def)
        push!(result, (Symbol("$(func_name)f"), "libopenlibm") => func_def)
    end
    result
end


"""
Helper to get around the extraneous checks that Julia performs
when multiplying bools to floats.
"""
@noinline function case(test::Bool, a::Number)
    Base.select_value(test, a, zero(a))
end


basefuncs = Dict(

    Base.FastMath.box => (typ, value) -> "$value",
    Base.box => (typ, value) -> "$value",
    Base.sitofp => (typ, value) -> "(($typ)$value)",
    Base.fptosi => (typ, value) -> "(($typ)$value)",
    Base.uitofp => (typ, value) -> "(($typ)$value)",
    Base.fptoui => (typ, value) -> "(($typ)$value)",
    Base.checked_trunc_sint => (typ, value) -> "(($typ)$value)",
    Base.fpext => (typ, value) -> "(($typ)$value)",
    Base.fptrunc => (typ, value) -> "(($typ)$value)",
    Base.sext_int => (typ, value) -> "(($typ)$value)",
    Base.convert => (typ, value) -> "(($typ)$value)",
    Base.typeassert => (value, typ) -> "$value",

    Base.round => (value) -> "round($value)",
    Base.floor => (value) -> "floor($value)",
    Base.ceil => (value) -> "ceil($value)",
    Base.clamp => (val,min,max) -> "clamp($val,$min,$max)",

    ISPC.case => (test, a) -> "((float)$test * $a)",
    Core.Intrinsics.select_value => (test, a, b) -> "($test ? $a : $b)",

    (===) => (a, b) -> "($a == $b)",

    (*) => (a, b) -> "($a * $b)",
    (-) => (a, b) -> "($a - $b)",
    (+) => (a, b) -> "($a + $b)",
    (<) => (a, b) -> "($a < $b)",
    (!) => (a) -> "(!$a)",

    Base.FastMath.sub_fast => (a,b) -> "($a - $b)",
    Base.FastMath.add_fast => (a,b) -> "($a + $b)",
    Base.FastMath.mul_fast => (a,b) -> "($a * $b)",
    Base.FastMath.div_fast => (a,b) -> "($a / $b)",

    fastmath_variants(
        "Base.min" => (a) -> "min($a)",
        "Base.max" => (a) -> "max($a)",

        "Base.not_int" => (a) -> "__not($a)",
        "Base.or_int" => (a,b) -> "($a || $b)",
        "Base.xor_int" => (a,b) -> "($a ^ $b)",
        "Base.and_int" => (a,b) -> "($a && $b)",

        "Base.add_int" => (a,b) -> "($a + $b)",
        "Base.sub_int" => (a,b) -> "($a - $b)",
        "Base.mul_int" => (a,b) -> "($a * $b)",
        "Base.sdiv_int" => (a,b) -> "($a / $b)",
        "Base.udiv_int" => (a,b) -> "($a / $b)",

        "Base.eq_int" => (a,b) -> "($a == $b)",
        "Base.slt_int" => (a,b) -> "($a < $b)",
        "Base.sle_int" => (a,b) -> "($a <= $b)",
        "Base.ult_int" => (a,b) -> "($a < $b)",
        "Base.ule_int" => (a,b) -> "($a <= $b)",

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

        "Base.powi_llvm" => (a,b) -> "pow($a, $b)",
        "Base.floor_llvm" => (a) -> "floor($a)",
        "Base.sqrt_llvm" => (a) -> "sqrt($a)",
        "Base.sqrt" => (a) -> "sqrt($a)",
        "Base.inv" => (a) -> "rcp($a)",

        "Base.nan_dom_err" => (a, b) -> "$a",

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
        "sqrt" => (a) -> "sqrt($a)",
    )...
)

