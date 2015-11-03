
const ispc_types = Dict(
    Void => "void",
    Bool => "bool",
    Float32 => "float",
    Float64 => "double",
    Int64 => "int64",
    UInt64 => "unsigned int64",
    Int32 => "int32",
    UInt32 => "unsigned int32",
    Int16 => "int16",
    UInt16 => "unsigned int16",
    Int8 => "int8",
    UInt8 => "unsigned int8",
)


function gen_ispc_call(call_id, ret_type, arg_types, arg_symbols...)
    quote
        ccall(call_id, ret_type, arg_types, arg_symbols...)
    end
end

function to_c_type(T, name="")
    if issubtype(T, AbstractArray)
        arg = to_c_type(eltype(T), name)
        return "$arg[]"
    else
        ctype = ispc_types[T]
        return strip("$ctype $name")
    end
end

function gen_ispc_func(name, ret_type, body, arg_types, arg_symbols...)
    c_ret_type = to_c_type(ret_type)
    c_arg_types = map(to_c_type, arg_types, arg_symbols)
    c_args = ["uniform $arg" for arg in c_arg_types]
    c_args_decls = join(c_args, ", ")
    return """
    export $c_ret_type $name($c_args_decls) {
    $body
    }
    """
end

