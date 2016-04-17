# ISPC code generation.


@enum VARIABILITY unbound=1 uniform=2 variable=3
const qualifiers = Dict(unbound=>"", uniform=>"uniform", variable=>"variable")

immutable EmitContext
    header::Vector{Any}
    var_types::Dict
    modified::Set
    globals::Set
    cnames::Dict
    sizes::Dict
    declared::Set
    types::Dict
    variability::VARIABILITY
    loc_info::AbstractString
end

function nested_contex(ctx::EmitContext, variability::VARIABILITY)
    EmitContext(ctx.header, ctx.var_types, ctx.modified,
                ctx.globals, ctx.cnames, ctx.sizes,
                ctx.declared, ctx.types, variability, ctx.loc_info)
end

function with_loc_info(ctx::EmitContext, info::AbstractString)
    EmitContext(ctx.header, ctx.var_types, ctx.modified,
                ctx.globals, ctx.cnames, ctx.sizes,
                ctx.declared, ctx.types, ctx.variability, info)
end

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

type CType
    cname::ASCIIString
    is_array::Bool
    fields::Dict
end

function declare(ctype, cname, variability=unbound, prefix="")
    qualifier = qualifiers[variability]
    suffix = ctype.is_array? "[]" : ""
    return strip("$qualifier $(ctype.cname) $prefix $cname$suffix")
end

function declare_alias(ctype, cname, variability=unbound, prefix="")
    qualifier = qualifiers[variability]
    prefix = ctype.is_array? "* $qualifier" : ""
    return strip("$qualifier $(ctype.cname) $prefix $cname")
end

function get_ctype(T, ctx::EmitContext)
    ctype = get(ctx.types, T, nothing)
    if ctype == nothing
        ctype = new_ctype(T, ctx)
        ctx.types[T] = ctype
    end
    ctype
end

function new_ctype(T, ctx::EmitContext)
    if T in keys(ispc_types)
        # Some atomic types can be directly translated
        # to ISPC types:
        cname = ispc_types[T]
        return CType(cname, false, Dict())
    elseif length(T.types) > 1
        # Composite type
        return declare_struct(T, ctx)
    elseif issubtype(T, DenseArray)
        # check for DenseArray and not AbstractArray
        # because that would pull in all indexable types
        celtype = get_ctype(eltype(T), ctx)
        return CType(celtype.cname, true, Dict())
    else
        error("Unsupported type $T in $(ctx.loc_info)")
    end
end

function declare_struct(typ, ctx::EmitContext)
    # As a first-level identifier the type also needs a unique name:
    substitute_identifiers([typ.name], ctx.cnames, globals=ctx.globals)
    ctypename = ctx.cnames[typ.name]
    push!(ctx.globals, ctypename)

    field_decls = []
    cfnames = substitute_identifiers(fieldnames(typ))
    for (fname, ftype) in zip(fieldnames(typ), typ.types)
        cfname = cfnames[fname]
        cftype = get_ctype(ftype, ctx)
        fdecl = declare(cftype, cfname)
        push!(field_decls, "$fdecl;")
    end

    decl = """
    struct $(ctypename) {
    $(indent(field_decls))
    };"""
    push!(ctx.header, decl)

    return CType(ctypename, false, cfnames)
end

# A number of definitions to accomodate some quirks in the generated
# code.
const ispc_includes = """
// Use ISPC's multiple dispatch capabilities to deal with the fact
// that Julia uses the same function for bitwise and boolean NOT,
// whereas the ~ operator in ISPC does not work on booleans:
inline bool __not(bool val) {return !val;} // boolean NOT
inline int8 __not(int8 val) {return ~val;} // all others are bitwise
inline int16 __not(int16 val) {return ~val;}
inline int32 __not(int32 val) {return ~val;}
inline int64 __not(int64 val) {return ~val;}
inline unsigned int8 __not(unsigned int8 val) {return ~val;}
inline unsigned int16 __not(unsigned int16 val) {return ~val;}
inline unsigned int32 __not(unsigned int32 val) {return ~val;}
inline unsigned int64 __not(unsigned int64 val) {return ~val;}
"""

function indent(s::AbstractString)
    indent(split(s, '\n'))
end

function indent(lines::AbstractArray)
    join(["    " * string(line) for line in lines], "\n")
end

function emit_ispc(obj::Any, ctx::EmitContext)
    error("Unhandled object $obj ($(typeof(obj))) in $(ctx.loc_info)")
end

function emit_ispc(obj::Void, ctx::EmitContext)
    ""
end

function emit_ispc(num::Real, ctx::EmitContext)
    if num == Inf32
        return "floatbits(0x7F800000)"
    elseif num == -Inf32
        return "floatbits(0xFF800000)"
    end
    # Use hexadecimal float literals so that we
    # can represent the number exactly:
    return @sprintf("%a",num)
end

function emit_ispc(num::Integer, ctx::EmitContext)
    return "$num"
end

function emit_ispc(symbol::SymbolNode, ctx::EmitContext)
    return ctx.cnames[symbol.name]
end

function emit_ispc(symbol::Symbol, ctx::EmitContext)
    return ctx.cnames[symbol]
end

function emit_ispc(symbol::GenSym, ctx::EmitContext)
    return ctx.cnames[symbol]
end

function emit_ispc(node::QuoteNode, ctx::EmitContext)
    return node.value
end

function emit_ispc(top::TopNode, ctx::EmitContext)
    error("Unhandled: $(sexpr(top)) in $(ctx.loc_info)")
end

function emit_ispc(ref::GlobalRef, ctx::EmitContext)
    return emit_ispc(eval(ref), ctx)
end

function emit_ispc(typ::DataType, ctx::EmitContext)
    return get_ctype(typ, ctx).cname
end

function emit_ispc(expr::Expr, ctx::EmitContext)
    emit_ispc(Val{expr.head}, expr.args, with_loc_info(ctx, sexpr(expr)))
end

function emit_ispc(head::Any, args, ctx::EmitContext)
    error("Unhandled expression $head $(sexpr(args)) in $(ctx.loc_info)")
end

function emit_ispc(head::Type{Val{:new}}, args, ctx::EmitContext)
    objtype = args[1]
    values = args[2:end]
    cargs = join([emit_ispc(val, ctx) for val in values], ", ")
    return "{$cargs}"
end

function emit_ispc(head::Type{Val{:block}}, args, ctx::EmitContext)
    body = join([emit_ispc(arg, ctx) for arg in args], ";\n")
    return body
    # Remember: :block does not introduce a nested scope, so it's not
    # equivalent to curly brackets in C.
    # return """{
    # $body
    # }
    # """
end

function emit_ispc(head::Type{Val{:call}}, args, ctx::EmitContext)

    f_expr = args[1]
    f_args = args[2:end]
    f_argtypes = (map(typeof, f_args)...)

    # Catch a few corner cases:
    if f_expr == TopNode(:getfield) && f_argtypes == (GlobalRef, QuoteNode)
        # getfield(GlobalRef, QuoteNode) points to an object that we
        # should be able to resolve here:
        mod = eval(f_args[1])
        symbol = f_args[2].value
        return eval(GlobalRef(mod, symbol), ctx)
    end

    if f_expr == TopNode(:ccall)
        # We don't generally support ccall() in ISPC code,
        # but we can rewrite some to calls to ISPC's stdlib:
        func = eval(f_args[1])
        f_argtypes = eval(f_args[3])
        f_args = f_args[4:4+length(f_argtypes)-1]        
        # there is sometimes one extra arg to ccall, so
        # make sure to take only as many as in arg_types.

    else
        # Just resolve the function:
        func = eval(f_expr)
    end

#    try
        return emit_func_call(func, f_args, ctx)
#    catch e
#        if isa(e, KeyError)
#            error("Error while translating $func in $(ctx.loc_info)")
#        end
#        rethrow(e)
#    end
end


function emit_func_call(f, args, ctx::EmitContext)
    if f == Base.arrayref
        return do_arrayref(ctx, args...)
    elseif f == Base.arrayset
        return do_arrayset(ctx, args...)
    elseif f == Base.getfield
        return do_getfield(ctx, args...)
    else
        fgen = basefuncs[f]
        fargs = [emit_ispc(arg, ctx) for arg in args]
        return fgen(fargs...)
    end
end

function do_getfield(ctx::EmitContext, obj, field::QuoteNode)
    cobj = emit_ispc(obj, ctx)
    ctype = get_ctype(ctx.var_types[obj], ctx)
    cfield = ctype.fields[field.value]
    return "$cobj.$cfield"
end

function col_major_index(sizes, indices)
    expr = indices[end]
    for i in length(sizes)-1:-1:1
        expr= :($(indices[i]) + $(sizes[i]) * $expr)
    end
    expr
end

function do_arrayref(ctx::EmitContext, array, I...)
    return indexed_array(ctx, array, I...)
end

function do_arrayset(ctx::EmitContext, array, value, I...)
    item = indexed_array(ctx, array, I...)
    val = emit_ispc(value, ctx)
    return "$item = $val;"
end

function indexed_array(ctx::EmitContext, array, I...)
    arr = emit_ispc(array, ctx)
    sizes = try
        ctx.sizes[array.name]
    catch e
        if isa(e, KeyError)
            error("Can't find array size for $(array.name) in $(ctx.sizes)")
        else
            rethrow(e)
        end
    end
    if length(I) == 1
        iexpr = :($(I[1]) - 1)
        idx = emit_ispc(iexpr, ctx)
    elseif length(I) == length(sizes)
        indices = [:($i-1) for i in I]
        idx = emit_ispc(col_major_index(sizes, indices), ctx)
    else
        error("There must be either one index or as many as dimensions (in $(ctx.loc_info))")
    end
    return "$arr[$idx]"
end

function emit_ispc(head::Type{Val{:return}}, args, ctx::EmitContext)
    if length(args) > 1
        error("Multiple return values are not supported (in $(ctx.loc_info)")
    end
    if args[1] == nothing
        return """return;"""
    else
        carg = emit_ispc(args[1], ctx)
        return """return $carg;"""
    end
end

function emit_ispc(head::Type{Val{:if}}, args, ctx::EmitContext)
    test = emit_ispc(args[1], ctx)
    then = args[2]
    coherent = false
    filter!(then.args) do stmt
        if stmt == Expr(:meta, :ispc, :coherent)
            coherent = true
            return false
        end
        return true
    end

    if_keyword = coherent ? "cif" : "if"
    thenblock = indent(emit_ispc(then, ctx))
    elsebranch = ""
    if length(args) >= 3
        elseblock = indent(emit_ispc(args[3], ctx))
        elsebranch = """
        else {
        $elseblock
        }"""
    end
    return """
    $(if_keyword) ($test) {
    $thenblock
    } $elsebranch"""
end

function emit_ispc(head::Type{Val{:dowhile}}, args, ctx::EmitContext)
    test = emit_ispc(args[1], ctx)
    block = args[2]
    coherent = false
    filter!(block.args) do stmt
        if stmt == Expr(:meta, :ispc, :coherent)
            coherent = true
            return false
        end
        return true
    end

    keyword = coherent ? "cdo" : "do"
    do_block = indent(emit_ispc(block, ctx))
    return """
    $(keyword) {
    $do_block
    } while ($test);"""
end

function emit_ispc(head::Type{Val{:while}}, args, ctx::EmitContext)
    test = emit_ispc(args[1], ctx)
    block = args[2]
    coherent = false
    filter!(block.args) do stmt
        if stmt == Expr(:meta, :ispc, :coherent)
            coherent = true
            return false
        end
        return true
    end

    keyword = coherent ? "cwhile" : "while"
    do_block = indent(emit_ispc(block, ctx))
    return """
    $(keyword)($test) {
    $do_block
    }"""
end

function emit_ispc(head::Type{Val{:continue}}, args, ctx::EmitContext)
    return """continue;"""
end

function emit_ispc(head::Type{Val{:break}}, args, ctx::EmitContext)
    return """break;"""
end


function emit_ispc(head::Type{Val{:foreach}}, args, ctx::EmitContext)
    targets, block = args
    body_ctx = nested_contex(ctx, unbound)
    body = indent(emit_ispc(block, body_ctx))

    active_exprs = (:active, QuoteNode(:active), Expr(:quote, :active))
    if length(targets) == 1 && targets[1].first in active_exprs
        idx = ctx.cnames[targets[1].second]
        return """
        foreach_active($idx) {
        $body
        }"""
    end

    iters = []
    for (object, index) in targets
        idx = ctx.cnames[index]
        if isa(object, SymbolNode)
            # It's an array symbol
            len = ctx.sizes[object.name][1]
            push!(iters, "$idx = 1 ... ($len+1)")
        elseif isa(object, Expr) && object.head == :(:)
            # It's a literal range
            start = emit_ispc(object.args[1], ctx)
            stop = emit_ispc(object.args[2], ctx)
            push!(iters, "$idx = $start ... ($stop+1)")
        else
            dump(object)
            error("unsupported iteration object: $object (in $(ctx.loc_info)")
        end
    end
    return """
    foreach($(iters...)) {
    $body
    }"""
end

function emit_ispc(head::Type{Val{:unmasked}}, args, ctx::EmitContext)
    body = join([indent(emit_ispc(arg, ctx)) for arg in args], "\n")
    return """unmasked {
    $body
    }"""
end

function emit_ispc(head::Type{Val{:(=)}}, args, ctx::EmitContext)
    lhs, rhs = args
    rhs_code = emit_ispc(rhs, ctx)

    if isa(lhs, Symbol) && isa(rhs, SymbolNode)
        # Alias any array sizes:
        try
            ctx.sizes[lhs] = ctx.sizes[rhs.name]
        catch
        end
    end

    cname = ctx.cnames[lhs]
    declared = lhs in ctx.declared
    if !declared
        push!(ctx.declared, lhs)
        ctype = get_ctype(ctx.var_types[lhs], ctx)
        if ctype.is_array
            # all arrays are currently uniform
            # TODO get variability from type
            decl = declare_alias(ctype, cname, uniform)
        else
            decl = declare(ctype, cname, ctx.variability)
        end
        return "$decl = $rhs_code;"
    else
        return "$cname = $rhs_code;"
    end
end


function emit_ispc(newvar::NewvarNode, ctx::EmitContext)
    cname = get(ctx.cnames, newvar.name, nothing)
    if cname == nothing
        println(STDERR, "Warning: encountered bug https://github.com/JuliaLang/julia/issues/12620 -- correctness is not guaranteed.")
        prefix = string(newvar.name)
        for key in keys(ctx.cnames)
            if startswith(string(key), "##$(prefix)#")
                println(STDERR, "Guessing $newvar => $(NewvarNode(key))")
                return emit_ispc(NewvarNode(key), ctx)
            end
        end
        return ""
    end
    ctype = get_ctype(ctx.var_types[newvar.name], ctx)
    decl = declare(ctype, cname, ctx.variability)
    push!(ctx.declared, newvar.name)
    return "$(decl);"
end


import Graphs

function to_ispc(ast, ctx::EmitContext)

    # println(var_types)

    # Get rid of line number nodes:
    ast = strip_lineno(ast)

    # Simplify jump analysis by replacing labels with direct indexing:
    statements = labels_to_lines(ast.args)
    # print_statements(STDOUT, statements)

    graph = to_graph(statements)
    dot = Graphs.to_dot(graph)
    open("graph.dot", "w") do f
        write(f, dot)
    end

    # Recover a structured control flow:
    statements, _ = raise_ast(statements)
    # print_statements(STDOUT, statements)

    ast = Expr(:block, statements...)

    # Make trees from opening and closing :ispc tags:
    ast = meta_to_trees(ast)

    # Collect the indices to foreach statements:
    ast = collect_foreach_indices(ast)
    # println(ast)

    # Emit ISPC code:

    # code = try
        code = emit_ispc(ast, ctx)
    # catch e
    #    println(STDERR, ctx.var_types)
    #    print_statements(STDERR, statements)
    #    rethrow(e)
    # end
    return code
end


function gen_ispc_func(ctx::EmitContext, name, ret_type, body, arg_types, arg_symbols...)
    c_ret_type = get_ctype(ret_type, ctx).cname
    c_args = []
    for (argtype, argname) in zip(arg_types, arg_symbols)
        prefix = argname in ctx.modified ? "&" : ""
        carg = declare(get_ctype(argtype, ctx), ctx.cnames[argname], uniform, prefix)
        push!(c_args, carg)
    end
    c_args_decls = join(c_args, ", ")
    return """
    export $c_ret_type $name($c_args_decls) {
    $body
    }
    """
end


function ispc_codegen!(func::ISPCFunction, kernel_name)
    sizes = Dict()
    argtypes = []
    argnames = []
    for arg in func.arg_names
        if isa(arg, Symbol) || isa(arg, Expr)
            push!(argnames, arg)
            push!(argtypes, func.var_types[arg])
        else
            symbol = arg[1]
            sizes[symbol] = arg[2:end]
            for a in arg
                push!(argnames, a)
                push!(argtypes, func.var_types[a])
            end
        end
    end

    # Replace Julia identifier with C-compatible ones:
    cnames = substitute_identifiers(keys(func.var_types),
                                    globals=func.file.global_names)

    substitute_identifiers([kernel_name], cnames,
                                    globals=func.file.global_names)

    header = []
    declared = Set(argnames)
    ctx = EmitContext(header, func.var_types, func.modified,
                        func.file.global_names, cnames, sizes,
                        declared, Dict(), uniform, "")

    body = indent(to_ispc(func.ast, ctx))

    io = IOBuffer()
    for decl in ctx.header
        write(io, decl)
        write(io, "\n")
    end
    func.ispc_name = cnames[kernel_name]
    func_code = gen_ispc_func(ctx, func.ispc_name, Void, body,
                                   argtypes, argnames...)
    write(io, func_code)
    write(io, "\n")
    func.ispc_code = takebuf_string(io)
end



"""
Converts symbols to unique C-proof identifiers.
"""
function substitute_identifiers(symbols, subst=Dict(); globals=Set())
    idx = 1
    for s in unique(symbols)
        if isa(s, GenSym)
            base = "_gensym$(s.id)"
        else
            base = gen_valid_identifier(string(s))
        end
        name = base
        while name in values(subst) || name in globals
            name = "$(base)_$(idx)"
            idx += 1
        end
        subst[s] = name
    end
    return subst
end

# Converting Julia identifiers to C identifiers:
# We could do it the boring way and generate faceless numbered
# variables, but for clarity and aesthetics we'll be extra nice
# and even try to spell out the names of unicode characters.
# Sorry about the wasted microseconds. Maybe add a debug switch.

latex_symbols = Base.REPLCompletions.latex_symbols
const unicode_names = Dict([reverse(p) for p in latex_symbols])
const disallowed = r"[^a-zA-Z0-9_]"
# this does not catch all cases but is enough when the input is
# already a valid Julia identifier (eg. no initial digit etc.)


function gen_valid_identifier(s::ASCIIString)
    replace(s, disallowed, "_")
end

function gen_valid_identifier(s::UTF8String)
    # Replace all non-ascii characters with either the
    # corresponding latex command, or an underscore:
    s2 = join(map(collect(s)) do c
        isascii(c) ? c : get(unicode_names, "$c", "_")
    end, "")
    gen_valid_identifier(ascii(s2))
end

