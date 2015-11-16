# ISPC code generation.

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

function to_c_type(T, name="")
    if issubtype(T, AbstractArray)
        arg = to_c_type(eltype(T), name)
        return "$arg[]"
    else
        ctype = ispc_types[T]
        return strip("$ctype $name")
    end
end

const ispc_includes = """
// A number of definitions to accomodate some quirks in the generated
// code.

struct UnitRange {
    const int64 start;
    const int64 stop;
};

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


immutable EmitContext
    types::Dict
    cnames::Dict
    sizes::Dict
    declared::Set
    uniform::Bool
end

function indent(s::AbstractString)
    join(["    " * line for line in split(s, '\n')], "\n")
end

function emit_ispc(obj::Any, ctx::EmitContext)
    error("Unhandled object: $obj ($(typeof(obj)))")
end

function emit_ispc(num::Real, ctx::EmitContext)
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

function emit_ispc(newvar::NewvarNode, ctx::EmitContext)
    cname = get(ctx.cnames, newvar.name, nothing)
    if cname == nothing
        return ""
    end
    decl = to_c_type(ctx.types[newvar.name], cname)
    push!(ctx.declared, newvar.name)
    return "$(decl);"
end

function emit_ispc(top::TopNode, ctx::EmitContext)
    error("Unhandled: $(sexpr(top))")
end

function emit_ispc(ref::GlobalRef, ctx::EmitContext)
    return emit_ispc(eval(ref), ctx)
end

function emit_ispc(typ::Type, ctx::EmitContext)
    return to_c_type(typ)
end

function emit_ispc(expr::Expr, ctx::EmitContext)
    emit_ispc(Val{expr.head}, expr.args, ctx)
end

function emit_ispc(head::Any, args, ctx::EmitContext)
    return "unsupported!!: $(sexpr(head)) $(sexpr(args))"
end

function emit_ispc(head::Type{Val{:block}}, args, ctx::EmitContext)
    body = join([emit_ispc(arg, ctx) for arg in args], "\n")
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

    return emit_func_call(func, f_args, ctx)

    # codegen_func = try
    #     basefuncs[func]
    # catch
    #     error("Unhandled function $func from $(sexpr(args))")
    # end

    # cargs = [emit_ispc(arg, ctx) for arg in f_args]
    # return codegen_func(cargs...)
end


function emit_func_call(f, args, ctx::EmitContext)
    if f == Base.arrayref
        return do_arrayref(ctx, args...)
    elseif f == Base.arrayset
        return do_arrayset(ctx, args...)
    else
        fgen = basefuncs[f]
        fargs = [emit_ispc(arg, ctx) for arg in args]
        return fgen(fargs...)
    end
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
    sizes = ctx.sizes[array.name]
    if length(I) == 1
        idx, = emit_ispc(I, ctx)
    elseif length(I) == length(sizes)
        idx = emit_ispc(col_major_index(sizes, I), ctx)
    else
        error("There must be either one index or as many as dimensions")
    end
    return "$arr[$idx]"
end

function emit_ispc(head::Type{Val{:return}}, args, ctx::EmitContext)
    if length(args) > 1
        error("Multiple return values are not supported")
    end
    if args[1] == nothing
        return """return;"""
    else
        carg = emit_ispc(arg, ctx)
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
    body_ctx = EmitContext(ctx.types, ctx.cnames, ctx.sizes,
                            ctx.declared, false)
    body = indent(emit_ispc(block, body_ctx))

    if length(targets) == 1 && targets[1].first == QuoteNode(:active)
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
            push!(iters, "$idx = 0 ... $len")
        elseif isa(object, Expr) && object.head == :(:)
            # It's a literal range
            start = emit_ispc(object.args[1], ctx)
            stop = emit_ispc(object.args[2], ctx)
            push!(iters, "$idx = $start ... $stop")
        else
            dump(object)
            error("unsupported iteration object: $object")
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

    cname = ctx.cnames[lhs]
    declared = lhs in ctx.declared
    if !declared
        push!(ctx.declared, lhs)
        decl = to_c_type(ctx.types[lhs], cname)
        qual = ifelse(ctx.uniform, "uniform", "")
        return "$qual $decl = $rhs_code;"
    else
        return "$cname = $rhs_code;"
    end
end


import Graphs

function to_ispc(ast, var_types, cnames, sizes)

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
    ctx = EmitContext(var_types, cnames, sizes, Set(), true)

#    code = try
    code = emit_ispc(ast, ctx)
#    catch e
#       println(STDERR, ast)
#       rethrow(e)
#    end
    return code
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


function ispc_codegen!(func::ISPCFunction)

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
    cnames = substitute_identifiers(keys(func.var_types))
    argnames = [cnames[key] for key in argnames]

    body = indent(to_ispc(func.ast, func.var_types, cnames, sizes))

    func.ispc_name = "ispc_func_$(func.idx)"
    func.ispc_code = gen_ispc_func(func.ispc_name, Void, body,
                                   argtypes, argnames...)
    return func
end

"""
Converts symbols to unique C-proof identifiers.
"""
function substitute_identifiers(symbols)
    idx = 1
    subst = Dict()
    names = Set()
    for s in unique(symbols)
        if isa(s, GenSym)
            base = "_gensym$(s.id)"
        else
            base = gen_valid_identifier(string(s))
        end
        name = base
        while name in names
            name = "$(base)_$(idx)"
            idx += 1
        end
        push!(names, name)
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
# a valid Julia identifier (eg. no initial digit etc.)


function gen_valid_identifier(s::ASCIIString)
    replace(s, disallowed, "_")
end

function gen_valid_identifier(s::UTF8String)
    s2 = join(map(collect(s)) do c
        isascii(c) ? c : get(unicode_names, "$c", "_")
    end, "")
    gen_valid_identifier(ascii(s2))
end

