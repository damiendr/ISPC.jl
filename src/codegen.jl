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

const includes = """

struct UnitRange {
    const int64 start;
    const int64 stop;
};

"""

function gen_ispc_call(call_id, ret_type, arg_types, arg_symbols...)
    quote
        ccall(call_id, ret_type, arg_types, arg_symbols...)
    end
end


immutable EmitContext
    types::Dict
    cnames::Dict
    sizes::Dict
    declared::Set
end

function indent(s::AbstractString)
    join(["    " * line for line in split(s, '\n')], "\n")
end

function emit_ispc(obj::Any, ctx::EmitContext)
    error("Unhandled object: $obj ($(typeof(obj)))")
end

function emit_ispc(num::Real, ctx::EmitContext)
    return "$(Float64(num))"
    # print as a Float64 because Julia has a non-C
    # syntax for Float32 exponents eg. 2.134f10
end

function emit_ispc(num::Integer, ctx::EmitContext)
    return "$num"
end

function emit_ispc(symbol::SymbolNode, ctx::EmitContext)
    return ctx.cnames[symbol.name]
end

function emit_ispc(symbol::GenSym, ctx::EmitContext)
    return ctx.cnames[symbol]
end

function emit_ispc(node::QuoteNode, ctx::EmitContext)
    return node.value
end


function emit_ispc(top::TopNode, ctx::EmitContext)
    # if top.name == :getfield
    #     error("stop")
    #     return GlobalRef
    # end
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

    # catch a few corner cases:
    if f_expr == TopNode(:getfield) && f_argtypes == (GlobalRef, QuoteNode)
        # getfield(GlobalRef, QuoteNode) points to an object that we
        # should be able to resolve here:
        mod = eval(f_args[1])
        symbol = f_args[2].value
        return eval(GlobalRef(mod, symbol), ctx)

    elseif f_expr == TopNode(:ccall)
        # We can't ccall from inside ISPC, but we can rewrite
        # some ccalls, eg. those to math functions.
        func = eval(f_args[1])
        f_argtypes = eval(f_args[3])
        f_args = f_args[4:4+length(f_argtypes)-1]        
        # there is sometimes one extra arg to ccall, so
        # make sure to take only as many as in arg_types.

    else
        func = eval(f_expr)
    end

    func = try
       basefuncs[func]
    catch
        error("Unhandled function $func from $(sexpr(args))")
    end

    cargs = [emit_ispc(arg, ctx) for arg in f_args]
    return func(cargs...)
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
    body = indent(emit_ispc(block, ctx))

    if length(targets) == 1 && targets[1].first == QuoteNode(:active)
        idx = ctx.cnames[targets[1].second]
        return """
        foreach_active($idx) {
        $body
        }"""
    end

    iters = []
    for (array, index) in targets
        idx = ctx.cnames[index]
        len = ctx.sizes[array.name][1]
        push!(iters, "$idx = 0 ... $len")
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

        if isa(rhs, SymbolNode)
            # This assignment is essentially an alias.
            # Use a #define for it because that handles
            # array types (for instance from arguments)
            # in a transparent manner:
            return "#define $cname $rhs_code"
        end

        decl = to_c_type(ctx.types[lhs], cname)
        return "$decl = $rhs_code;"
    else
        return "$cname = $rhs_code;"
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

import Graphs

function to_ispc(fragment, var_types, cnames, sizes)

    # Get rid of line number nodes:
    fragment = strip_lineno(fragment)

    # Simplify jump analysis by replacing labels with direct indexing:
    statements = labels_to_lines(fragment)
    # print_statements(STDOUT, statements)

    # graph = to_graph(statements)
    # dot = Graphs.to_dot(graph)
    # open("graph.dot", "w") do f
    #     write(f, dot)
    # end

    # Recover a structured control flow:
    statements, _ = raise_ast(statements)
    # print_statements(STDOUT, statements)

    ast = Expr(:block, statements...)

    # Make trees from opening and closing :ispc tags:
    ast = meta_to_trees(ast)

    # Collect the indices to foreach statements:
    ast = collect_foreach_indices(ast)

    # println(ast)
    # error("done")

#    ast = rewrite_libm_calls(ast)

    # Resolve TopNodes:
#    ast = resolve_topnodes(ast)

#    println()
#    dump(ast, 100)
#    println(ast)
#    println()

#    println(ast)

    # Emit ISPC code:
    ctx = EmitContext(var_types, cnames, sizes, Set())

#    code = try
    code = emit_ispc(ast, ctx)
#    catch e
#        println(ast)
#        rethrow(e)
#    end
    return code
end


function ispc_codegen(io, func::ISPCFunction)

    sizes = Dict()
    argtypes = []
    argnames = []
    for arg in func.signature
        if isa(arg, Symbol)
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

    body = indent(to_ispc(func.fragment, func.var_types, cnames, sizes))

    func_name = "ispc_func_$(func.idx)"
    func_code = gen_ispc_func(func_name, Void, body, argtypes, argnames...)
    write(io, func_code)
    return func_name
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

