# ISPC.jl

[![Build Status](https://travis-ci.org/damiendr/ISPC.jl.svg?branch=master)](https://travis-ci.org/damiendr/ISPC.jl)

**Note: [recent changes](https://github.com/JuliaLang/julia/pull/13412) in the way Julia represents functions and closures require a redesign of this package, which will be published when the 0.5 IR stabilises a bit more.**

ISPC.jl uses the [Intel ISPC compiler](http://ispc.github.io) to compile fragments
of ISPC-C or Julia code to vector code (eg. SSE/AVX on Intel CPUs) at runtime.
It is similar to the standard `@simd` macro but supports control statements and a
number of math functions.

Speedups of 2x-8x compared to plain Julia code can be achieved (single core).

- high-level interface: translates Julia to ISPC
- low-level interface: compiles and loads ISPC programs

## Requirements

- Julia 0.4 or 0.5-dev up to [cf93d6f (2016-01-29 02:19 UTC)](https://github.com/JuliaLang/julia/commit/cf93d6fa9544995f1c402734894e99397167bf50)
- `ispc` (must be found in $PATH)
- `libtool` or `g++` (must be found in $PATH)
- (optional) `llvm_dis` for looking at ISPC llvm assembly.

## High-level interface

*Work in progress! The high-level interface is fairly functional already,
but bugs and the odd missing math function should be expected.
More documentation to come.*

See the [example notebook](https://github.com/damiendr/ISPC.jl/blob/master/examples/ISPC-mandelbrot.ipynb) for an overview.

The high-level interface translates Julia code that has been annotated with
a set of macros into code compiled by ISPC on-the-fly. Example:

```julia
using ISPC

@inline function mandel(c_re, c_im, count)
    z_re = c_re
    z_im = c_im
    i = 0
    while i < count
        if (z_re * z_re + z_im * z_im > 4.0f0)
            break
        end
        new_re = z_re*z_re - z_im*z_im
        new_im = 2.0f0 * z_re * z_im
        z_re = c_re + new_re
        z_im = c_im + new_im
        i += 1
    end
    return i
end

@ispc function mandelbrot_ispc(x0, y0, x1, y1, output, max_iters)
    height, width = size(output)
    dx = (x1 - x0) / width
    dy = (y1 - y0) / height
    @kernel(`--target=avx1-i32x8`) do
        for i = 1:width
            @foreach(1:height) do j
                x = x0 + i * dx
                y = y0 + j * dy
                output[j,i] = mandel(x, y, max_iters)
            end
        end
    end
    output
end;
```

Supported ISPC constructs:
- `@foreach`
- `@foreach(:active)` (untested)
- `@unmasked` (untested)
- `@coherent` (untested)

### Limitations

The high-level interface is meant to be used with numerical kernels only.
Not all of Julia's syntax and types are supported:

- Arrays can only be indexed with integers, providing either a single
  index (linear indexing) or one index per dimension (multi-dimensional
  arrays). Indexing must yield an array element, not a sub-array.

- All outer variables ("kernel arguments") must be primitive types
  or arrays of primitive types.

- Simple composite types like `UnitRange` are supported inside kernels
  (eg. in `for` loops) but not as kernel arguments at the moment.

- Only functions that have a direct translation to ISPC are supported 
  inside kernels. User-defined functions may be used if they are declared
  `@inline`. This restriction may be lifted in the future.

- Inner functions, exceptions, task switching, I/O etc. are not supported.

ISPC task-level constructs are not yet supported.

## Low-level interface

The low-level interface allows you to load and call fragments of ISPC code:

```julia
using ISPC

# A basic ISPC kernel:
code = """
export void simple(uniform float vin[], uniform float vout[],
                   uniform int count) {
    foreach (index = 0 ... count) {
        float v = vin[index];
        if (v < 0.5)
            v = v * v;
        else
            v = sqrt(v);
        vout[index] = v;
    }
}
"""

# Compile the code and get a function pointer to our kernel:
lib = load_ispc(code, `--target=avx1-i32x8`)
fptr = Libdl.dlsym(lib, "simple")

# Call the kernel:
vin = rand(Float32, 1000);
vout = zeros(Float32, 1000);
ccall(fptr, Void, (Ref{Float32}, Ref{Float32}, UInt64), vin, vout, length(vout))
```

## How it works

1. The `@kernel` macro creates a lambda function containing the kernel code.
2. `code_lowered()` or `code_typed()` is run on the main `@ispc` function to get
   information about closure variables captured by the kernel lambda. These become
   kernel arguments.
3. Kernel fragments in the main function are replaced by calls to a `@generated`
   `kernel_call` function.
4. When `kernel_call` is called for the first time, type inference is run on the
   kernel fragment. This gives us types for local variables and inlines functions
   that can be inlined.
5. The lowered and typed AST is transformed to *un-lower* `goto`s back into `if`
   and `while` statements (ISPC does not support varying `goto`s)
6. The transformed AST is translated to ISPC C
7. The resulting code is compiled with `ispc`, loaded with `Libdl` and called with `ccall`.
