# ISPC.jl

[![Build Status](https://travis-ci.org/damiendr/ISPC.jl.svg?branch=master)](https://travis-ci.org/damiendr/ISPC.jl)

Tools & etc. to work with ISPC from Julia.

## High-level interface

The high-level interface translates Julia code that has been annotated with
a set of macros into code compiled by ISPC on-the-fly. Example:

```julia
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

Not all of Julia's syntax and types are supported.

- Arrays can only be indexed with integers and all outer
variables ("kernel arguments") must be primitive types or arrays of
primitive types.

- Simple composite types like `UnitRange` are supported inside kernels
(eg. in `for` loops) but not as kernel arguments at the moment.

Work in progress, expect bugs and missing math functions. More documentation to
come.

See the [example notebook](https://github.com/damiendr/ISPC.jl/blob/master/examples/ISPC-mandelbrot.ipynb) for an overview.

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