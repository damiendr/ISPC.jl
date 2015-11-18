# ISPC.jl

[![Build Status](https://travis-ci.org/damiendr/ISPC.jl.svg?branch=master)](https://travis-ci.org/damiendr/ISPC.jl)

Tools & etc. to work with ISPC from Julia.

## High-level interface

The high-level interface translates Julia code that has been annotated with
a set of macros into code compiled by ISPC on-the-fly.

Work in progress, expect bugs and missing math functions.

See the [example](https://github.com/damiendr/ISPC.jl/blob/master/examples/ISPC-mandelbrot.ipynb) for an overview.

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
fptr = fptr = Libdl.dlsym(lib, "simple")

# Call the kernel:
vin = rand(Float32, 1000);
vout = zeros(Float32, 1000);
ccall(fptr, Void, (Ref{Float32}, Ref{Float32}, UInt64), vin, vout, length(vout))
```