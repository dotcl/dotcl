# dotcl-simd

128-bit SIMD values with the type and operation names of sb-simd.

    (require "dotcl-simd")
    (dotcl-simd:f64.2+ (dotcl-simd:make-f64.2 1d0 2d0) 10d0)

See docs/extensions.md for the list of types and operations.
