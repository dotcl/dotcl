# dotcl-rotate-byte

`rotate-byte`, with the interface of SBCL's `sb-rotate-byte`:
`(rotate-byte count bytespec integer)` returns `integer` with the bits in
`bytespec` rotated left by `count` (right when `count` is negative) and the
other bits unchanged.

    (require "dotcl-rotate-byte")
    (dotcl-rotate-byte:rotate-byte 3 (byte 32 0) #x80000001) ; => #xC

A call on `(byte 32 0)` whose integer is known to be an `(unsigned-byte 32)`
and whose count is known to be a fixnum compiles to one 32-bit rotate. The same
holds for `(byte 64 0)` and an `(unsigned-byte 64)`. Other calls go through the
general function.

No dependencies.
