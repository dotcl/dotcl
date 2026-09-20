# dotcl-float

IEEE bit patterns for floats, both directions: `single-float-bits`,
`double-float-bits`, `bits-single-float`, `bits-double-float`, plus constants
for the infinities and NaNs that cannot be written as literals
(`+single-float-nan+`, `+double-float-positive-infinity+`, and so on).

    (require "dotcl-float")

No dependencies. It is a thin layer over `System.BitConverter`, which lives in
System.Runtime and is always resolvable, so there is no helper assembly to
build.
