# dotcl-gpu

Run a Lisp function over arrays on an [ILGPU](https://www.ilgpu.net/) accelerator.

    (require "dotcl-gpu")
    (dotcl-gpu:gpu-map '(lambda (x) (* 2d0 x))
                       (make-array 4 :element-type 'double-float
                                     :initial-contents '(1d0 2d0 3d0 4d0)))
    ;; => #(2.0d0 4.0d0 6.0d0 8.0d0)

`gpu-map` takes a lambda expression of one to three double-float arguments and
that many `(simple-array double-float (*))` of one length, and returns a new
array with the function applied to every element. Two arrays:

    (dotcl-gpu:gpu-map '(lambda (a b) (+ (* a a) b)) xs ys)

How it works: dotcl compiles the lambda to a fasl, the contrib reads the IL of
its typed entry (the static method dotcl emits for a function of double-floats),
re-emits it without the function-object parameter as a device function, and
gives ILGPU a kernel that calls it for each element. The kernel is built once
per lambda expression (compared with `equal`) and kept; `clear-kernel-cache`
drops them.

What can be a device function: straight-line double-float arithmetic and calls to
`System.Math` (`sqrt`, `sin`, `exp`, ...). A branch, a loop, a call to another
function or a value that dotcl has to box signals `unsupported-kernel`.

Needs a build with code generation (it compiles the lambda and ILGPU reads IL at
run time), the .NET SDK, and the network the first time: NuGet restores ILGPU when
the first kernel is built, not when the module is required. Only the CPU
accelerator is used for now.
