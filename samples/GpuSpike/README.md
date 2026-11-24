# GpuSpike

Does ILGPU accept the IL that dotcl emits? `ilgpu-spike.lisp` answers it for the
simplest case, an element-wise `y[i] = f(a, x[i], y[i])` on ILGPU's CPU
accelerator, where `f` is an ordinary Lisp function with an FTYPE of
double-floats.

```lisp
(load "samples/GpuSpike/ilgpu-spike.lisp")
(gpu-spike:run-typed-entry "axpy.fasl" "AXPY_typed" 2d0 :n 1000)
```

Where `axpy.fasl` came from compiling

```lisp
(declaim (ftype (function (double-float double-float double-float) double-float) axpy))
(defun axpy (a x y) (declare (optimize (debug 0) (safety 0))) (+ (* a x) y))
```

What the spike found:

- The typed entry of a straight-line double function is plain IL (loads, stores,
  `mul`, `add`, `call Math.Sqrt`). ILGPU compiles it and the kernel gives the
  right answer.
- It does not accept the typed entry as it is emitted: the first parameter is the
  function object, and passing one to the kernel fails ("Not supported instruction
  Ldnull"). The spike re-emits the body without that parameter.
- Anything that calls into the runtime's multiple-values bookkeeping is rejected
  ("Cannot load from the static field ... since it is not read only"). A `do` loop
  or a `PSETF` compiles to such calls, `leave` regions and boxed doubles, so loops
  are out until the compiler can emit a device-friendly form.
- A kernel that indexes `ArrayView1D` has to be emitted by hand here, because the
  compiler does not emit calls to `ArrayView1D<double, Dense>.get_Item`.

Needs the .NET SDK and, the first time, the network (NuGet restores ILGPU).
