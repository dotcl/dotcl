# dotcl-jitdisasm

Native disassembly of a compiled Lisp function: after loading this contrib,
`(dotcl:jit-disassemble #'some-fn)` prints the machine code the JIT produced for
it. A development tool for looking at what the compiler ends up emitting.

    (require "dotcl-jitdisasm")

Not an ASDF system, and not part of the packaged dotcl. The directory holds
`JitDisasm.cs` and its project file; the `.lisp` loads the built
`lib/DotCL.Contrib.JitDisasm.dll` (Iced as the disassembler) and wires it into
the `dotcl:jit-disassemble` hook. Build it from a source tree first, with
`make contrib-dotcl-jitdisasm`.
