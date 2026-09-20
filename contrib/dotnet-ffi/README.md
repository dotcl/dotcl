# dotnet-ffi

`dotnet:define-ffi` -- declare a native function and get a Lisp function that
calls it:

    (require "dotnet-ffi")
    (dotnet:define-ffi set-console-mode "kernel32.dll" "SetConsoleMode"
                       :args '(:ptr :uint32) :ret :bool)
    (set-console-mode handle mode)   ; => T or NIL

Not an ASDF system: the directory holds one file, a macro over the runtime's own
`dotnet:%ffi-call`, and `require` is how it is loaded. No dependencies. Which
library names resolve is a platform question -- the example above is Windows.
