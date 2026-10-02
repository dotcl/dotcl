# Deliberate deviations

Where dotcl knowingly behaves differently from the standard, or from SBCL. Each
row was decided rather than discovered: if you find something not listed here
that differs, it is a bug and worth reporting.

Two kinds of entry appear. Some rows differ from a strict reading of the
standard in order to agree with the implementations that real libraries are
written against. Others differ from SBCL, because the standard leaves the point
open and dotcl reads it differently.

| Area | dotcl | CLHS / SBCL | Why | State |
| --- | --- | --- | --- | --- |
| `defgeneric` over a name that already has an ordinary function | Warns, removes the function definition, and defines the generic function | CLHS 7.6.1 says an error is signaled; SBCL warns | Widely used libraries (cl-ppcre among them) reach this case while loading, and erroring stops them dead | permanent |
| Floating-point underflow | `exp` and `expt` flush to zero | CLHS 12.1.4.3 permits signaling `floating-point-underflow`; SBCL, CCL and ECL flush | The IEEE underflow trap is masked by default on every host dotcl runs on, and dotcl's own multiplication already flushed -- only `exp` and `expt` signalled, which is worse than either answer. Overflow still signals | permanent |
| Arithmetic `loop` variable seen by `finally` | The value after the final step: `(loop for x from 1 to 5 do (progn) finally (return x))` is 6 | The conformance suite expects the last in-body value; CLHS 6.1.2.1.1 does not settle it, and the suite tags those tests as a spec problem itself. SBCL and ABCL return the stepped value | Libraries are written against what the implementations do -- babel's unibyte encoders read the stepped value | permanent |
| Printing a circular list with `*print-circle*` nil | Stops and prints `...`. The cut is found inside the cycle rather than at its entry, so a few more elements are printed than a per-cons table would emit | The standard leaves printing circular structure without `*print-circle*` undefined | Terminating beats printing forever, and the two-pointer guard that finds the cycle costs no allocation, so ordinary list printing does not pay for it | permanent |
| Printing infinities and NaN | `#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY` and friends, which read back; with `*read-eval*` nil, an unreadable `#<...>` form; with `*print-readably*` also true, `print-not-readable` | The standard gives these no external representation; SBCL prints a constant from its own package | The printed form has to read back whatever `*package*` is, and an unqualified name does not: dotcl's `CL-USER` does not use `DOTCL` | permanent |
| `~E` with an explicit digit count | Rounds the exact binary value | SBCL rounds the shortest representation that reads back, then pads with zeros | The conformance suite checks `~E` output against the rational the float denotes, so the exact value is what passes there. `~F` follows SBCL, since nothing checks it against a rational | permanent |
| `open :direction :output :if-exists :append`: moving the position | Every write goes to the end of the file as it is at that moment, so another appender's data is never overwritten. `file-position` reports that end; setting it to anything else returns `nil` | SBCL opens with `O_APPEND` too, but setting the position returns `t` and the next write still goes to the end | Answering `t` would promise a write position that the OS does not honour. The end-of-file guarantee comes from the OS: an append-only handle on Windows, `O_APPEND` on Linux and macOS. Elsewhere (other Unix systems, WebAssembly) the stream seeks to the end once, at open, which is right only while it is the sole writer. `:direction :io` with `:if-exists :append` also still seeks once | until a caller needs the other answer |
| `~G`: how many digits a value with no short exact form gets, and which way an exact tie rounds | `(format nil "~g" 123456789.0)` gives `123456792.` (the exact single-float value); `(format nil "~,3g" 1234.5)` gives `1.234e+3` | SBCL gives `123456790.` (the shortest form that reads back) and `1.235e+3` | CLHS 22.3.3.3 decides neither, and dotcl's own format tests already record the direction of an exact tie as free. Every other `~G` form measured against SBCL agrees | until a caller needs one |

## Library backends

**CFFI foreign pointers are plain integers.** The CFFI-SYS backend that the
dotcl dist ships represents a foreign pointer as the integer address itself:
`null-pointer` is `0`, `inc-pointer` is `+`, and `foreign-pointer` is the type
`integer`. So `pointerp` is true of every integer, including `42` and `0`, and
`pointer-eq` and `null-pointer-p` accept any integer without signalling. On
SBCL a pointer is a separate object (a SAP) and all three reject a bare
integer. Four tests in CFFI's own suite check exactly that and fail on dotcl:
`POINTERP.4`, `POINTERP.5`, `POINTER-EQ.NON-POINTERS.1` and
`NULL-POINTER-P.NON-POINTER.2`. The integer form is what .NET interop hands
over and accepts for an address (`IntPtr` converts to and from an integer), so
every pointer crosses that boundary without a wrapper being allocated or
unwrapped, and pointer arithmetic stays integer arithmetic. CFFI's Allegro
backend makes the same choice for the null pointer, which is also the integer
`0` there. A distinct pointer type would change every path through `mem-ref`,
`inc-pointer` and the callbacks, and any existing code that passes an address
it got from .NET straight to CFFI; it is not planned until a library needs to
tell a pointer from an integer.

Behaviour at the boundary between Lisp and .NET -- which exception is caught
where, and what a condition escaping a callback does -- is a larger topic with
its own table in [DESIGN.md](../DESIGN.md), section 3.11.

## Where the conformance suite is adjusted

dotcl runs the ansi-test suite unmodified, from a fresh clone. Three
adjustments are made in the harness instead, and they follow from the table
above:

- **Eight `EXP.ERROR` / `EXPT.ERROR` tests are skipped**, through the note the
  suite already carries for implementations that do not signal underflow by
  default. SBCL and the others skip them the same way.
- **Four `LOOP.1` tests are skipped**, through a note registered for them,
  because they ask the question the standard leaves open above. The suite tags
  them as a spec problem itself; disabling the narrow note rather than that tag
  keeps unrelated tests running.
- **One printer test has its random range narrowed by a single value.** It
  draws integers from a range whose low endpoint is exactly the magnitude at
  which CLHS 22.1.3.1.3 stops using fixed-point notation, so at that one draw
  the test's expectation is out of spec -- dotcl, SBCL and ABCL all print it in
  exponential notation. The remaining draws still run, and the override checks
  that the upstream test still looks the way it did when it was written, so a
  change upstream is reported rather than silently shadowed.

Everything else in the suite is expected to pass; the current count and the one
known failure live in `ansi-state.json`.
