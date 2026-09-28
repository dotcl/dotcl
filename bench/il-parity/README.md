| tokenizer | 300 | 74033288 |
| vec2 | 200 | 245144966 |
| `tokenizer` | the only character case: one element read per iteration, compared against constants |
| `vec2` | the float case: DOUBLE-FLOAT slots, where the boxed representation costs an object per value rather than per value outside a cache |
# il-parity

Five small data structures written twice -- once in C#, once in fully declared
Common Lisp -- so that the IL dotcl emits can be diffed against the IL the C#
compiler emits for the same algorithm.

The time benchmark next door (`bench/csharp-parity`) answers "is this a
contest?". This one answers "which instructions are the difference?", which is a
sharper signal: it is deterministic, it names the surplus instruction, and it
survives a noisy machine.

```
bench/il-parity/
  <name>/Ref.cs       the C# reference           -> IlParityRefs.dll  (Release)
  <name>/impl.lisp    the Lisp counterpart       -> impl.fasl         (compile-file)
  Refs.csproj         builds every Ref.cs into one assembly
  tool/               reads IL out of either assembly and compares
```

Both outputs are IL-only .NET assemblies, so **one reader handles both sides**:
a dotcl `.fasl` is a PE file with method bodies in it exactly as a C# `.dll` is.

## The five

| case | what it exercises |
| --- | --- |
| `stack` | the container itself: a field read, an element access, a field write |
| `hash` | open addressing with fixnum keys -- a probe loop, index arithmetic, three parallel arrays |
| `heap` | the sift loops: index arithmetic that either stays in registers or spills |
| `ring` | the densest field traffic of the five, where a struct representation difference shows first |
| `tokenizer` | the only character case: one element read per iteration, compared against constants |

## Both halves compute the same number

Every case has a `SelfCheck(n)` / `ilp-<name>-selfcheck` that exercises the
structure and folds every step into one integer. Comparing IL between two
programs that do different things is meaningless, so the harness checks the
numbers agree before it reports on instructions:

| case | n | value |
| --- | --- | --- |
| stack | 50 | 2872 |
| hash | 200 | 219099 |
| heap | 200 | 1343498 |
| ring | 500 | 858950 |
| tokenizer | 300 | 74033288 |

## Writing a new case

- Give the Lisp **every declaration a careful writer would give** -- slot types,
  argument types, `(speed 3) (safety 0) (debug 0)`. What is being measured is
  what the compiler does with a fully declared program.
- Write the two sides in the same shape: same loop form, same early exits, same
  helper split. A difference in the IL should mean a difference in lowering, not
  a difference in phrasing.
- Return numbers, not objects. A case that allocates its results measures the
  allocator instead of the code under comparison.
- Add the pair to the self-check table above, and to `methods.txt` so the
  comparator knows which C# method answers which Lisp function.

## What a count change means, and what it does not

An instruction count is evidence about **emitted size**. It is not evidence
about **time**, in either direction, and the two have come apart in both
directions in practice.

A count that went **up** while the program got **faster**: the array element
buffer hoist used to read a buffer that a fill-pointered or adjustable vector
could replace underneath it, which lost writes silently. The fix gives each
access a path for the case where the buffer cannot be taken, and emits the body
twice for loops so the test sits outside the loop rather than inside it. Totals
here rose 1.3x to 1.8x -- `hash/ILP-HASH-PUT` 177 to 316, `heap/ILP-HEAP-POP`
141 to 247, `stack/ILP-STACK-PUSH` 36 to 47 -- while the machine code of the
inner loop came out **byte-identical** to what it had been, and the kernel it
was measured on ran in the same time as before. Read as "the gap to C# grew",
that would have been false in the only sense anyone cares about.

A count that stayed **the same** while the program got faster has also happened,
so the rule is not "up is bad, flat is good".

So when a count moves, say what moved in the emitted code and check the machine
code or the clock before concluding anything about speed. `DOTNET_JitDisasm` on
the inner loop settles it in minutes and has settled it three times.
