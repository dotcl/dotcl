# ablation

An isolated C# harness for the question a profiler cannot answer: **which part
of this is the cost?**

`bench/csharp-parity` says whether dotcl is in the contest. `bench/il-parity`
says which instructions the gap is made of. This one says which of them you
would have to remove to close it, and how much each is worth -- by running the
same loop several times over, with one suspected cause removed each time.

It is a scratch harness, not a gate. Nothing in CI builds it. You build it by
hand, edit `Program.cs` down to the shape you are chasing, and throw the edit
away afterwards. What is worth keeping between investigations is the three
rules below, not the particular variants.

```
dotnet build bench/ablation/ablation.csproj -c Release
DOTNET_gcServer=0 bench/ablation/bin/Release/net10.0/ablation.exe
```

## Why it exists

A sampling profile of a fully declared numeric kernel tells you nothing,
because everything that runs is inlined into one method: the profile is one
peak whose inclusive time equals its exclusive time. The disassembly then shows
you every instruction, but it will not tell you which of the forty you are
looking at are the ones you can afford to remove. Between those two there is a
gap, and this is what fills it.

## The three rules

1. **One variant per suspected cause.** Each variant removes exactly one thing
   relative to the one above it, so the difference between two adjacent rows
   is the price of the thing between them. A variant that changes two things
   at once prices neither.

2. **Alternate every variant inside one process.** Run them round-robin and
   keep the minimum of five, rather than running one to completion and then
   the next. A machine that gets busier partway through otherwise lands
   entirely on whichever variant was running at the time, and the ranking it
   produces can come out backwards. Warm every variant before timing any of
   them, or the first one measured pays for tiering that the others do not.

3. **Include a variant with everything removed.** That is the reachable
   ceiling. Without it you learn that something is expensive; with it you learn
   how much of the job is left after you remove it -- and sometimes, as below,
   that the thing you suspected of being a design mistake is in fact free.

Two more that are not specific to this harness but break results just as
thoroughly: build the input once, outside every timed region, and never let the
loop construct what it measures.

## A microbenchmark measures the operation, not the program

The one thing this harness cannot tell you is whether the operation it prices
is a large enough part of anything to matter, and it will happily report a
convincing speedup for a change that makes real code slower.

Worked through once, on the boxed structure slot read. This harness said the
operation went from 12.2x the ceiling to 5.1x -- a 2.4x speedup, stable across
three rounds with every unchanged variant holding still. A compile workload
then measured no difference at all over nine alternated rounds, and a
struct-walking Lisp loop measured **42% slower**. The disassembly said why: the
change had made the runtime entry small enough for the JIT to inline, and its
body carries the boxing sites for raw slots, so inlining it into a hot Lisp
loop took that loop from 460 bytes of code and one allocation site to 1044 and
five. The harness had measured the operation in a caller with nothing else in
it, which is the one place where that does not cost anything.

So: price the operation here, then confirm on a workload, and believe the
workload. The two rows above that agreed with each other were both wrong about
the program.

## The worked example: structure slot access

`Program.cs` is left holding the investigation it was written for, as a worked
example of the shape. Loop body `acc += A + B; A = i`, 5e7 iterations. V1 calls
the real `DotCL.Runtime` on a real `LispStruct`; V2 to V6 are replicas of the
same guard chain with one guard removed at a time; V7 is a plain sealed class
with two `long` fields, which is what `bench/csharp-parity` measures.

| variant | vs ceiling | ms |
| --- | ---: | ---: |
| V1 real `Runtime.StructRefL` / `StructSetL` | 5.21x | 164.2 |
| V2 replica of the same chain | 4.45x | 140.2 |
| V3 minus the `SlotCount` test | 3.75x | 118.1 |
| V4 minus the layout indirection | 1.73x | 54.6 |
| V5 cast only | **1.01x** | 32.0 |
| V6 backing array hoisted out of the loop | **1.04x** | 32.9 |
| V8 hoisted, with a per-access null test and a live arm | 1.96x | 61.7 |
| V7 ceiling, plain `long` fields | 1.00x | 31.5 |

The ratio is the number to quote and the milliseconds are not. Re-running the
same binary on the same machine a few hours later, with something else on the
box, moved every absolute figure up by about 20% and left all seven ratios
identical to three significant figures. Report the ratio column; treat the
times as "on this machine, that afternoon".

One row is sensitive to tiering and the rest are not. V2 to V7 measure the same
with `DOTNET_TieredCompilation=0` and with tiering on, because their guard
chains are short and unconditional enough that there is nothing for dynamic PGO
to do. V1, the real runtime, is 5.2x with tiering and PGO and about 5.9x
without: its fallback branches are what PGO lays out. The gated
`bench/csharp-parity` figure for this kernel sits between the two, which is
what you would expect of a kernel that reaches its tiered state partway
through. Quote the mode alongside any V1 number.

Read the bottom of the table first. V5 and V6 land **on** the ceiling, so the
raw `long[]` slot storage carries no penalty at all and the whole of the 5.21x
is guard: the layout indirection is 39% of it, the `SlotCount` test 13%, the
null and bounds tests on the raw array 14%. A reader who stops at the first row
concludes that the raw storage was a mistake, which is the opposite of what the
measurement says. This is rule 3 earning its place.

V8 was added when the compiler actually grew the hoist, and it is the row that
matters for anyone repeating this. V6 is not reachable, because the fetch can
legitimately fail and so every access needs a test and an arm for that; V8 is
the same shape with the arm, and it is the real floor. The compiler's output
measures 1.73x, between the two and below V8, because its loop scaffolding is
tighter than C#'s.

V8 also demonstrates a way to write this kind of variant wrong. Its arms first
called the throwing replicas above, and it read 1.06x -- the JIT prunes a
block that only throws into a cold path, the diamonds vanish with it, and the
number was for a shape nobody emits. An arm has to RETURN to model a real
fallback.

`struct-slots.lisp` is the same decomposition done from the Lisp side: the same
loop with one term added at a time, which is how you find out whether the cost
is in the thing you are studying or in the scaffolding around it. Here the
scaffolding-only variant compiled to a four-instruction inner loop against C#'s
eight, which removed the loop machinery from suspicion before any of the C#
work started.

## Turn the JIT profile off before you compare two binaries

`DOTCL_NO_JIT_PROFILE=1`, on every run of an A/B that launches separate
processes. The runtime records a JIT profile and the next run reads it, so each
launch changes the one after it, and two builds compared without this are not
being compared on equal terms.

How much it matters: the same binary, copied into two directories and run
alternately, measured 39.6 against 49.5, then 46.6 against 53.2, drifting from
39.6 to 71.9 over four rounds. With the variable set, the same control came
back 38.9 against 39.0 and 46.5 against 46.7 -- pairs agreeing to a few tenths
of a percent.

Which is the second rule of this harness, in a form worth stating on its own:
**run the control first.** Copy the unmodified binary into a second directory
and A/B it against itself. If that does not come back flat, nothing measured
afterwards means anything, and a 40% difference invented by the harness looks
exactly like a 40% difference in the code.

## Tiering will lie to you first

Run it with `DOTNET_TieredCompilation=0` when you want variants that can be
compared to each other. With tiering on, a variant's number depends on how much
ran before it, and in a file with several variants they do not all reach the
same state. The struct-slots decomposition above is unusable with tiering on --
one variant came out slower than the one that does strictly more work.

The same trap is waiting in `bench/csharp-parity`, and it is worse there
because that one is gated. See "A kernel measured alone is not the same kernel"
in `docs/profiling.md`: the struct-slots kernel reads 188 ms inside the full
`kernels.lisp` and 496 ms on its own, because it only tiers up after the five
kernels ahead of it have run. Cut one kernel out to iterate on it and you get a
number 2.6x too slow, and a phantom to chase.
