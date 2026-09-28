# sil-shapes

A corpus of small forms, compiled through the SBCL host path, for the question
"did that compiler change alter what we emit, and where?". Nothing here runs:
the answer is the SIL, read before and after.

```sh
DOTCL_INPUTS="bench/sil-shapes/corpus.lisp" DOTCL_OUTPUT=/tmp/before.sil \
  ros run --load compiler/cil-compile.lisp
# ... make the change ...
DOTCL_INPUTS="bench/sil-shapes/corpus.lisp" DOTCL_OUTPUT=/tmp/after.sil \
  ros run --load compiler/cil-compile.lisp
join <(bash bench/sil-shapes/count-sil.sh /tmp/before.sil) \
     <(bash bench/sil-shapes/count-sil.sh /tmp/after.sil)
```

## Check that the compiler you think you measured is the one that ran

The driver above exits 0 when the Lisp half fails. `ros run --load` enters the
debugger, takes EOF from the closed stdin, and returns success, so a stray
paren in a compiler source leaves the previous `.sil` in place and every count
afterwards describes the old compiler. The symptom is that your clause appears
to do nothing, which is also what a clause that does nothing looks like.

Check the artifact, never the exit code:

```sh
make cross-compile && find compiler/cil-out.sil -newer compiler/cil-compiler.lisp \
  | grep -q . || echo "STALE: cil-out.sil is older than its sources"
```

For a before/after pair the check is cheaper still, because a stale run gives
itself away: the two `.sil` files come out byte-identical. If they differ, both
were written. If they do not, establish which of "nothing changed" and "nothing
ran" you are looking at before concluding anything -- compare the file
timestamps against the compile, or confirm the driver printed its
`dotcl-a2: ... -> ...` line, which it only does on success.

A regression test that asserts the NEW instruction is present is the same check
in another form, and a stronger one: it cannot pass against a stale artifact.

It sits between the two instruments that already exist. `bench/sil-oracle`
asks whether a refactor changed the output at all and wants a byte-identical
answer. `bench/il-parity` asks what the gap to C# is made of, on six realistic
data structures, and gates on it. This one asks which *shapes* a change
reaches, on forms chosen to be one shape each, and gates on nothing -- it is
for the middle of an investigation, when you need to know whether a clause
fired before you can say anything about whether it helped.

## Record the base AND the log, not just the base

A number here means nothing without the commit it was taken against, because
the base moves under you: the same clause measured +5 instructions on one base
and -1 on the next, after an unrelated change landed in between. Both were
right for the tree they were taken on.

Recording the base is necessary and not sufficient. A number that carries its
base looks exactly like every other number that carries its base, whether or
not anyone ran anything. That is not hypothetical: a test total was written
into a commit message as a measured result, with the base named, when it had
never been run -- two people had independently predicted it from arithmetic,
agreed, and the agreement felt like corroboration. It was not. The arithmetic
turned out to be right, which is what made it dangerous; a prediction that is
probably right is the kind that gets promoted to a fact.

So cite the log file beside the base. A number with a base and no log is
indistinguishable from a number with a base and no measurement.

## Two things it will not tell you, both learned the hard way

**An instruction count cannot see a specialisation.** A change that swaps
`Runtime.Multiply` for `Runtime.MultiplyFixnum` leaves the count exactly where
it was, and this harness reports "no change" about a real improvement. It
happened: `(* 2 (length v))` was read as untouched when its call had in fact
been specialised. Diff the operand names too, not only the totals, and treat
an unchanged count as "look closer" rather than as an answer.

**An instruction count going UP is not a regression by itself.** Replacing a
runtime call with the work it was doing makes the count rise while the work
falls -- `(1- (length v))` went from two calls to a call plus four
instructions, and lost a type test and an overflow test in the process. Read
what the call did before concluding anything. `bench/il-parity/check-counts.sh`
has the same warning in its header, for the same reason.

## Writing forms for it

**Leave the declarations off.** A type proof that only fires on undeclared
code is invisible in declared code, and most of the interesting gaps are that
way round: the compiler's `THE` handling hands back the full int64 range for
`(THE FIXNUM E)` whatever E is, so wrapping a form in a declaration can mask
exactly the thing under test. Some of the forms below would answer the
opposite question if a declaration were added to them, and one of them --
`(1- (LENGTH V))` -- is genuinely slower with the declaration than without.

One shape per function, named for the shape. The point of a corpus this small
is that a row moving tells you which shape moved.
