# asdf-parallel

Builds an ASDF system with several files compiling at once.

```lisp
(require "asdf-parallel")
(asdf-parallel:load-system "ironclad" :jobs 4)
```

or, through ASDF itself:

```lisp
(asdf:load-system "ironclad" :plan-class 'asdf-parallel:parallel-plan
                             :plan-options '(:jobs 4))
```

ASDF makes its usual plan. The compile-op of each Lisp source file goes to a
worker as soon as everything it depends on is done; every other action (the
loads above all) runs in the calling thread, in plan order, once its own
dependencies are done. How much this gains depends on the shape of the
dependency graph: a system whose files mostly depend on a common core
(ironclad) gains the most, a `:serial t` system nothing.

Workers are threads on dotcl. On SBCL, whose compiler does not run on two
threads at once, they are forked processes; load the source file after ASDF
there. `asdf-parallel:*executor*` chooses (`:thread`, `:fork`, `:serial`).

A file that compiles only because something its system does not depend on was
loaded before it in the serial order fails in a worker. It is then compiled
again in the calling thread at the point a serial build compiles it, and
`asdf-parallel:*statistics*` lists it under `:retried`. The same kind of
undeclared dependency can also compile without an error but differently, for
example a call to a function that a serial build would have inlined.

`*worker-specials*` lists the special variables a thread worker takes from the
calling thread. Handlers established around the build are not seen by the
workers.
