# asdf-version-range

Experimental. Lets a `:version` dependency name a range instead of only a
lowest version:

```lisp
(defsystem "app"
  :depends-on ((:version "foo" "1.2")                  ; 1.2 or later (ASDF's own meaning)
               (:version "bar" (:^ "1.2"))             ; 1.2 or later, below 2
               (:version "baz" (:>= "1.2" :< "1.5")))) ; every pair holds
```

    (require "asdf-version-range")      ; on other Lisps: load asdf-version-range.lisp after ASDF

The operators are `:^ :>= :> :<= :< :=`. `(:^ V)` allows the versions that
keep the leading components of V up to its first nonzero one, as Cargo's
caret does: `^1.2` is below 2, `^0.3.1` below 0.4. Missing components count
as 0, so "2" and "2.0" are the same version.

A plain string keeps ASDF's meaning, at least that version, with no upper
bound. An ASDF without this module accepts a range (ASDF compares a list as
no constraint) and loads whatever version it finds.

`dotcl-lock` loads this module, and the lock resolver
(`contrib/dotcl-lock/resolve.lisp`) uses the same functions, so the version
a lock picks is the one ASDF accepts when it loads.
