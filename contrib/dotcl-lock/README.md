# dotcl-lock

Experimental. Reads a lock file that lists which `.asd` file each system is
loaded from, and lets ASDF load two versions of one system in the same image:
the version the lock assigns to a package unit is defined, compiled and loaded
inside that unit, where its packages have their plain names.

    (require "dotcl-lock")
    (dotcl-lock:use-lock "dotcl-systems.lock")
    (asdf:load-system "app")

The format is described at the top of `dotcl-lock.lisp` and may change. The
lock can be written by hand, or by `resolve.lisp` beside it (a prototype, also
runs on SBCL), which reads every `.asd` under some directories, where one
system may appear at several versions, and chooses versions as Cargo does:
one per compatible range (1.x, 2.x, 0.3.x), and a unit only for the version
fewer systems use when two dependencies need different ones:

    (load "contrib/dotcl-lock/resolve.lisp")
    (dotcl-lock-resolve:resolve '("app") '("./" "/path/to/systems/")
                                :output "dotcl-systems.lock")

Ranges in `:version` dependencies are those of `asdf-version-range`, which
dotcl-lock loads, so ASDF checks the versions the lock chose.

ASDF itself is not modified: the contrib adds a system search function,
gives each unit its own copy of ASDF's registries and output translations,
and represents a system that lives in another unit by a proxy system. Unit names are upcased. A system the lock does not name is
loaded as before, with a `dotcl-lock:system-not-in-lock` style warning.
Outside every unit, a system the lock loads only in units is not found, even
when a source registry over the same files has a copy of it.

Code compiled in a unit always refers to that unit's packages, but names
looked up at run time (`find-package`, `read`, `intern`) and threads started
from it follow the unit of the caller. Call an entry point that does either
-- such as `swank:create-server` -- in the unit of its system:

    (dotcl-lock:call-in-system-unit "probe-a" (lambda () (probe-a:start 4005)))

`dotcl-lock:system-unit` names that unit (NIL outside every unit); a system the
lock loads in more than one unit is an error there.

## Without a lock file

`add-unit` puts the systems of one `.asd` file in a unit of their own and
leaves every other system as ASDF finds it (Quicklisp, a source registry),
loaded once outside every unit. Two versions of swank, each on its own port:

    (asdf:load-system "swank")       ; the version ASDF finds, global
    (require "dotcl-lock")
    (dotcl-lock:add-unit "swank@git" "/src/slime/swank.asd")
    (dotcl-lock:call-in-unit "swank@git"
      (lambda ()
        (asdf:load-system "swank")
        (uiop:symbol-call :swank :create-server :port 4006 :dont-close t)))
    (swank:create-server :port 4005 :dont-close t)

Code outside the unit is read before the unit's packages exist, so it names
them at run time (`uiop:symbol-call`, `find-symbol`) inside `call-in-unit`.
The unit owns the packages and modules that the `defpackage`,
`define-package`, `make-package`, `provide` and `:nicknames` forms under the
directory of the `.asd` spell; pass `:packages` when the library makes others
(a trailing `*` matches any suffix). Both versions can also be put in units
with two `add-unit` calls. `test/package-units-swank/` connects SLIME 2.31 and
the current SLIME to such an image at the same time.

What a unit does not separate: global variables of CL and of shared
libraries (`*debugger-hook*`, `*features*`, `*readtable*`), methods both
versions add to a shared generic function for a shared class, and the
process (current directory, environment, ports). An object made by one
version is not of the other version's types, so do not pass one across.
