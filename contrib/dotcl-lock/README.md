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
