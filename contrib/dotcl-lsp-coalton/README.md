# dotcl-lsp-coalton

Coalton for `dotcl-lsp`: type errors as diagnostics, and the type of a global
Coalton value on hover. Load Coalton first:

    dotcl --eval '(require "asdf")' \
          --eval '(let ((*standard-output* *error-output*)) (asdf:load-system "coalton"))' \
          --eval '(require "dotcl-lsp-coalton")' \
          --eval '(dotcl:quit (dotcl-lsp:serve-stdio))'

Each `--eval` is read after the previous one ran, so the packages it names
exist by then. Standard output is the protocol's, so the load prints to
standard error.

A `.coal` document (or language id `coalton`), and a `.lisp` document that
contains `coalton-toplevel`, are checked from the text the editor sent; nothing
is written or loaded. In a `.lisp` document, `defpackage`, `in-package` and
`in-readtable` forms are evaluated so the rest reads as it would in a file, and
each `coalton-toplevel` is checked as it is read (the document needs
`(named-readtables:in-readtable coalton:coalton)`). Nothing else is evaluated.
The global Coalton environment is rebound around the check and put back, so the
document's definitions are not left in the image; the environment the check
ended with is kept for hover, which therefore knows the document's own
functions.

A `.coal` document is checked as a whole, so after an error hover uses the
environment of the last check that succeeded. The type of an expression under
the cursor is not shown yet.

Without Coalton loaded the contrib loads, but declines every document.
