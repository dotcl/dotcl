# dotcl-repl

Terminal line editing, read character by character through System.Console:
left/right/home/end, backspace and delete, history on the up and down arrows,
completion through the `dotcl-repl:*completer*` hook, and display widths that
count wide CJK characters as two columns.

    (require "dotcl-repl")
    (dotcl-repl:readline "CL-USER> ")

`dotcl-repl:enable` installs it for the running REPL and `disable` puts the
plain reader back. No dependencies.
