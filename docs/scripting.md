# Writing scripts

A file of Lisp, run and then done:

```console
$ dotcl hello.lisp
```

```lisp
;;; hello.lisp
(format t "~&hello~%")
```

dotcl exits when the file has been read, so a script does not need to say so.

## Arguments

`dotcl:script-arguments` is the list of what came after the file name:

```console
$ dotcl greet.lisp world twice
```

```lisp
;;; greet.lisp
(destructuring-bind (&optional (who "someone") (times "1")) (dotcl:script-arguments)
  (dotimes (i (parse-integer times))
    (format t "~&hello ~a~%" who)))
```

The arguments are strings, in order, with nothing else in the list -- no program
name, no flags belonging to dotcl itself. Outside a script (a REPL, `--eval`,
`--load`) it is empty.

Anything after the file name belongs to the script, dashes included, so a script
can take `--verbose` without dotcl reading it first:

```console
$ dotcl build.lisp --verbose out/
```

`dotcl:command-line-arguments` is a different thing and is easy to reach for by
mistake. It is shaped the way `uiop:command-line-arguments` expects to find it --
`("dotcl" "--" "world" "twice")` -- so a script's own arguments are what follows
the `--`, and reading the first element answers `"dotcl"`. Use it when handing
the whole command line to code that expects the uiop shape; use
`dotcl:script-arguments` for everything else.

## Exit code

Falling off the end of the file exits 0. `dotcl:quit` sets the code:

```lisp
(handler-case (do-the-work)
  (error (condition)
    (format *error-output* "~&~a~%" condition)
    (dotcl:quit 1)))
```

An unhandled error also exits non-zero, after printing the condition, so a script
that does nothing about failure still reports it to the shell.

## Running a script directly

A `#!` line on the first line is ignored, so a script can be executable on
Unix-like systems:

```lisp
#!/usr/bin/env dotcl
(format t "~&~a~%" (dotcl:script-arguments))
```

```console
$ chmod +x greet.lisp
$ ./greet.lisp world
```

## `--load` and `--eval`

`--load` and `--eval` do their work and exit, the same as a script does, and they
can be repeated and mixed -- they run in the order given:

```console
$ dotcl --load setup.lisp --eval '(run-report)'
```

The difference from a positional file is what the program is: with a positional
file the script is the program and the rest of the line is its arguments, while
`--load` and `--eval` are instructions to dotcl. A `--load`ed file therefore has
no `script-arguments` of its own.

That is also how to run something and then look around, since `repl` after a
positional file would be one of the file's arguments:

```console
$ dotcl --load app.lisp repl
```

## Speed

A script pays for reading and compiling itself every run. For something run
often, compile it once with `compile-file` and load the fasl, or package the
whole system as a tool -- see [Packaging an app](dotcl-pack.md).
