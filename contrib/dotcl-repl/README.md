# dotcl-repl

Terminal line editing, read character by character through System.Console:
left/right/home/end, backspace and delete, word-wise motion and deletion,
history on the up and down arrows and on Ctrl+R, completion through the
`dotcl-repl:*completer*` hook, and display widths that count wide CJK
characters as two columns. A form that runs over several lines is edited as one
form, and pasting one pastes all of it. The history is kept in a file, so it is
there again the next time you start.

    (require "dotcl-repl")
    (dotcl-repl:readline "CL-USER> ")

`dotcl-repl:enable` installs it for the running REPL and `disable` puts the
plain reader back. No dependencies.

## rlwrap

You do not need it. `rlwrap` exists to give a line editor to a program that
has none, and this is one.

Running `rlwrap dotcl` anyway is harmless: `rlwrap` notices that dotcl reads
single keypresses, warns about it, and passes them through, so everything
below still works. `rlwrap -n dotcl` silences the warning. One artifact
remains either way - your first input line is echoed a second time underneath
itself, because `rlwrap` writes a line break that the redraw does not account
for, and the redraw deliberately never rewrites the prompt. It is cosmetic and
happens once per session.

What is left that `rlwrap` has and this editor has not is `~/.inputrc` and vi
mode, and reaching either means turning this editor off with `rlwrap dotcl
--no-readline`, which also turns off multi-line editing, the comma commands and
TAB completion, since all three are implemented here. History that survives the
session was on that list until it was implemented; it is below.

## Multi-line forms

A form is not a line, so Enter asks whether the form is finished before it
sends anything. Parentheses still open mean it is not: Enter opens a line
instead and indents it for you.

    CL-USER> (defun add1 (x)
               (+ x 1))
    ADD1

The indentation follows three rules and no table of operators, so a macro you
defined a moment ago is indented like everything else. The brackets still open
are counted from the start of the form, and the one opened last decides:

1. If something comes before it on its line, the new line lines up with its
   first argument, or one column past the bracket when the line has none.
2. If it is the first thing on its line, the new line goes two columns in from
   it.
3. If the line above closed everything it opened, the new line keeps that
   line's indentation.

For example:

    CL-USER> (let ((x 1)
                   (y 2))
               (+ x y))

    CL-USER> (format t "~a" (list 1
                                  2))

The clauses of a `cond` after the first one, the branches of an `if` and the
keywords of a `loop` go two columns in rather than under their neighbours. Start
the first clause on the next line and every clause lines up.

The indentation is real text in the buffer, so it
goes to the reader and comes back out of the history looking the way it went
in. The padding that lines the second line up under the first is not: that is
drawn on the screen and is not in the form.

| Key | What it does |
| --- | --- |
| Enter | Send a finished form, open a line inside an unfinished one |
| Alt+Enter | Send the form whatever its brackets say |
| Ctrl+J | Open a line whatever its brackets say |
| Up / Down | A line at a time inside the form, then the history |
| Home / End | The start and the end of the line the cursor is on |
| Backspace | At the start of a line, join it to the one above |
| Ctrl+R | Search the history backwards as you type |
| Alt+B / Alt+F | A token back, a token forward |
| Alt+D | Delete the token in front of the cursor |
| Ctrl+W | Delete back to the last blank |
| Ctrl+L | Clear the screen and redraw the line at the top of it |
| Ctrl+C | Drop the form and start a fresh prompt |
| Ctrl+D | On an empty prompt, leave the REPL; otherwise nothing |

The history holds whole forms, so a multi-line definition comes back on the up
arrow as the lines it was written on.

A token here is a Lisp token, so Alt+B over `*standard-output*` passes the
whole of it: a motion that stopped inside a symbol name would be of little use
where the hyphen is the space of ordinary names. Ctrl+W is the other notion,
readline's own, and takes everything back to the last blank, brackets and all,
because what it takes back is what you have just typed.

Some terminals keep Alt+Enter for themselves -- Windows Terminal toggles full
screen with it. Where that happens, a closing bracket too many also sends the
form, and the reader reports what is wrong with it.

Counting brackets is a scan and not the reader, which means it steps over
string literals, `;` comments, `#|` block comments including nested ones, and
`#\(` and `#\)`. It does not step over a bracket inside `|a vertical bar
symbol|`, and an unclosed string does not on its own make a form unfinished.
Alt+Enter is the way out of anything it gets wrong.

The same count shows which bracket a closing bracket closes: with the cursor
just after one, the opening bracket it closes is drawn in reverse video,
wherever it is in the form. It is part of the redraw, which writes the whole
form on every key anyway, so it asks the terminal nothing. It is shown only
when the REPL paints (not with `--color=never`, `NO_COLOR` or `TERM=dumb`), and
the line sent with Enter is left unpainted.

The redraw also colours strings (green), comments (grey) and keywords
(magenta) as you type, with the same scan: what it steps over as a string or a
comment is what it colours as one. The colours stay on the line after Enter.
`(setf dotcl-repl:*syntax-highlight* nil)` in the init file, after
`(require "dotcl-repl")`, turns them off and leaves the matching bracket.

A line whose first character is a comma is a command and is sent at once
without counting anything, so `,help (` does not leave you waiting for a
bracket that no command was going to read. On a continuation line the comma is
an unquote again and is counted like any other character.

### Pasting

The editor asks the terminal for bracketed paste, so text that is pasted is
known to be pasted: none of it is taken as a keystroke, no newline in it is
asked to decide anything, and a form of any length arrives and is read once.
The mode is turned off again whenever the editor is not running, because a
terminal that has been asked for the delimiters will send them, and a
delimiter nobody reads is the `[200~` that used to turn up in the line.

### One thing it does not do yet

Input taller than the window is not redrawn. The editor never asks the
terminal where the cursor is -- that question is answered through standard
input on Unix and the answer lands in whatever else is reading it -- so it
knows where the prompt is only by counting rows back from the cursor. Once the
input is taller than the window the prompt has scrolled off the top and there
are no longer that many rows to count back through. Rather than redraw the
wrong ones, the editor stops redrawing and lets what is typed echo where it is
typed. The form is still whole and still evaluates; it is only the picture of
it that stops being maintained, and a form that long belongs in a file.

## History

What you type is kept between sessions, in `%APPDATA%\dotcl\history` on
Windows and `$XDG_CONFIG_HOME/dotcl/history` (i.e. under `~/.config`)
elsewhere, which is the directory dotcl already keeps `init.lisp` in. It is
deliberately
not in the cache tree: `dotcl clean` empties that, and a history a maintenance
command is allowed to eat is not a history. Set `dotcl-repl:*history-file*` to
a pathname of your own to move it, to `nil` to turn it off, or set
`DOTCL_NO_HISTORY=1` in the environment, which does the same.

A line is written the moment it is accepted rather than at exit. Two REPLs open
at once then both keep everything they typed, in the order it was typed,
without either reading the other's file or overwriting it on the way out; a
session that is killed keeps its history as well. Two lines accepted in the
same instant in two terminals can still lose one of them, which is the price
of never rewriting the file; nothing else about it depends on timing.

Each entry is one line, with the newlines inside a multi-line form spelled out,
so a form comes back out of the history as the lines it went in on and a file
cut short by a crash loses only the line that was being written. The file grows
by appending and is trimmed back to the newest `dotcl-repl:*history-max*`
entries when it is read and has grown past twice that.

Ctrl+R searches it backwards as you type, and Ctrl+R again steps back to the
next match. Backspace undoes one keystroke of the search, whether that was a
character or a step back. Enter sends what was found, Escape keeps it in the
line for editing, and Ctrl+G leaves the line as it was.

## Commands

A line whose first character is a comma is an instruction to the REPL rather
than a form to evaluate. `,help` lists them and `,help <command>` explains one.

    CL-USER> ,help cd

The mechanism, the names and the aliases come from
[icl](https://github.com/atgreen/icl) (MIT licensed), kept the same so that
what a reader already knows carries over. The one divergence is `,cd`: icl
changes package with it, and here that is `,in-package`, so `,cd` is free to
mean what it means in a shell.

| Command | Aliases | Argument | What it does |
| --- | --- | --- | --- |
| `,help` | `,h` `,?` | `[command]` | List the commands, or explain one |
| `,quit` | `,exit` `,q` | | Leave the REPL |
| `,clear` | | | Clear the screen |
| `,history` | | | Print what the up arrow remembers |
| `,in-package` | `,pkg` | `<package>` | Change the current package |
| `,pwd` | | | Print the current package and directory |
| `,cd` | | `[directory]` | Change directory, process and Lisp alike |
| `,doc` | `,d` | `<symbol>` | Documentation a symbol carries |
| `,describe` | `,desc` | `<symbol or form>` | Describe a symbol, or the value of a form |
| `,apropos` | `,ap` | `<pattern>` | Symbols whose names contain a string |
| `,args` | | `<symbol>` | How a function is called |
| `,mx` | | `<form>` | Expand a macro call once |
| `,mxa` | | `<form>` | Expand a form all the way down |
| `,time` | | `<form>` | Evaluate and report what it cost |
| `,load` | `,ld` | `<file>` | Load a file |
| `,ql` | | `<system>` | Load a system with quicklisp |
| `,trace` | | `<symbol>` | Trace a function |
| `,untrace` | | `[symbol]` | Stop tracing |
| `,dis` | | `<symbol>` | Disassemble a function |

The argument is whatever follows the name, undivided, so a form keeps its
spaces and a quoted file name keeps the space inside it:

    CL-USER> ,time (loop for i below 1000000 sum i)
    CL-USER> ,load "some file.lisp"

`,describe` takes a bare symbol as itself, the way icl takes a symbol name, and
evaluates anything else, so `,describe car` and `,describe 'car` both describe
the symbol CAR, while `,describe #'car` describes the function object and
`,describe (make-hash-table)` a new table.

A comma at the start of a continuation line is left alone, because that is
where an unquote in a multi-line backquoted form is written.

### The cmd> prompt

A `,` at the start of an empty line switches the prompt to `cmd>` for one line
and offers the command names in a menu under it. Typing narrows it, the arrows
move the mark, TAB puts the marked name in the line and Enter runs it (or, for
a command that needs an argument, puts its name and a blank in the line).
Typing a whole command straight on works as it always did: `cmd> doc car` runs
`,doc car`, and the history keeps it as `,doc car`. Backspace on the empty line
or Ctrl+C goes back to Lisp. Where no menu can be drawn the comma is not taken
and the line goes as typed. The mode is the second entry in the table of line
modes, with a `:menu` function (the commands matching the line) and a
`:choose` function (what Enter and TAB do with the marked one).

### A line of shell

A `;` at the start of an empty line switches the prompt to `sh>` for one line,
which the shell (`$SHELL` or `/bin/sh`; `%ComSpec%` or `cmd.exe` on Windows)
runs when Enter is pressed. Backspace on the empty line switches back. A paste
never switches, and the shell mode drops a line that starts with `;`, so a
pasted `;;;` comment stays harmless. `,help` lists the modes after the
commands; `define-line-mode` in the source is how a mode is added.

### Adding your own

`define-command` is exported, and a command defined with it is worth exactly as
much as one defined here: `,help` lists it, TAB completes it, and an error in
it is reported at the prompt rather than ending the session. In a user init
file, require the contrib first so that the package exists:

```lisp
(require "dotcl-repl")

(dotcl-repl:define-command ("ls" "dir") (argument "[directory]")
  "List a directory, or the current one."
  (dolist (entry (directory (merge-pathnames
                             "*.*"
                             (if (string= argument "") "./" argument))))
    (format t "~&~A~%" (enough-namestring entry))))
```

The names after the first are aliases, all matched without regard to case.
`argument` is bound to everything typed after the name, trimmed; the string
beside it is how `,help` spells that argument. The first line of the docstring
is what the listing shows and the whole of it is what `,help <command>` prints.

## Menus

`run-menu` draws a list of choices on the rows under the line being typed and
lets the arrows move a mark through it. It is what the debugger uses to offer
its restarts (see docs/repl.md), and it is written to serve other lists as
well: it takes the labels, the column the line ends at, and optionally
functions to read keys and write output, and returns `:choose` and an index,
`:cancel`, `:eof`, or `:other` with a key it did not use, having taken itself
off the screen in every case.

It draws with relative cursor movement only, like the editor. Rows are written
below the line with a line feed each, so at the bottom of the window the
terminal scrolls and the menu still lands under the line; every row is cut one
column short of the width, so none wraps; at most ten rows are shown and a
longer list scrolls to keep the mark in view. Escape, Ctrl+C and Ctrl+G
cancel. The marked row is shown in reverse video when the REPL paints (the
`--color` decision) and with a `>` in front of it either way.

TAB completion is the second user: with several candidates the editor moves
the cursor to the end of the input, anchors the menu there so the menu and the
erase never touch the text, and maps the keys through
`completion-menu-key` (TAB takes the mark, Shift+TAB goes up, a key a menu has
no use for comes back as `:other` and is handed to the line).

The debugger's `:frames` is the third: `frame-menu` offers the backtrace under
the debugger prompt with the selected frame marked, digits choosing by number
as `:frame N` does. It answers the frame index (the row is left reading
`:frame N`), `:cancel` for Escape, Ctrl+C or Ctrl+D, or the typed key for a
line, and takes the same `read-key` and `write` functions so it runs without a
terminal.

`menu-step`, `menu-window-top` and `render-menu` are the pure parts: what a
key does, which rows are shown, and the exact string written.
