# The REPL

What you meet at the `CL-USER>` prompt: how to get one, what edits your
input, what happens when a form signals, and what is not there yet.

The keys and the commands themselves are documented where they are
implemented, in [`contrib/dotcl-repl/README.md`](../contrib/dotcl-repl/README.md).
This page is the REPL around them.

## Starting one

```
dotcl repl
```

Entering the REPL is something you say. dotcl does not fall into one when it
cannot make sense of its arguments, because an invocation that was not
understood should not look like a successful start:

```
$ dotcl --evla '(+ 1 2)'
dotcl: unknown option '--evla'
  dotcl --help for usage
$ echo $?
2
```

Run with no arguments at all, dotcl lists what it can do and exits 2 as well.
The exception is standard input that is not a terminal: `echo '(print 1)' |
dotcl` runs what it reads as a script (see [Writing scripts](scripting.md)),
while `echo ... | dotcl repl` is still a REPL, reading its input from the pipe.

`repl` also composes, and then it means "stay":

```
dotcl --load setup.lisp repl        Load a file, then leave me at a prompt
dotcl --eval '(require "asdf")' repl
```

Without the trailing `repl`, `--load` and `--eval` run and exit. The `repl`
token has to come before a positional script file, since everything after
that file is the script's own arguments.

## The prompt

The prompt is the current package and `> `. The name shown is the shortest of
the package name and its nicknames, so `COMMON-LISP-USER` appears as
`CL-USER>` and a package nicknamed `app` as `app>`.

A form that is not finished gets a continuation prompt of the same width, made
of blanks, so what you type stays in one column.

Ctrl+D on an empty prompt leaves. Once something is typed it does nothing, as
in readline, so a stray Ctrl+D does not throw away a half-written form; Ctrl+C
is the key that drops what you typed. Without the line editor, a Windows
console reads Ctrl+D as an ordinary character: there the end of input is
Ctrl+Z then Enter, and the banner says which of the two applies.

### Colour

On a terminal the REPL paints what it writes itself, so each kind of text can
be told from the others at a glance:

| Text | Colour |
| --- | --- |
| The package name in the prompt | bold green |
| The debugger's depth in its prompt (`0]`) | bold red |
| The `sh>` prompt of a shell line | bold magenta |
| The `cmd>` prompt of a comma command | bold cyan |
| The value a form returned | cyan |
| A warning | yellow |
| An error, and the debugger's report of one | red |
| The marked row of the debugger's restart menu | reverse video |
| The bracket that the one before the cursor closes | reverse video |
| A string in the line you are typing | green |
| A comment in the line you are typing | faint (the terminal's own text colour, dimmed) |
| A keyword in the line you are typing | magenta |

What your program prints is left in the terminal's own colour. It is yours,
and it may carry colour of its own.

The colours in the line you are typing can be turned off on their own by
putting these two lines in the init file. The matching bracket stays.

    (require "dotcl-repl")
    (setf dotcl-repl:*syntax-highlight* nil)

`--color=auto` is the default and paints a stream only when it is a terminal,
so a log or a pipe never gets escape sequences in it; standard output and
standard error are decided separately. `--color=always` paints even a pipe,
and `--color=never` paints nothing. Two environment variables turn colour off
whatever the flag says: `NO_COLOR` set to anything but the empty string (see
[no-color.org](https://no-color.org/)), and `TERM=dumb`.

The colours themselves can be changed with the `DOTCL_COLORS` environment
variable, written like `GCC_COLORS`: `role=params` pairs separated by colons,
where `params` is what goes between `ESC [` and `m`. The roles and their
defaults:

    DOTCL_COLORS='prompt=1;32:debugger=1;31:shell=1;35:command=1;36:result=36:warning=33:error=31:selected=7:location=1:match=7:string=32:comment=2:keyword=35'

Only the roles you name change. An empty value (`comment=`) leaves that role
unpainted. A role that is not in the list, or a value with anything but digits
and semicolons, is skipped, and the REPL starts as usual. `DOTCL_COLORS` picks
the colours only: whether to paint at all is still up to `--color`,
`NO_COLOR` and `TERM`. For example, to show comments in dim italic and strings
in yellow:

    export DOTCL_COLORS='comment=2;3:string=33'

The same form can be given from the init file:

    (require "dotcl-repl")
    (dotcl-repl:set-colors "comment=2;3:string=33")

On Windows a console is painted when virtual terminal processing can be turned
on for it, which it can on Windows 10 and later. A terminal that talks to
dotcl through pipes rather than a console, as mintty does, counts as a pipe:
use `--color=always` there. `DOTCL_NO_VT=1` leaves the console mode alone, and
then `auto` does not paint.

### What the last form left behind

The value a form returned is not only printed, it is kept. `*` is that value,
`**` the one before it and `***` the one before that; `/` is the whole list of
values the form returned, with `//` and `///` behind it; and `+` is the form
itself, with `++` and `+++`, while `-` is the form being evaluated now.

```
CL-USER> (+ 1 2)
3
CL-USER> (list * + / -)
(3 (+ 1 2) (3) (LIST * + / -))
```

`/` is where several values go: after `(values 1 2)` it is `(1 2)` and `*` is
`1`, and after `(values)` it is empty and `*` is `NIL`. A form that exits
without returning leaves all of them alone, so an error does not cost you the
value you were about to reach for. The debugger's prompt keeps the same
variables, which is how something worked out down there is still in `*` after
the restart.

## Line editing

On a console, dotcl loads the `dotcl-repl` contrib at startup and reads
through it: arrow keys, history, multi-line editing of a single form, the
comma commands, and TAB. When standard input or standard output is
redirected it is not loaded, because reading single keypresses has no meaning
for a pipe and the editor's redrawing has none in a file:

```
echo '(+ 1 2)' | dotcl repl        Plain line input, no editor
```

Nor is it loaded in a terminal that says it takes no escape sequences,
`TERM=dumb`, which is what Emacs sets for `M-x shell` and other comint
buffers. There the editor would fill the buffer with cursor movements, and
the buffer already edits the line itself. The REPL reads plain lines, writes
no colour and no escape sequence at all, and the debugger prints its restarts
as a numbered list. On Windows the same holds for a console on which virtual
terminal processing could not be turned on (or `DOTCL_NO_VT=1`).

Two flags override that choice. `--readline` forces the editor on,
`--no-readline` forces it off. `TERM=dumb` wins over `--readline`, as it does
over `--color=always`. A user init file that turns the editor on is also
overridden there: it serves every terminal the REPL starts in. If the editor cannot start, or fails
mid-session on a terminal that cannot support it, dotcl says so in one line on
standard error and continues with plain line input rather than ending the
session. Plain line input on a Windows console ends with Ctrl+Z then Enter
rather than Ctrl+D. What editing it has there (arrows, and F7 for a history
list) is the console's own, not dotcl's.

You do not need `rlwrap`. What it exists to provide is what the contrib is.
See the contrib README for what `rlwrap` still has that this does not, and
what turning the editor off to reach it costs.

### Multi-line forms

Enter asks whether the form is finished before sending anything, so a
definition is edited as one form and comes back out of the history as the
lines it was written on. Alt+Enter sends whatever the brackets say.

With the cursor just after a closing bracket, the opening bracket it closes is
shown in reverse video, on the same line or on a line above. It is found by the
same count Enter uses, so a bracket in a string, a comment or `#\(` is not
matched. Like the rest of the colour, it is off for `--color=never`,
`NO_COLOR` and `TERM=dumb`.

### History

What you type is kept between sessions, in a file beside the init file:

| Platform | Path |
| --- | --- |
| Windows | `%APPDATA%\dotcl\history` |
| Unix, macOS | `~/.config/dotcl/history` |

It is there rather than in the cache tree because `dotcl clean` empties the
cache tree, and a history a maintenance command is allowed to eat is not a
history. Each line is written as it is accepted rather than at exit, so two
REPLs open at once both keep what they typed and a session that is killed
keeps its history too; two lines accepted in the same instant in two terminals
can still lose one of them, which is the price of never rewriting the file. A
read trims the file back to the newest 500 entries,
the number the editor holds in memory, once it has grown past twice that.
`DOTCL_NO_HISTORY=1` turns the file off and `dotcl-repl:*history-file*` moves
it.

Ctrl+R searches the history backwards as you type, and Ctrl+R again steps back
to the next match.

### Commands

A line whose first character is a comma is an instruction to the REPL rather
than a form: `,help` lists them, `,cd` changes directory, `,time` evaluates
and reports the cost, `,mx` expands a macro call. `define-command` adds your
own, and one added that way is listed by `,help` and completed by TAB like any
other.

A `,` typed at the start of an empty line does not go into the line: the
prompt turns into `cmd>` and the command names are offered in a menu under it,
each with the first line of what it does. Typing narrows the menu to the names
that start with what is typed. Up and Down (or Ctrl+P and Ctrl+N, Shift+TAB for
up) move the mark, TAB puts the marked name in the line, and Enter runs it; a
command that needs an argument gets its name and a blank instead, for the
argument to be typed after it. Once there is a blank in the line the menu goes
away and Enter sends the line as it is.

```
CL-USER> ,             (the prompt becomes cmd>, with the menu under it)
cmd> doc car
CAR as a function:
...
CL-USER>
```

Typed straight on, nothing changes: `,doc car` runs `,doc car`, a name typed
in full runs that name whatever is marked (so `,cd` with nothing after it
still goes home), and the history keeps the line as `,doc car`. Backspace on
the empty `cmd>` line goes back to Lisp, and so does Ctrl+C. Nothing is lost
by taking the comma: at the start of a line it could only be a reader error.

The prompt needs a terminal the menu can be drawn on. Where there is none
(the editor forced on with `--readline` while the output is redirected, or
`TERM=dumb`) the comma goes into the line and `,doc car` is sent as typed, which runs the same
command. With `--no-readline` or on a pipe there are no commands at all, as
before.

### A line of shell

A `;` typed at the start of an empty line does not go into the line: the
prompt turns into `sh>`, and the line typed there is run by the shell when you
press Enter. The prompt after it is the Lisp one again, so each shell line
starts with its own `;`. Backspace on the empty `sh>` line goes back without
running anything, and so does Ctrl+C.

```
CL-USER> ;             (the prompt becomes sh>)
sh> ls *.lisp
hello.lisp
CL-USER>
```

The shell is `$SHELL`, or `/bin/sh` when that is not set; on Windows it is
`%ComSpec%`, which is `cmd.exe` unless something changed it. It runs in the
REPL's directory, the one `,cd` moves, on the same terminal, so a command that
asks for input can have it, and Ctrl+C while it runs goes to the command. A
status other than zero is reported on the line after its output.

Nothing is lost by taking the `;`: in Lisp it would only start a comment. A
pasted block that opens with `;;;` stays Lisp, because a paste never switches
the mode. On a terminal that does not mark pastes, the first `;` switches and
the shell mode then drops the line, since it starts with `;` as well.

The mode is part of the line editor, so it is not there with `--no-readline`
or on a pipe. Shell lines are not kept in the history.

### TAB

TAB completes command names always. For symbols it asks `dotcl-lsp-api`, which
is loaded if it is available and left out if it is not, so TAB is inert for
symbols in a build without it. `dotcl-repl:*completer*` is the hook, and
setting it replaces both.

A single candidate is inserted. Several are extended as far as they agree, and
then offered as a menu under the input, each with its detail (a package, or a
.NET signature) beside it: Up and Down (or Ctrl+P and Ctrl+N, Shift+TAB for up)
move the mark, Enter or TAB puts the marked candidate in place of the word, and
Escape, Ctrl+C or Ctrl+G close the menu and leave the line as it is. Any other
key closes it and does what it always does, so typing on, Backspace and the
arrows along the line need no key to close the menu first. Ten rows show at a
time and a longer list scrolls. Where there is no menu (the editor forced on
with `--readline` where output is not a terminal, or an input taller than
the window leaves no room under it) the candidates are printed as a list
instead, up to twenty, and the prompt and input are drawn again below them.

## The init file

Before the prompt appears, dotcl loads a user init file if there is one:

| Platform | Path |
| --- | --- |
| Windows | `%APPDATA%\dotcl\init.lisp` |
| Unix, macOS | `~/.config/dotcl/init.lisp` |

On Unix and macOS, an absolute `XDG_CONFIG_HOME` replaces `~/.config`. Older
builds used `~/Library/Application Support/dotcl/init.lisp` on macOS; an init
file there is still loaded as long as there is none at the path above.

`(dotcl:user-init-file)` returns the absolute pathname on any platform, which
is what a library should use rather than building the path itself.

The init file is loaded for a REPL and for `--eval` / `--load`, and not for a
script file, which starts clean. `--no-init` skips it. An error while loading
it is reported with its source location and does not stop the REPL from
starting.

## When a form signals

`error`, `break` and `invoke-debugger` enter the debugger, which prints the
condition, the restarts it found, and a numbered prompt:

```
CL-USER> (error "boom")
; Debugger entered on SIMPLE-ERROR:
;   boom
;
; Available restarts:
;   0: [ABORT] Return to top level.
;
0]
```

A number invokes that restart. Besides the numbers the prompt takes `:bt` for
a backtrace, `:frame` / `:up` / `:down` (and `:frames`, below) to walk it, `:locals` and `:specials`
for what is bound in the selected frame, `:source` for where its function is defined, `:restarts`, `:abort`, `:continue`,
and any Lisp expression, which is evaluated and printed there. `:help` lists
them.

With the line editor on, the restarts are not printed as a list. They are a
menu under the prompt instead, with the first one marked:

```
0]
> 0: [ONE] First.
  1: [TWO] Second.
  2: [ABORT] Return to top level.
```

Up and Down (or Ctrl+P and Ctrl+N) move the mark and Enter invokes the marked
restart. Digits still choose by number: they appear after the prompt, mark the
restart they name, and Enter takes it. Any other key puts the menu away and
starts an ordinary edited line with that key in it, for `:bt` or an
expression; the menu is back under the next prompt. Escape or Ctrl+C puts it
away for good at this level and prints the numbered list, and `:restarts`
brings it back. When a restart is chosen the menu is erased and the prompt row
keeps the restart that was taken (`0] 1: [TWO] Second.`). A description wider
than the terminal is cut to fit, and the marked row is shown in reverse video
when colour is on. Where escape sequences would not reach a terminal that
obeys them (standard output redirected, `TERM=dumb`) there is no menu and the
list is printed as above.

`:frames` (or `:fr`) offers the backtrace the same way, with the selected
frame marked:

```
0] :frames
0]
> -->  0: (INNER 1)
       1: (OUTER 1)
```

The arrows, Home and End, or digits mark a frame, and Enter selects it as
`:frame N` would: the prompt row is left reading `0] :frame 1`, the frame's
call form and locals are printed under it, and `:locals`, `:specials`, `:up`
and `:down` go on from there. Escape, Ctrl+C or Ctrl+D closes the menu and
leaves the selection as it was. Any other key starts an ordinary line with it,
as at the restart menu. Ten frames show at a time and a longer backtrace
scrolls; a call form wider than the terminal is cut to fit. Without a menu
(no line editor, standard output redirected, `TERM=dumb`) `:frames` prints the
backtrace as `:bt` does.

`:source` (or `:src`) shows where the selected frame's function is defined,
with the lines around it:

```
0] :source
; OUTER: src/a.lisp:5
;     3 | (defun inner (x) (error "boom ~a" x))
;     4 | (defmethod meth ((x integer)) (inner x))
; --> 5 | (defun outer (y)
;     6 |   (let ((z (1+ y)))
;     7 |     (meth z)))
```

When that place is known, `:frame`, `:up`, `:down` and a choice from
`:frames` also print it under the call form (`;      source: src/a.lisp:5`);
when it is not, they print nothing more. The place is recorded when `load` or
`compile-file` reads a definition from a source file in this process, so a
function typed at the prompt, or loaded only from a `.fasl` or a `.sil`, has
none, and `:source` says so. The line is where the top-level form defining the
function starts, not the expression the frame stopped in. For a generic
function it is the last `defmethod` or `defgeneric` read, which need not be the
method in the frame. A frame knows its function by name only, so when several
packages define a function of that name the one the current package sees is
taken; if none is, `:source` lists them all. A file under the current
directory is shown relative to it, and the file and line are bold when colour
is on.

So does an error the runtime signals rather than your own code: a wrong type,
an undefined function, an index out of bounds, a division by zero, an
exception from a .NET method you called. The debugger opens before anything
unwinds, so the frames that signalled are still there for `:bt`:

```
CL-USER> (defun inner (x) (car x))
INNER
CL-USER> (defun outer (x) (inner x))
OUTER
CL-USER> (outer 1)
; Debugger entered on TYPE-ERROR:
;   CAR: not a list: 1
;
; Available restarts:
;   0: [ABORT] Return to top level.
;
0] :bt
; -->  0: (INNER 1)
;      1: (OUTER 1)
0]
```

The debugger comes last, after your handlers: a `handler-case` or a
`handler-bind` that handles the condition keeps it out of the debugger, and
one that declines runs first. The runtime establishes no restarts of its own
for these errors, so ABORT is usually the only one listed. An error in a form
typed at the debugger prompt prints one line there rather than opening a
second debugger.

Ctrl+C at the prompt drops what you have typed and gives you a fresh prompt;
it does not open the debugger, since there is no computation to inspect. While
a form is being evaluated, Ctrl+C interrupts it and enters the debugger, and
`:abort` (or the number of the ABORT restart) returns to the prompt. The ABORT
restart covers the whole iteration rather than the evaluation alone, so it is
there whichever of the two the interrupt arrives in.

In a session that is not interactive, the debugger does not choose a restart
on your behalf. It reports and signals instead, because the innermost ABORT
usually belongs to a library rather than to your program, and taking it would
let a failed script resume at the next form and still exit 0. A REPL whose
standard input is not a terminal counts as not interactive: `echo ... | dotcl
repl` reports the error, skips the rest of the input and exits 1.

## Not there yet

- **The debugger prompt has no line editing without the editor.** With
  `--no-readline` it reads a line at a time, so the arrow keys are escape
  sequences there: to leave it, give a restart number or `:abort`. On Windows,
  Ctrl+D there is an ordinary character, not the end of input.
- **Input taller than the window stops being redrawn.** The editor counts rows
  back from the cursor to find the prompt and never asks the terminal where the
  cursor is; once the prompt has scrolled off there are not that many rows to
  count. What is typed still echoes and the form still evaluates.
