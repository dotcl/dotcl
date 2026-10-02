# Editors

This page covers connecting Emacs to dotcl, with SLIME or with SLY. Both run a
server inside dotcl (swank for SLIME, slynk for SLY) that the editor talks to.
The Lisp side comes from Quicklisp, which dotcl bundles (see
[Libraries](libraries.md)). The first `ql:quickload` in a new Quicklisp home
installs the stock Quicklisp dist and dotcl's dist; the dotcl support for swank
and slynk comes from the latter, so nothing else needs to be set up.

Use SLIME or SLY in one Emacs, not both.

## Emacs with SLIME

### Install the helper

Quicklisp's `quicklisp-slime-helper` installs SLIME and writes an Emacs file,
`slime-helper.el`, that loads that same SLIME into Emacs, so the Emacs side and
the Lisp side are always the same version. In dotcl:

```lisp
(require "quicklisp")
(ql:quickload "quicklisp-slime-helper")
```

`slime-helper.el` is written to dotcl's Quicklisp home. This prints where that
is on your machine:

```lisp
(progn (require "quicklisp") ql:*quicklisp-home*)
```

It is `~/.local/share/dotcl/quicklisp/` on macOS and Linux (or
`$XDG_DATA_HOME/dotcl/quicklisp/` when that is set) and
`%APPDATA%\dotcl\quicklisp\` on Windows.

### Emacs configuration

Change the path in `load` to the Quicklisp home printed above:

```elisp
(setq quicklisp-slime-helper-dist "dotcl")
(load (expand-file-name "~/.local/share/dotcl/quicklisp/slime-helper.el"))
(setq slime-lisp-implementations '((dotcl ("dotcl" "repl"))))
```

`quicklisp-slime-helper-dist` tells the helper that SLIME was installed from
dotcl's dist; it has to be set before the `load`.

### Connecting

`M-x slime` starts dotcl and connects to it.

## Emacs with SLY

### The Lisp side

In dotcl, load slynk and start it:

```lisp
(require "quicklisp")
(ql:quickload "slynk")
(slynk:create-server :port 4006 :dont-close t)
```

To start it from a shell, put those forms in a file, say `~/start-slynk.lisp`,
and run:

```
dotcl --load ~/start-slynk.lisp repl
```

### Emacs configuration

Install SLY from [MELPA](https://melpa.org/#/getting-started):

```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

Then `M-x package-refresh-contents` and `M-x package-install RET sly RET`.

### Connecting

`M-x sly-connect`, host `127.0.0.1`, port `4006`.
