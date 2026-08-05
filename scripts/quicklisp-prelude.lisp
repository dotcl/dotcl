;;;; dotcl prelude for the bundled quicklisp client. Emitted ahead of the
;;;; client's own components by scripts/build-quicklisp.sh.
;;;;
;;;; Upstream, ql-setup is defined by a setup.lisp that lives inside the
;;;; quicklisp home and pins *quicklisp-home* to its own location; the client is
;;;; then loaded from there. dotcl ships the client as a contrib fasl instead, so
;;;; there is no home-resident setup.lisp to define it. This file takes over that
;;;; role and picks the home from dotcl's own conventions.
;;;;
;;;; Everything here decides at load time, never at read time: the shipped fasl
;;;; is a single OS-agnostic IL assembly, so #+windows and friends must not
;;;; appear or the build host's platform would be baked into the artifact.

(defpackage #:ql-setup
  (:use #:cl)
  (:export #:*quicklisp-home*
           #:qmerge
           #:qenough
           ;; dotcl additions, defined in the postlude
           #:*https-credentials-function*
           #:https-fetch
           #:*dotcl-dist-url*
           #:*offer-dotcl-dist*
           #:dotcl-dist
           #:install-dotcl-dist))

(in-package #:ql-setup)

(defun %windows-p ()
  (and (member :windows *features*) t))

(defun %env-directory (name)
  "Directory pathname named by environment variable NAME, or NIL if unset."
  (let ((value (dotcl:getenv name)))
    (when (and value (plusp (length value)))
      (pathname (concatenate 'string (substitute #\/ #\\ value) "/")))))

(defun %default-quicklisp-home ()
  "Where dotcl keeps the quicklisp home when the user has not chosen one.

An existing ~/quicklisp/ wins, so a machine already set up by the stock
installer keeps working. Otherwise the home lives under dotcl's own per-user
directory rather than squatting on ~/quicklisp: %APPDATA%/dotcl/quicklisp/ on
Windows (the same root as dotcl:user-init-file), $XDG_DATA_HOME/dotcl/quicklisp/
on Unix — dists/ is bulk data rather than configuration, so XDG places it under
~/.local/share, not ~/.config."
  (let ((classic (merge-pathnames "quicklisp/" (user-homedir-pathname))))
    (if (probe-file classic)
        classic
        (merge-pathnames
         "dotcl/quicklisp/"
         (if (%windows-p)
             (or (%env-directory "APPDATA")
                 (merge-pathnames "AppData/Roaming/" (user-homedir-pathname)))
             (or (%env-directory "XDG_DATA_HOME")
                 (merge-pathnames ".local/share/" (user-homedir-pathname))))))))

(defvar *quicklisp-home* (%default-quicklisp-home))

(defun qmerge (pathname)
  "Return PATHNAME merged with the base Quicklisp directory."
  (merge-pathnames pathname *quicklisp-home*))

(defun qenough (pathname)
  (enough-namestring pathname *quicklisp-home*))
