;;; An invocation whose arguments were not understood must say so, not look like
;;; a normal start.
;;;
;;; Two ways that used to fail silently, both ending in the same place: the
;;; argument was dropped, no action was recorded, and the empty action list took
;;; the process into the REPL -- which reads exactly like a successful `dotcl`.
;;;
;;;   dotcl --evla '(princ 1)'   typo: the flag was discarded
;;;   dotcl --eval               value missing: the option was discarded
;;;
;;; The REPL is now entered only when asked for (`repl`), so an empty action list
;;; is itself reported rather than being a way in.
;;;
;;; Arguments after a script name belong to the script and are not options: that
;;; is checked here too, because the obvious implementation of "reject unknown
;;; options" would eat them.

(defvar *cli-ae-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *cli-ae-core*
  (or (ignore-errors (namestring (truename "compiler/cil-out.sil")))
      "compiler/cil-out.sil"))

(defun %cli-ae-run (args) (dotcl:run-process *cli-ae-exe* args))

(defun %cli-ae-core-args (&rest args)
  (append (list "--asm" *cli-ae-core*) args))

;;; --- unknown options -------------------------------------------------------

(deftest cli-argument-errors.unknown-long-option
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core* "--evla" "(princ 1)")))
         (err (third r)))
    (list (first r) (and (search "unknown option" err) t)))
  (2 t))

(deftest cli-argument-errors.unknown-short-option
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core* "-x")))
         (err (third r)))
    (list (first r) (and (search "unknown option" err) t)))
  (2 t))

;;; --- value-taking options with no value ------------------------------------

(deftest cli-argument-errors.eval-without-form
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core* "--eval")))
         (err (third r)))
    (list (first r) (and (search "requires an argument" err) t)))
  (2 t))

(deftest cli-argument-errors.load-without-file
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core* "--load")))
         (err (third r)))
    (list (first r) (and (search "requires an argument" err) t)))
  (2 t))

;;; --- no action at all ------------------------------------------------------

(deftest cli-argument-errors.no-arguments-reports-instead-of-repl
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core*)))
         (err (third r)))
    (list (first r) (and (search "nothing to do" err) t)))
  (2 t))

;;; --- what must keep working ------------------------------------------------

(deftest cli-argument-errors.eval-still-runs
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core* "--eval" "(princ :ok)"))))
    (list (first r) (and (search "OK" (second r)) t)))
  (0 t))

;;; Flags after a script name are the script's arguments, not dotcl's.
(deftest cli-argument-errors.script-arguments-pass-through
  (let* ((path (concatenate 'string
                            (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR")
                                                    (dotcl:getenv "TEMP")
                                                    "/tmp"))
                            "/dotcl-cli-ae.lisp")))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "(format t \"~&ARGS=~s~%\" (length (dotcl:command-line-arguments)))" s))
    (let ((r (%cli-ae-run (list "--core" *cli-ae-core* path "--foo" "-x"))))
      (list (first r) (and (search "ARGS=" (second r)) t))))
  (0 t))

;;; A subcommand's options are the subcommand's.
;;;
;;; `pack` removes its own "pack" token before parsing, so what was left for the
;;; general option scan began with `--system` and the scan rejected it as a
;;; dotcl option: every `dotcl pack` invocation exited 2 with "unknown option"
;;; before the subcommand ran. `build` was spared only by accident -- it leaves
;;; its token in place, so the scan stopped on that bare token immediately.
;;;
;;; The run below still fails: the system does not exist and neither does the
;;; donor directory. What it must not do is fail as a dotcl argument error. Only
;;; that is asserted -- which words pack chooses for its own failure are its
;;; business.

(defun %cli-ae-tmp (name)
  (concatenate 'string
               (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR")
                                       (dotcl:getenv "TEMP")
                                       "/tmp"))
               "/" name))

(deftest cli-argument-errors.pack-options-are-not-dotcl-options
  (let* ((r (%cli-ae-run (list "--core" *cli-ae-core* "pack"
                               "--system" "no-such-system"
                               "--id" "x"
                               "--command" "x"
                               "--version" "0.0.1"
                               "--from" (%cli-ae-tmp "dotcl-cli-ae-no-donor")
                               "--rids" "any"
                               "-o" (%cli-ae-tmp "dotcl-cli-ae-no-out"))))
         (out (second r))
         (err (third r)))
    (list (/= 0 (first r))
          (and (search "unknown option" err) t)
          (and (search "unknown option" out) t)))
  (t nil nil))

;;; A packed application's options are the application's.
;;;
;;; `dotcl pack` and save-application produce a launcher that runs a bundled
;;; dotcl.user.fasl instead of dotcl's own CLI, so every argument after the
;;; program name belongs to that application. The scan ran for it all the same
;;; and exited 2 on the first option dotcl does not know, which left a packed
;;; tool unable to take a --format or a --help of its own. Nothing the scan
;;; computes is read on that path -- the launcher returns as soon as the fasl has
;;; run -- so it is skipped there, as it already is under build and pack.
;;;
;;; The two cases below are the same directory run twice, once with the marker
;;; file and once without, so the "not rejected" case cannot pass by accident:
;;; the second case has to reproduce the old failure for the first to mean
;;; anything. HasUserFasl only asks whether dotcl.user.fasl exists, so an empty
;;; one is enough to take the packed path; the child then dies on the invalid
;;; image, which is the point -- it died past argument parsing.
;;;
;;; The copy carries the launcher and nothing else, and is run as
;;; `dotnet runtime.dll` rather than through the apphost so that no execute bit
;;; has to survive the copy. A child that gets as far as argument parsing is all
;;; the evidence there is to collect.

(defun %cli-ae-packed-dll (fasl-p)
  "Lay out a launcher-only copy of this runtime and return its .dll.
FASL-P decides whether a dotcl.user.fasl sits beside it."
  (let* ((exe (dotnet:static "System.Environment" "ProcessPath"))
         (src (dotnet:static "System.IO.Path" "GetDirectoryName" exe))
         (stem (dotnet:static "System.IO.Path" "GetFileNameWithoutExtension" exe))
         (dst (%cli-ae-tmp "dotcl-cli-ae-packed"))
         (stub (dotnet:static "System.IO.Path" "Combine" dst "dotcl.user.fasl")))
    (dotnet:static "System.IO.Directory" "CreateDirectory" dst)
    (dolist (name (list (dotnet:static "System.IO.Path" "GetFileName" exe)
                        (concatenate 'string stem ".dll")
                        (concatenate 'string stem ".deps.json")
                        (concatenate 'string stem ".runtimeconfig.json")
                        "DotCL.Runtime.dll"))
      (dotnet:static "System.IO.File" "Copy"
                     (dotnet:static "System.IO.Path" "Combine" src name)
                     (dotnet:static "System.IO.Path" "Combine" dst name)
                     t))
    (if fasl-p
        (dotnet:static "System.IO.File" "WriteAllBytes" stub
                       (make-array 0 :element-type '(unsigned-byte 8)))
        (when (dotnet:static "System.IO.File" "Exists" stub)
          (dotnet:static "System.IO.File" "Delete" stub)))
    (dotnet:static "System.IO.Path" "Combine" dst (concatenate 'string stem ".dll"))))

(deftest cli-argument-errors.packed-app-options-are-not-dotcl-options
  (let* ((r (dotcl:run-process "dotnet"
                               (list (%cli-ae-packed-dll t) "--format" "json")))
         (err (third r)))
    (list (and (search "unknown option" err) t) (= 2 (first r))))
  (nil nil))

;;; Same directory, marker removed: the launcher is dotcl again and the option
;;; is rejected, as it should be.
(deftest cli-argument-errors.plain-launcher-still-rejects-unknown-options
  (let* ((r (dotcl:run-process "dotnet"
                               (list (%cli-ae-packed-dll nil) "--format" "json")))
         (err (third r)))
    (list (and (search "unknown option" err) t) (first r)))
  (t 2))
