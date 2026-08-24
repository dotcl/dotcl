;;; --asm has to mean the same thing wherever it appears on the command line.
;;;
;;; It was recognized at argv[0] only. Put any other flag ahead of it and the
;;; whole invocation fell through to the ordinary path, where --asm is not a
;;; known flag: the .sil that followed was LOADed as source and died with
;;;
;;;   compiler/cil-out.sil:1: DEFUN: package COMMON-LISP is locked; cannot redefine EQ
;;;
;;; which says nothing about argument order. The other half of the same bug:
;;; --asd-search-path was not handled inside the --asm path at all, so putting it
;;; AFTER --asm made it "LOAD: file not found: --asd-search-path".
;;;
;;; The behaviour under test is argument handling in main, so each case runs this
;;; same executable again. Two children, because two orders — the plain
;;; "--asm first, nothing in front" case is what the whole suite already runs on.

(defvar *cli-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *cli-core*
  (or (ignore-errors (namestring (truename "compiler/cil-out.sil")))
      "compiler/cil-out.sil"))

(defvar *cli-lib-dir*
  (let ((dir (concatenate 'string
                          (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR")
                                                  (dotcl:getenv "TEMP")
                                                  "/tmp"))
                          "/dotcl-cli-asd/")))
    (ensure-directories-exist dir)
    (with-open-file (s (concatenate 'string dir "cliasd.asd")
                       :direction :output :if-exists :supersede)
      (write-string "(defsystem \"cliasd\" :components ((:file \"cliasd\")))" s))
    (with-open-file (s (concatenate 'string dir "cliasd.lisp")
                       :direction :output :if-exists :supersede)
      (write-string "(defpackage :cliasd (:use :cl) (:export #:tag))
                     (in-package :cliasd)
                     (defun tag () :cli-asd-loaded)" s))
    dir))

(defun %cli-run (args) (dotcl:run-process *cli-exe* args))

;;; The reported break: a flag in front of --asm. This one goes all the way to
;;; ASDF, so it also pins that the directory really reaches *central-registry*
;;; from this position.

(deftest cli-asm-flag-order.search-path-before-asm
  (let* ((result (%cli-run (list "--asd-search-path" *cli-lib-dir*
                                 "--asm" *cli-core*
                                 "--eval" "(require \"asdf\")"
                                 "--eval" "(format t \"~&RESULT=~s~%\"
                                             (handler-case
                                                 (progn (funcall (intern \"LOAD-SYSTEM\" :asdf) \"cliasd\")
                                                        (funcall (intern \"TAG\" :cliasd)))
                                               (error (e) (list :err (princ-to-string e)))))"
                                 "--eval" "(dotcl:quit 0)")))
         (out (second result)))
    (list (first result) (and (search "RESULT=:CLI-ASD-LOADED" out) t)))
  (0 t))

;;; The other half: after --asm the flag must be consumed as a flag, not LOADed
;;; as a file. Kept cheap — no asdf — because what broke here was argument
;;; handling, and the child dying at all is the signal.

(deftest cli-asm-flag-order.search-path-after-asm-is-not-a-file
  (let* ((result (%cli-run (list "--asm" *cli-core*
                                 "--asd-search-path" *cli-lib-dir*
                                 "--eval" "(format t \"~&OK=~s~%\" (+ 1 2))"
                                 "--eval" "(dotcl:quit 0)")))
         (out (second result))
         (err (third result)))
    (list (first result)
          (and (search "OK=3" out) t)
          (and (search "file not found" err) t)))
  (0 t nil))
