;;; A fasl literal that travels as its printed representation (circular or
;;; shared structure) is read back the first time the code holding it runs.
;;; That read must not see the caller's reader variables: under *READ-SUPPRESS*
;;; T it returned NIL, and the site kept NIL from then on (a reader macro
;;; function first called from inside #+(or) lost its literal for good); under
;;; another *READ-BASE* its integers changed value.

(defvar *flrv-dir*
  (let ((dir (concatenate 'string
                          (regression-temp-dir)
                          "/dotcl-flrv-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %flrv-compile-and-load (source name)
  "Write SOURCE (a string of Lisp text) to a file, compile it, load the fasl."
  (let ((lisp (concatenate 'string *flrv-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede
                            :external-format :utf-8)
      (write-string source s))
    (load (compile-file lisp))
    t))

;;; ---- first run under *READ-SUPPRESS* T, and the site keeps the value ----

(deftest-compiled-only flrv-read-suppress
  (progn
    (%flrv-compile-and-load
     "(in-package :cl-user)
      (defun %flrv-rs () (car '#1=(end-of-file 10 . #1#)))"
     "flrv-rs")
    (let ((f (intern "%FLRV-RS")))
      (list (let ((*read-suppress* t)) (funcall f))
            (funcall f))))
  (end-of-file end-of-file))

;;; ---- first run under another *READ-BASE* ----

(deftest-compiled-only flrv-read-base
  (progn
    (%flrv-compile-and-load
     "(in-package :cl-user)
      (defun %flrv-rb () (cadr '#1=(:a 10 . #1#)))"
     "flrv-rb")
    (let ((f (intern "%FLRV-RB")))
      (list (let ((*read-base* 16)) (funcall f))
            (funcall f))))
  (10 10))
