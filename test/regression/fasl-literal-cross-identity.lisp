;;; An object that occurs in two literals of one top level form is still one
;;; object after COMPILE-FILE and LOAD (CLHS 3.2.4.4). A macro that puts one
;;; subform into two quoted constants, `(list '(wrap ,x) ',x), gave two copies
;;; of the subform once compiled to a fasl, because each literal was built by
;;; code of its own. Try's IS relies on it: it matches the forms it captured
;;; explicitly against the ones it captured implicitly with EQ.
;;;
;;; The literals of a form are built at first use, in whatever order the code
;;; reaches them, so the tests also call the second site first.

(defvar *flci-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-flci-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %flci-compile-and-load (source name)
  (let ((lisp (concatenate 'string *flci-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string source s))
    (load (compile-file lisp))
    t))

(deftest-compiled-only flci-cross-literal-identity
  (progn
    (%flci-compile-and-load
     "(in-package :cl-user)
      (defmacro flci-m (x) `(list '(wrap ,x) ',x))
      (defmacro flci-twice (x) `(list ',x ',x))
      (defun flci-f () (flci-m (1+ 2)))
      (defun flci-g () (flci-twice (a b)))
      (defmacro flci-split (x) `(values (lambda () '(wrap ,x)) (lambda () ',x)))
      (defun flci-h () (flci-split #P\"flci\"))"
     "flci")
    (let ((f (funcall (intern "FLCI-F")))
          (g (funcall (intern "FLCI-G"))))
      (multiple-value-bind (wrap bare) (funcall (intern "FLCI-H"))
        ;; the bare site first: it builds the shared pathname
        (let ((b (funcall bare)) (w (funcall wrap)))
          (list (eq (second (first f)) (second f))
                (eq (first g) (second g))
                (eq b (second w))
                (pathnamep b))))))
  (t t t t))
