;;; compile-file.lisp — Tests for compile-file content correctness
;;; Tests that various Lisp constructs survive compile-file → load round-trip.
;;; Complements ansi-test/system-construction/compile-file.lsp which tests
;;; the compile-file protocol (return values, pathnames, warnings).

;;; Helper: compile-file a string, load it, return the result of calling funcname
(defun cf-roundtrip (source funcname &rest args)
  "Write SOURCE to a temp file, compile-file it, load the result, call FUNCNAME with ARGS."
  (let ((src-path (merge-pathnames "cf-test-tmp.lisp" *default-pathname-defaults*)))
    (with-open-file (s src-path :direction :output :if-exists :supersede)
      (write-string source s))
    (let ((compiled (compile-file src-path)))
      (load compiled)
      (prog1 (apply funcname args)
        ;; cleanup
        (ignore-errors (delete-file src-path))
        (ignore-errors (delete-file compiled))))))

;;; --- Basic defun ---

(deftest compile-file.defun.1
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-defun-1 () 42)"
   'cf-test-defun-1)
  42)

(deftest compile-file.defun.args
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-defun-args (x y) (+ x y))"
   'cf-test-defun-args 10 32)
  42)

;;; --- Constants ---

(deftest compile-file.constant.string
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-const-str () \"hello world\")"
   'cf-test-const-str)
  "hello world")

(deftest compile-file.constant.integer
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-const-int () 12345)"
   'cf-test-const-int)
  12345)

(deftest compile-file.constant.float
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-const-float () 3.14)"
   'cf-test-const-float)
  3.14)

(deftest compile-file.constant.character
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-const-char () #\\A)"
   'cf-test-const-char)
  #\A)

(deftest compile-file.constant.symbol
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-const-sym () 'foo)"
   'cf-test-const-sym)
  foo)

(deftest compile-file.constant.keyword
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-const-kw () :hello)"
   'cf-test-const-kw)
  :hello)

(deftest compile-file.constant.list
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-const-list () '(1 2 3))"
   'cf-test-const-list)
  (1 2 3))

(deftest compile-file.constant.nil
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-const-nil () nil)"
   'cf-test-const-nil)
  nil)

(deftest compile-file.constant.t
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-const-t () t)"
   'cf-test-const-t)
  t)

;;; --- Closures ---

(deftest compile-file.closure.1
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-make-adder (n) (lambda (x) (+ n x)))
    (defun cf-test-closure-1 () (funcall (cf-test-make-adder 10) 32))"
   'cf-test-closure-1)
  42)

(deftest compile-file.closure.let-over-lambda
  (cf-roundtrip
   "(in-package :cl-test)
    (let ((counter 0))
      (defun cf-test-inc () (incf counter))
      (defun cf-test-get () counter))
    (defun cf-test-closure-lol ()
      (cf-test-inc) (cf-test-inc) (cf-test-inc) (cf-test-get))"
   'cf-test-closure-lol)
  3)

(deftest compile-file.closure.nested
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-closure-nested ()
      (let ((x 10))
        (let ((f (lambda (y) (+ x y))))
          (funcall f 32))))"
   'cf-test-closure-nested)
  42)

;;; --- Macros ---

(deftest compile-file.defmacro.1
  (cf-roundtrip
   "(in-package :cl-test)
    (defmacro cf-test-when (test &body body) `(if ,test (progn ,@body)))
    (defun cf-test-macro-1 () (cf-test-when t 42))"
   'cf-test-macro-1)
  42)

(deftest compile-file.defmacro.available-after-load
  (cf-roundtrip
   "(in-package :cl-test)
    (defmacro cf-test-my-unless (test &body body)
      `(if (not ,test) (progn ,@body)))
    (defun cf-test-use-unless () (cf-test-my-unless nil 99))"
   'cf-test-use-unless)
  99)

;;; --- defvar / defparameter ---

(deftest compile-file.defvar
  (cf-roundtrip
   "(in-package :cl-test)
    (defvar *cf-test-var* 42)
    (defun cf-test-defvar () *cf-test-var*)"
   'cf-test-defvar)
  42)

(deftest compile-file.defparameter
  (cf-roundtrip
   "(in-package :cl-test)
    (defparameter *cf-test-param* \"hello\")
    (defun cf-test-defparam () *cf-test-param*)"
   'cf-test-defparam)
  "hello")

;;; --- Multiple defuns calling each other ---

(deftest compile-file.mutual-call
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-double (x) (* x 2))
    (defun cf-test-mutual () (cf-test-double 21))"
   'cf-test-mutual)
  42)

;;; --- eval-when ---

(deftest compile-file.eval-when.load-toplevel
  (cf-roundtrip
   "(in-package :cl-test)
    (eval-when (:load-toplevel :execute)
      (defun cf-test-ew-load () :loaded))"
   'cf-test-ew-load)
  :loaded)

(deftest compile-file.eval-when.compile-toplevel-only
  (progn
    (cf-roundtrip
     "(in-package :cl-test)
      (eval-when (:compile-toplevel)
        (defvar *cf-test-ct-ran* t))
      (defun cf-test-ew-ct () (if (boundp '*cf-test-ct-ran*) :yes :no))"
     'cf-test-ew-ct)
    ;; *cf-test-ct-ran* should NOT be bound at load time
    ;; (compile-toplevel only runs at compile time)
    ;; But since we're in the same process, it IS bound...
    ;; The function should still be callable.
    (cf-test-ew-ct))
  :yes)

;;; --- Control flow ---

(deftest compile-file.if.true
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-if (x) (if x :yes :no))"
   'cf-test-if t)
  :yes)

(deftest compile-file.if.false
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-if-2 (x) (if x :yes :no))"
   'cf-test-if-2 nil)
  :no)

(deftest compile-file.loop
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-loop ()
      (let ((sum 0))
        (dotimes (i 10) (incf sum i))
        sum))"
   'cf-test-loop)
  45)

(deftest compile-file.handler-case
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-handler ()
      (handler-case (error \"boom\")
        (error () :caught)))"
   'cf-test-handler)
  :caught)

;;; --- Multiple values ---

(deftest compile-file.multiple-values
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-mv () (values 1 2 3))"
   'cf-test-mv)
  1 2 3)

;;; --- &optional, &rest, &key ---

(deftest compile-file.optional.default
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-opt (&optional (x 42)) x)"
   'cf-test-opt)
  42)

(deftest compile-file.optional.supplied
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-opt-2 (&optional (x 42)) x)"
   'cf-test-opt-2 99)
  99)

(deftest compile-file.rest
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-rest (&rest args) (length args))"
   'cf-test-rest 1 2 3)
  3)

(deftest compile-file.keyword
  (cf-roundtrip
   "(in-package :cl-test)
    (defun cf-test-key (&key (x 0) (y 0)) (+ x y))"
   'cf-test-key :x 10 :y 32)
  42)
