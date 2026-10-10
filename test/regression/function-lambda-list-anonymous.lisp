;;; DOTCL:FUNCTION-LAMBDA-LIST of a function that has no name. Only a DEFUN (and
;;; a DEFMACRO) recorded one, so a LAMBDA answered (NIL NIL) unless the
;;; interpreter happened to build it. SBCL answers for every function, closures,
;;; local functions and FORMATTER functions included, and libraries that wrap
;;; this (millet's LAMBDA-LIST) test exactly those.
;;;
;;; Everything here except the COMPILE and COMPILE-FILE tests runs on every
;;; build: under the interpreter the same answers come from what the evaluator
;;; keeps for its closures, and the two have to agree.

(defun %fla (fn) (multiple-value-list (dotcl:function-lambda-list fn)))

(deftest function-lambda-list-anonymous.lambda
  (%fla (lambda (x &optional y) (list x y)))
  ((x &optional y) t))

(deftest function-lambda-list-anonymous.function-of-lambda
  (%fla #'(lambda (a &key (b 1 bp)) (list a b bp)))
  ((a &key (b 1 bp)) t))

(deftest function-lambda-list-anonymous.required-only
  (%fla (lambda (a b) (list a b)))
  ((a b) t))

(deftest function-lambda-list-anonymous.no-parameters
  (%fla (lambda () :v))
  (nil t))

;;; A closure is made again on every evaluation; the list comes from the code it
;;; runs, so every closure of one site answers the same.
(deftest function-lambda-list-anonymous.closure
  (let ((fns (loop for k from 1 to 2
                   collect (let ((k k)) (lambda (z &rest more) (list* z k more))))))
    (mapcar #'%fla fns))
  (((z &rest more) t) ((z &rest more) t)))

(deftest function-lambda-list-anonymous.local-function
  (flet ((loc (a b) (list a b))) (%fla #'loc))
  ((a b) t))

(deftest function-lambda-list-anonymous.coerce-lambda-form
  (%fla (coerce '(lambda (a) a) 'function))
  ((a) t))

;;; The variable names are the user's symbols, not uninterned copies: a tool
;;; prints them, and millet compares the list with EQUAL.
(deftest function-lambda-list-anonymous.names-are-the-users-symbols
  (equal (dotcl:function-lambda-list (lambda (fla-p fla-q) (list fla-p fla-q)))
         '(fla-p fla-q))
  t)

;;; A FORMATTER function takes a stream and then the format arguments.
(deftest function-lambda-list-anonymous.formatter
  (let ((ll (dotcl:function-lambda-list (formatter "~A"))))
    (list (car ll) (and (member '&rest ll) t)))
  (stream t))

;;; The interpreter's closure is itself a compiled closure that takes
;;; (&REST args); the user's lambda list must win over the one of the code that
;;; runs it.
(deftest function-lambda-list-anonymous.interpreted-closure-keeps-users-list
  (let ((dotcl:*evaluator-mode* :interpret))
    (%fla (eval '(let ((c 1)) (lambda (i &optional (j c)) (list i j))))))
  ((i &optional (j c)) t))

(when (emitting-mode-p)
  (deftest function-lambda-list-anonymous.compile-nil
    (%fla (compile nil '(lambda (p &rest r) (list p r))))
    ((p &rest r) t)))

;;; From a FASL: the list rides on the method the function's code is in, read
;;; back in the package that was current when the file was compiled.
(defpackage :fla-pkg (:use :cl))

(when (emitting-mode-p)
  (deftest function-lambda-list-anonymous.from-fasl
    (let ((src "fla-tmp.lisp")
          (fasl "fla-tmp.fasl"))
      (unwind-protect
           (progn
             (with-open-file (s src :direction :output :if-exists :supersede)
               (write-line "(in-package :fla-pkg)" s)
               (write-line "(defparameter *plain* (lambda (m &optional n) (list m n)))" s)
               (write-line "(defparameter *direct* (lambda (m) m))" s)
               (write-line "(defparameter *closure* (let ((q 2)) (lambda (m &key (n q)) (list m n))))" s))
             (compile-file src :output-file fasl)
             (load fasl)
             (mapcar (lambda (var)
                       (let ((ll (dotcl:function-lambda-list
                                  (symbol-value (find-symbol var :fla-pkg)))))
                         (list (prin1-to-string ll)
                               (package-name (symbol-package (car ll))))))
                     '("*PLAIN*" "*DIRECT*" "*CLOSURE*")))
        (ignore-errors (delete-file src))
        (ignore-errors (delete-file fasl))))
    (("(FLA-PKG::M &OPTIONAL FLA-PKG::N)" "FLA-PKG")
     ("(FLA-PKG::M)" "FLA-PKG")
     ("(FLA-PKG::M &KEY (FLA-PKG::N FLA-PKG::Q))" "FLA-PKG"))))
