;;; COMPILE-FILE makes a top level DEFUN callable while the rest of the file is
;;; compiled, for a macro that calls a sibling function. A name that is not yet
;;; defined gets a stand-in, and the DEFUN is compiled for that purpose only
;;; when something calls it. Most such functions are never called at compile
;;; time, and compiling each one used to cost as much as compiling it for the
;;; fasl.

(defun %cfdd-compile (name lines)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l s)))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

;; A function no macro calls is not compiled during COMPILE-FILE: a macro in
;; its body is expanded for the fasl only, not a second time for an in-memory
;; definition. It is still not defined after COMPILE-FILE, and the fasl
;; defines it.
(defvar *cfdd-expansions* 0)
(defmacro cfdd-counted (x) (incf *cfdd-expansions*) x)

(deftest-compiled-only cf-deferred-early-defun.uncalled-not-compiled
  (let* ((fasl (%cfdd-compile "cfdd-a"
                              '("(defun cfdd-a-f (x) (cl-user::cfdd-counted (+ x 1)))")))
         (after-compile (list *cfdd-expansions* (fboundp 'cfdd-a-f))))
    (load fasl)
    (list after-compile (cfdd-a-f 10)))
  ((1 nil) 11))

;; A macro that calls a sibling function at expansion time gets a working
;; function: required, optional and keyword arguments, multiple values, a
;; setf function, and recursion through the name.
(deftest-compiled-only cf-deferred-early-defun.called-by-macro
  (let ((fasl (%cfdd-compile
               "cfdd-b"
               '("(defun cfdd-b-f (a &optional (b 2) &key (c 3)) (values (+ a b c) :second))"
                 "(defun cfdd-b-fact (n) (if (< n 2) 1 (* n (cfdd-b-fact (1- n)))))"
                 "(defvar *cfdd-b-cell* (list 0))"
                 "(defun (setf cfdd-b-first) (v cell) (setf (car cell) v))"
                 "(defmacro cfdd-b-m ()"
                 "  (let ((cell (list 0)))"
                 "    (setf (cfdd-b-first cell) :set)"
                 "    `'(,(multiple-value-list (cfdd-b-f 1)) ,(cfdd-b-f 1 1 :c 1)"
                 "       ,(cfdd-b-fact 5) ,(car cell))))"
                 "(defparameter *cfdd-b-seen* (cfdd-b-m))"))))
    (let ((after-compile (fboundp 'cfdd-b-f)))
      (load fasl)
      (list after-compile (symbol-value (intern "*CFDD-B-SEEN*")))))
  (nil ((6 :second) 3 120 :set)))

;; The function captured at compile time and called after the fasl is loaded
;; still answers, and calling it does not replace the loaded definition.
(defvar *cfdd-c-captured* nil)

(deftest-compiled-only cf-deferred-early-defun.called-after-load
  (let ((fasl (%cfdd-compile
               "cfdd-c"
               '("(defun cfdd-c-f () :early)"
                 "(eval-when (:compile-toplevel) (setf cl-user::*cfdd-c-captured* #'cfdd-c-f))"))))
    (load fasl)
    (list (funcall *cfdd-c-captured*) (eq (fdefinition 'cfdd-c-f) *cfdd-c-captured*)
          (cfdd-c-f)))
  (:early nil :early))
