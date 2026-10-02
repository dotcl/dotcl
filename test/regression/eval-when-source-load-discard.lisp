;;; LOAD of a source file and EVAL process an EVAL-WHEN only when :EXECUTE is
;;; among its situations (CLHS 3.8). Without :EXECUTE the body is discarded:
;;; nothing in it is defined, as in SBCL.
;;;
;;; The defining forms in the body (DEFCONSTANT, DEFPARAMETER, DEFVAR,
;;; DEFMACRO, ...) used to be evaluated early whenever :COMPILE-TOPLEVEL was
;;; present, so (eval-when (:compile-toplevel :load-toplevel) (defconstant ...))
;;; defined the constant on a source LOAD. cl-bench's hash.lisp then ran on
;;; dotcl and failed on SBCL.

(defun %ewsl-load-source (lines)
  (let ((src "ewsl-tmp.lisp"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (dolist (l lines) (write-line l s)))
           (load src))
      (ignore-errors (delete-file src)))))

(defun %ewsl-defined (i)
  (list (boundp (intern (format nil "+EWSL-K~d+" i)))
        (boundp (intern (format nil "*EWSL-P~d*" i)))
        (boundp (intern (format nil "*EWSL-V~d*" i)))
        (and (fboundp (intern (format nil "EWSL-F~d" i))) t)
        (and (macro-function (intern (format nil "EWSL-M~d" i))) t)
        (and (get 'ewsl-key (intern (format nil "RAN~d" i))) t)))

(defun %ewsl-form (i situations)
  (format nil "(eval-when ~a (setf (get 'ewsl-key 'ran~d) t) (defconstant +ewsl-k~d+ ~d) (defparameter *ewsl-p~d* ~d) (defvar *ewsl-v~d* ~d) (defun ewsl-f~d () ~d) (defmacro ewsl-m~d () ~d))"
          situations i i i i i i i i i i i))

(deftest eval-when-source-load.situations
  (progn
    (%ewsl-load-source
     (list (%ewsl-form 0 "(:compile-toplevel)")
           (%ewsl-form 1 "(:compile-toplevel :load-toplevel)")
           (%ewsl-form 2 "(:load-toplevel)")
           (%ewsl-form 3 "(:execute)")
           (%ewsl-form 4 "(:compile-toplevel :execute)")
           (%ewsl-form 5 "(compile load eval)")))
    (loop for i below 6 collect (%ewsl-defined i)))
  ((nil nil nil nil nil nil)
   (nil nil nil nil nil nil)
   (nil nil nil nil nil nil)
   (t t t t t t)
   (t t t t t t)
   (t t t t t t)))

(deftest eval-when-eval.compile-toplevel-only
  (progn
    (eval (read-from-string (%ewsl-form 6 "(:compile-toplevel :load-toplevel)")))
    (eval (read-from-string (%ewsl-form 7 "(:compile-toplevel :execute)")))
    (list (%ewsl-defined 6) (%ewsl-defined 7)))
  ((nil nil nil nil nil nil) (t t t t t t)))

;; With :EXECUTE the early definition is still made, so a later form in the
;; same body can expand a macro that reads a variable defined before it.
(deftest eval-when-source-load.execute-early-defvar
  (progn
    (%ewsl-load-source
     '("(eval-when (:compile-toplevel :load-toplevel :execute)"
       "  (defvar *ewsl-slots* '(a b))"
       "  (defmacro ewsl-slots () `',*ewsl-slots*)"
       "  (defparameter *ewsl-seen* (ewsl-slots)))"))
    (symbol-value (intern "*EWSL-SEEN*")))
  (a b))
