;;; COMPILE-FILE and a DEFMACRO that is not a top-level form.
;;;
;;; CLHS 3.2.3.1: only a top-level DEFMACRO has a compile-time side effect. One
;;; nested in UNLESS, LET or any other non-top-level position is ordinary code
;;; that defines the macro when it runs. The compiler used to register every
;;; DEFMACRO it compiled, top level or not, so a file shaped like
;;;
;;;   (eval-when (:compile-toplevel :load-toplevel :execute)
;;;     (unless (fboundp 'm)
;;;       (defmacro m (x) (helper x))
;;;       (defun helper (x) ...)))
;;;
;;; (lift's timeout/with-timeout.lisp) saw M already fbound when the compile-time
;;; evaluation tested it, skipped the whole body, and HELPER never existed:
;;; the first use of M failed with "Undefined function: HELPER". SBCL runs the
;;; body and defines both.

(defun ntdm-compile-and-load (name lines)
  (let ((src (format nil "~a-tmp.lisp" name))
        (fasl (format nil "~a-tmp.fasl" name)))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (dolist (l lines) (write-line l s)))
           (compile-file src :output-file fasl)
           (load fasl)
           t)
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl)))))

(deftest-compiled-only nontoplevel-defmacro.guarded-body-runs-at-compile-time
  (progn
    (ntdm-compile-and-load
     "ntdm1"
     '("(eval-when (:compile-toplevel :load-toplevel :execute)"
       "  (unless (fboundp 'ntdm1-m)"
       "    (defmacro ntdm1-m (x) (ntdm1-build x))"
       "    (defun ntdm1-build (x) (list 'list :built x))))"
       "(defun ntdm1-use () (ntdm1-m 7))"))
    (funcall (intern "NTDM1-USE")))
  (:built 7))

;; A non-top-level DEFMACRO compiled but never run leaves no macro behind.
;; (The form is a top-level WHEN rather than a DEFUN body: COMPILE-FILE also
;; evaluates a top-level DEFUN early, and that evaluation compiles the body
;; outside COMPILE-FILE's rules.)
(deftest-compiled-only nontoplevel-defmacro.not-defined-by-compiling
  (let ((src "ntdm2-tmp.lisp")
        (fasl "ntdm2-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(when nil (defmacro ntdm2-m () 2))" s))
           (compile-file src :output-file fasl)
           (macro-function (intern "NTDM2-M")))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  nil)
