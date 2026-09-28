;;; A variable docstring belongs to its symbol only, whether DEFVAR,
;;; DEFPARAMETER or DEFCONSTANT gave it or (SETF DOCUMENTATION) set it, and
;;; setting it to NIL clears it whichever of the two put it there.

(defpackage "VARDOC-A" (:use "COMMON-LISP"))
(defpackage "VARDOC-B" (:use "COMMON-LISP"))

(defvar vardoc-a::*x* 1 "Doc of A's X.")
(defparameter vardoc-a::*p* 2 "Doc of A's P.")
(defconstant vardoc-a::+c+ 3 "Doc of A's C.")
(defvar vardoc-b::*x* 10)

(deftest vardoc-on-the-symbol-itself
  (list (documentation 'vardoc-a::*x* 'variable)
        (documentation 'vardoc-a::*p* 'variable)
        (documentation 'vardoc-a::+c+ 'variable))
  ("Doc of A's X." "Doc of A's P." "Doc of A's C."))

(deftest vardoc-not-for-a-same-named-symbol
  (list (documentation 'vardoc-b::*x* 'variable)
        (documentation (intern "*P*" "VARDOC-B") 'variable)
        (documentation (make-symbol "*X*") 'variable)
        (documentation (make-symbol "+C+") 'variable))
  (nil nil nil nil))

(defvar vardoc-a::*cleared* 1 "Going away.")

(deftest vardoc-setf-nil-clears-a-defvar-docstring
  (progn (setf (documentation 'vardoc-a::*cleared* 'variable) nil)
         (documentation 'vardoc-a::*cleared* 'variable))
  nil)

;;; The later writer wins, in either order.

(defvar vardoc-a::*order* 1 "From DEFVAR.")

(deftest vardoc-setf-after-defvar
  (progn (setf (documentation 'vardoc-a::*order* 'variable) "From SETF.")
         (documentation 'vardoc-a::*order* 'variable))
  "From SETF.")

(deftest vardoc-defvar-after-setf
  (progn (eval '(defvar vardoc-a::*order* 1 "From DEFVAR again."))
         (documentation 'vardoc-a::*order* 'variable))
  "From DEFVAR again.")

(deftest vardoc-setf-on-an-unbound-symbol
  (let ((s (make-symbol "FRESH")))
    (setf (documentation s 'variable) "Fresh doc.")
    (list (documentation s 'variable)
          (documentation (make-symbol "FRESH") 'variable)))
  ("Fresh doc." nil))

;;; A built-in CL docstring stays underneath: clearing a user docstring on a CL
;;; variable falls back to it, as before.

(deftest vardoc-cl-builtin-after-clear
  (let ((builtin (documentation '*print-base* 'variable)))
    (setf (documentation '*print-base* 'variable) "Mine.")
    (list (documentation '*print-base* 'variable)
          (progn (setf (documentation '*print-base* 'variable) nil)
                 (equal (documentation '*print-base* 'variable) builtin))
          (stringp builtin)))
  ("Mine." t t))
