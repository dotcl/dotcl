;;; APPLY costs what the same call written as FUNCALL costs.
;;;
;;; Two separate charges, both per call:
;;;
;;; 1. RUNTIME.APPLY collected the argument list into a List<LispObject> and called
;;;    ToArray. That is three objects -- the List, its power-of-two backing array,
;;;    and the copy -- for a length the walk over the list already knew. Walking
;;;    once to count, then filling one array (or passing the arguments positionally
;;;    for the small arities, which is what makes FUNCALL free), removes all three.
;;;
;;; 2. The compiler rewrote (APPLY F A B LIST) to (APPLY F (LIST* A B LIST)), so
;;;    every spread argument cost a cons -- built only for APPLY to walk it straight
;;;    back apart. One and two fixed arguments now go to runtime entries that take
;;;    them positionally.

(defun %ac-f0 () :zero)
(defun %ac-f1 (a) (list :one a))
(defun %ac-f3 (a b c) (list a b c))
(defun %ac-opt (a &optional (b :dflt)) (list a b))
(defun %ac-key (a &key (k :dk) other) (list a k other))
(defun %ac-rest (a &rest r) (list a r))
(defun %ac-mv (a) (values a (* a 2)))

(defgeneric %ac-gf (x))
(defmethod %ac-gf ((x integer)) (list :int x))

;;; Behaviour, across the arities and the shapes that pick each path: 0-4 arguments
;;; go positionally, 5 and up build one array, and the fixed-argument count picks
;;; between APPLY / APPLYSPREAD1 / APPLYSPREAD2 / the LIST* rewrite.

(deftest apply-consing.arities
  (list (apply #'%ac-f0 nil)
        (apply #'%ac-f1 '(9))
        (apply #'%ac-f1 9 nil)
        (apply #'%ac-f3 '(1 2 3))
        (apply #'%ac-f3 1 '(2 3))
        (apply #'%ac-f3 1 2 '(3))
        (apply #'%ac-f3 1 2 3 nil))
  (:zero (:one 9) (:one 9) (1 2 3) (1 2 3) (1 2 3) (1 2 3)))

;;; Past the positional cut, and past the two spread entries into the LIST* rewrite.
(deftest apply-consing.long-argument-lists
  (list (apply #'list '(1 2 3 4 5 6 7 8 9))
        (apply #'list 1 2 3 4 5 '(6 7)))
  ((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7)))

(deftest apply-consing.lambda-list-keywords
  (list (apply #'%ac-opt '(1))
        (apply #'%ac-opt '(1 2))
        (apply #'%ac-key '(1))
        (apply #'%ac-key '(1 :k 5))
        (apply #'%ac-key 1 :k 5 '(:other 6))
        (apply #'%ac-rest '(1))
        (apply #'%ac-rest '(1 2 3 4)))
  ((1 :dflt) (1 2) (1 :dk nil) (1 5 nil) (1 5 6) (1 nil) (1 (2 3 4))))

(deftest apply-consing.callable-designators
  (list (apply '%ac-f3 '(1 2 3))
        (apply (lambda (a b) (+ a b)) '(1 2))
        (apply #'%ac-gf '(7)))
  ((1 2 3) 3 (:int 7)))

;;; Multiple values pass through untouched.
(deftest apply-consing.multiple-values
  (multiple-value-list (apply #'%ac-mv '(3)))
  (3 6))

;;; An improper last argument is a type error, on the plain and the spread entry
;;; alike -- the check has to happen before the callee runs, not after.
(deftest apply-consing.improper-list-signals
  (list (handler-case (apply #'%ac-f1 '(1 . 2)) (type-error () :type-error) (error () :other))
        (handler-case (apply #'%ac-f3 1 '(2 . 3)) (type-error () :type-error) (error () :other))
        (handler-case (apply #'%ac-f1 5) (type-error () :type-error) (error () :other)))
  (:type-error :type-error :type-error))

(deftest apply-consing.argument-errors-still-signalled
  (list (handler-case (progn (apply #'%ac-f3 '(1)) :no-error) (error () :error))
        (handler-case (progn (apply #'%ac-key '(1 :zz 2)) :no-error) (error () :error)))
  (:error :error))

;;; The point of the change.

(defparameter *ac-l1* (list 3))
(defparameter *ac-l3* (list 1 2 3))

(defun %ac-plain (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (apply #'%ac-f3 *ac-l3*)))))

(defun %ac-spread (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (apply #'%ac-f3 1 2 *ac-l1*)))))

;; %AC-F3 returns a fresh 3-list, so neither loop can reach zero; the bound is what
;; the result costs plus room, and well under what the call cost before (136 B for
;; the plain form, 200 for the spread one, on top of the same result).
;; Compiled-only, like the other consing assertions.
(deftest-compiled-only apply-consing.plain-costs-only-the-result
  (< (bytes-per-op #'%ac-plain) 130)
  t)

(deftest-compiled-only apply-consing.spread-costs-only-the-result
  (< (bytes-per-op #'%ac-spread) 130)
  t)
