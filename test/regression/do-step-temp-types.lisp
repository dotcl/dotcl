;;; DO step temporaries inherit the loop variable's type declaration.
;;;
;;; DO with more than one stepping variable has to compute the new values before
;;; assigning any of them, so it expands to a LET of temporaries. Those
;;; temporaries used to be undeclared, so a loop whose variables are declared
;;; FIXNUM still boxed every step value, ran the generic operation on it and
;;; unboxed it back into the native slot -- three allocations an iteration, and
;;; the same loop written with DOTIMES paid none.
;;;
;;; A step temporary holds exactly the value about to be assigned to the loop
;;; variable, so the variable's declared type is as true of it as the user's own
;;; declaration is of the variable. The expander now carries the type over.
;;;
;;; What the tests pin: the typed shape appears where the declaration is there,
;;; nothing appears where it is not, the values are unchanged (including values
;;; outside the Fixnum cache), and a declaration that is not a type -- SPECIAL --
;;; is not carried over.

(setf dotcl:*save-sil* t)

(defun %dst-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %dst-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the shapes ----

(defun %dst-do (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (do ((i 0 (1+ i)) (s 0 (+ s (logand i 255))))
      ((>= i n) s)
    (declare (fixnum i s))))

(defun %dst-dotimes (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n s)
      (declare (fixnum i))
      (setq s (+ s (logand i 255))))))

;; No type declarations anywhere: the expansion must be what it always was.
(defun %dst-do-undeclared (n)
  (do ((i 0 (1+ i)) (s 0 (+ s i)))
      ((>= i n) s)))

;; The (type X var) spelling, and a bounded integer type rather than FIXNUM.
(defun %dst-do-type-spelling (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (do ((i 0 (1+ i)) (s 0 (+ s 1)))
      ((>= i n) s)
    (declare (type fixnum i) (type (signed-byte 32) s))))

;; DO* steps sequentially and needs no temporaries; pinned so it stays that way.
(defun %dst-do-star (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (do* ((i 0 (1+ i)) (s 0 (+ s (logand i 255))))
       ((>= i n) s)
    (declare (fixnum i s))))

(defun %dst-loop (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((s 0))
    (declare (fixnum s))
    (loop for i of-type fixnum from 0 below n
          do (setq s (+ s (logand i 255))))
    s))

;;; ---- SIL shape ----
;;;
;;; DEFTEST-EMITTING-ONLY, not DEFTEST: an emit-free build stores no SIL, so
;;; FUNCTION-SIL answers NIL there and every count taken from it is 0. That
;;; makes an assertion of "this instruction is gone" pass for the wrong reason
;;; and an assertion of "this slot is native" fail for the wrong reason. The
;;; values further down are the part that is a statement about the language,
;;; and they keep running everywhere.

;; The loop body of a native DO is free of boxing: the step values cost no
;; Fixnum.Make, and DO pays the same single box DOTIMES does (the result on the
;; way out).
(deftest-emitting-only do-step-temp-types.same-boxing-as-dotimes
  (list (%dst-count "Fixnum.Make" (%dst-sil #'%dst-do))
        (%dst-count "Fixnum.Make" (%dst-sil #'%dst-dotimes)))
  (1 1))

;; Exactly one box, no Increment, and the accumulator lives in an Int64 slot.
(deftest-emitting-only do-step-temp-types.no-boxing-in-body
  (let ((d (%dst-sil #'%dst-do)))
    (list (%dst-count "Fixnum.Make" d)
          (%dst-count "Runtime.Increment" d)
          (and (search "DECLARE-LOCAL S_" d) (search "Int64" d) t)))
  (1 0 t))

(deftest-emitting-only do-step-temp-types.type-spelling-is-native
  (let ((d (%dst-sil #'%dst-do-type-spelling)))
    (list (%dst-count "Runtime.Increment" d)
          (and (search "Int64" d) t)))
  (0 t))

;; Without declarations nothing changes: the generic path is still taken.
(deftest-emitting-only do-step-temp-types.undeclared-stays-generic
  (let ((d (%dst-sil #'%dst-do-undeclared)))
    (list (> (%dst-count "Fixnum.Make" d) 0)
          (> (%dst-count "Runtime.Increment" d) 0)))
  (t t))

(deftest-emitting-only do-step-temp-types.do-star-native
  (let ((d (%dst-sil #'%dst-do-star)))
    (list (%dst-count "Runtime.Increment" d) (and (search "Int64" d) t)))
  (0 t))

(deftest-emitting-only do-step-temp-types.loop-of-type-native
  (let ((d (%dst-sil #'%dst-loop)))
    (list (%dst-count "Runtime.Increment" d) (and (search "Int64" d) t)))
  (0 t))

;;; ---- values ----

(deftest do-step-temp-types.value-small
  (list (%dst-do 100) (%dst-dotimes 100))
  (4950 4950))

;; Past the Fixnum cache in both the counter and the accumulator.
(deftest do-step-temp-types.value-large
  (list (%dst-do 1000000) (%dst-dotimes 1000000))
  (127493856 127493856))

(deftest do-step-temp-types.value-zero-trips
  (list (%dst-do 0) (%dst-do-star 0) (%dst-do-undeclared 0))
  (0 0 0))

;; DO* is sequential, so S sees the already-stepped I. Unchanged by this fix,
;; and different from DO on purpose.
(deftest do-step-temp-types.do-star-is-sequential
  (list (%dst-do 1000000) (%dst-do-star 1000000))
  (127493856 127493920))

(deftest do-step-temp-types.undeclared-value
  (%dst-do-undeclared 1000)
  499500)

;;; ---- what is NOT carried over ----

(defvar *dst-special* 0)

;; A SPECIAL declaration describes the binding, not the value, so it must not
;; reach the temporary -- if it did, the temporary's assignment would go to the
;; symbol value and the parallel step would be wrong. The loop still works and
;; the special is restored on exit.
(defun %dst-do-special (n)
  (do ((*dst-special* 0 (1+ *dst-special*)) (s 0 (+ s *dst-special*)))
      ((>= *dst-special* n) s)
    (declare (special *dst-special*) (fixnum s))))

(deftest do-step-temp-types.special-not-carried
  (let ((*dst-special* 77))
    (list (%dst-do-special 10) *dst-special*))
  (45 77))

;; A step form that leaves the declared type is the user's error either way:
;; the value would be assigned to the declared variable next. What matters is
;; that the loop is not made to signal something new by the temporary. Stepping
;; a FIXNUM-declared variable with a non-number is a type error at the operation
;; itself, exactly as before the temporaries were declared.
(defun %dst-do-mistyped ()
  (do ((i 0 (1+ i)) (s 0 (cons s s)))
      ((>= i 2) s)
    (declare (fixnum i))))

(deftest do-step-temp-types.untyped-var-still-generic
  (let ((r (%dst-do-mistyped)))
    (and (consp r) (consp (car r)) t))
  t)

;;; ---- a DO whose variables are declared but never stepped ----

(deftest do-step-temp-types.no-step-no-temp
  (do ((i 0) (s 5))
      ((>= i 1) s)
    (declare (fixnum i s))
    (setq i (1+ i)))
  5)

(setf dotcl:*save-sil* nil)
