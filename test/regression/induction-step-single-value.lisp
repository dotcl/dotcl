;;; The step of a counting DO / LOOP variable is single-valued, and the
;;; compiler knows it.
;;;
;;; DO and LOOP write the step of a counting variable as
;;; (%INDUCTION-NEXT VAR DELTA), a macro that expands to (1+ VAR), (1- VAR) or
;;; (+ VAR DELTA). The name lets the compiler tell the macro's own step from any
;;; other assignment when it gives the variable a range type. Every expansion
;;; answers one value, but the SETQ of the step looked at the unexpanded form,
;;; did not know the name, and put a Runtime.UnwrapMv after every step of a
;;; loop whose counter is not typed (an untyped DO up to an argument).
;;;
;;; A step through an ordinary function keeps its unwrap. The value tests come
;;; first and the instruction counts after.

(setf dotcl:*save-sil* t)

(defun %isv-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %isv-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the shapes: counters with no declaration, limits from the arguments ----

(defun %isv-do-up (n)
  (let ((acc 0))
    (do ((i 0 (1+ i)))
        ((>= i n) acc)
      (setq acc (+ acc (logand i 255))))))

(defun %isv-do-down (n)
  (let ((acc 0))
    (do ((i n (- i 3)))
        ((<= i 0) acc)
      (setq acc (+ acc i)))))

(defun %isv-loop (s n)
  (let ((acc 0))
    (loop for i from s below n by 2 do (setq acc (+ acc i))
          finally (return acc))))

;; The control: a step that is a call to an ordinary function keeps its
;; unwrap. If this ever reads 0, the counts below prove nothing.
(defun %isv-inc (x) (+ x 1))
(defun %isv-do-call (n)
  (let ((acc 0))
    (do ((i 0 (%isv-inc i)))
        ((>= i n) acc)
      (setq acc (+ acc i)))))

;;; ---- values ----

(deftest induction-step-single-value.values
  (list (%isv-do-up 1000) (%isv-do-down 10) (%isv-loop 3 12) (%isv-do-call 5)
        (%isv-do-up 0) (%isv-loop 5 5))
  (124716 22 35 10 0 0))

;;; ---- emitted code ----

(deftest-emitting-only induction-step-single-value.no-unwrap
  (mapcar (lambda (fn) (%isv-count "Runtime.UnwrapMv" (%isv-sil fn)))
          (list #'%isv-do-up #'%isv-do-down #'%isv-loop))
  (0 0 0))

(deftest-emitting-only induction-step-single-value.control-keeps-unwrap
  (plusp (%isv-count "Runtime.UnwrapMv" (%isv-sil #'%isv-do-call)))
  t)
