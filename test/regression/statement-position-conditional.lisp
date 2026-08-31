;;; A conditional whose value is discarded compiles its arms that way too.
;;;
;;; Both arms of an IF have to leave a value at the branch merge, so an arm that
;;; assigns to a variable in a native slot boxed that value to produce one --
;;; even where the conditional sits in statement position and nobody reads it.
;;; (WHEN c (SETQ n ...)) in a loop body is where that shows: one box an
;;; iteration, in a loop whose declarations exist to make it allocation-free.
;;;
;;; WHEN and UNLESS are lowered to IF here the same way their handlers lower
;;; them, and a discarded PROGN discards its last form too -- without which the
;;; arms stop at the PROGN that WHEN wraps its body in.

(defun %spc-when (n x)
  (declare (fixnum n) (double-float x))
  (let ((c 0))
    (declare (fixnum c))
    (do ((i 0 (1+ i))) ((= i n) c)
      (declare (fixnum i))
      (when (> x 4.0) (setq c (+ c 1))))))

(defun %spc-unless (n x)
  (declare (fixnum n) (double-float x))
  (let ((c 0))
    (declare (fixnum c))
    (do ((i 0 (1+ i))) ((= i n) c)
      (declare (fixnum i))
      (unless (< x 4.0) (setq c (+ c 2))))))

(defun %spc-if-both (n x)
  (declare (fixnum n) (double-float x))
  (let ((c 0))
    (declare (fixnum c))
    (do ((i 0 (1+ i))) ((= i n) c)
      (declare (fixnum i))
      (if (> x 4.0) (setq c (+ c 1)) (setq c (- c 1))))))

(deftest statement-position-conditional.values
  (list (%spc-when 5 9.5d0) (%spc-when 5 1.5d0)
        (%spc-unless 5 9.5d0) (%spc-unless 5 1.5d0)
        (%spc-if-both 5 9.5d0) (%spc-if-both 5 1.5d0))
  (5 0 10 0 5 -5))

;;; A conditional in VALUE position still produces a value, including the NIL a
;;; missing branch gives.
(deftest statement-position-conditional.value-position-unchanged
  (list (let ((x 5)) (when (> x 4) :yes))
        (let ((x 1)) (when (> x 4) :yes))
        (let ((x 1)) (unless (> x 4) :yes))
        (let ((x 5)) (if (> x 4) :yes :no))
        (let ((x 1)) (if (> x 4) :yes))
        (let ((x 1)) (list (when (> x 4) :a) (unless (> x 4) :b))))
  (:yes nil :yes :yes nil (nil :b)))

;;; The arms still run for effect, in order, and only the taken one runs.
(deftest statement-position-conditional.effects-in-order
  (let ((log '()))
    (flet ((note (x) (push x log)))
      (when t (note 1) (note 2))
      (when nil (note :never))
      (unless nil (note 3))
      (if t (note 4) (note :never))
      (if nil (note :never) (note 5))
      (progn (note 6) (note 7)))
    (reverse log))
  (1 2 3 4 5 6 7))

;;; A local shadowing of the operator is not a conditional at all.
(deftest statement-position-conditional.shadowed-operator
  (let ((seen '()))
    (macrolet ((when (&rest args) `(push (list :macrolet ,@args) seen)))
      (when 1 2))
    seen)
  ((:macrolet 1 2)))

;;; The point: no box an iteration.
(defun %spc-bytes-for (f n)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (nth 4 (dotcl:gc-stats))))
        (funcall f n 9.5d0)
        (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

;; Compiled-only: a statement about the code the compiler emits.
(deftest-compiled-only statement-position-conditional.loop-allocates-nothing
  (progn (%spc-bytes-for #'%spc-when 1000)   ; warm
         (%spc-bytes-for #'%spc-if-both 1000)
         (let ((w (- (%spc-bytes-for #'%spc-when 400000)
                     (%spc-bytes-for #'%spc-when 100000)))
               (b (- (%spc-bytes-for #'%spc-if-both 400000)
                     (%spc-bytes-for #'%spc-if-both 100000))))
           ;; A box an iteration was 24 bytes, so 300k extra iterations would
           ;; show over 7 MB on each of these.
           (list (< w 100000) (< b 100000))))
  (t t))
