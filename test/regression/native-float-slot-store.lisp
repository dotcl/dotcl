;;; Assigning into a declared float local does not allocate.
;;;
;;; A local declared DOUBLE-FLOAT lives in a raw Double slot, and the arithmetic
;;; feeding it compiles to raw float instructions -- but the store went through a
;;; box: the value was wrapped in a DoubleFloat and unwrapped again on the very
;;; next instruction, one allocation per assignment in exactly the loops the
;;; declaration exists to make allocation-free. The box and the unbox are now
;;; recognized as the pair they are and both dropped.

(defun %nfs-double (n)
  (declare (fixnum n))
  (let ((s 0.0d0))
    (declare (double-float s))
    (dotimes (i n s) (setq s (+ s 1.5d0)))))

(defun %nfs-single (n)
  (declare (fixnum n))
  (let ((s 0.0))
    (declare (single-float s))
    (dotimes (i n s) (setq s (+ s 1.5)))))

(deftest native-float-slot-store.values-are-unchanged
  (list (%nfs-double 0) (%nfs-double 4) (%nfs-single 0) (%nfs-single 4))
  (0.0d0 6.0d0 0.0 6.0))

;;; Measured as the difference between two loop lengths, taking the smallest of
;;; several runs: the allocation counter is process-wide, so anything else alive
;;; in the image adds to some samples and never subtracts from any.
(defun %nfs-bytes-for (f n)
  (let ((best nil))
    (dotimes (r 5 best)
      (let ((before (nth 4 (dotcl:gc-stats))))
        (funcall f n)
        (let ((used (- (nth 4 (dotcl:gc-stats)) before)))
          (when (or (null best) (< used best)) (setq best used)))))))

;; Compiled-only: this is a statement about the code the compiler emits. An
;; emit-free build interprets the loop, where the value has no native slot to
;; stay in and the allocation is expected.
(deftest-compiled-only native-float-slot-store.loop-allocates-nothing
  (progn (%nfs-bytes-for #'%nfs-double 1000)   ; warm
         (%nfs-bytes-for #'%nfs-single 1000)
         (let ((d (- (%nfs-bytes-for #'%nfs-double 400000)
                     (%nfs-bytes-for #'%nfs-double 100000)))
               (s (- (%nfs-bytes-for #'%nfs-single 400000)
                     (%nfs-bytes-for #'%nfs-single 100000))))
           ;; One box per assignment was 24 bytes, so 300k extra iterations
           ;; would show over 7 MB on each of these.
           (list (< d 100000) (< s 100000))))
  (t t))
