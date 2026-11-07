;;; Compiling nested LETs that each bind an array and fill it in a DOTIMES whose body holds the next
;;; one used to repeat the work at every level, so the compile time grew about fourfold per level
;;; (depth 8 took 43 s). Checked by counting how often the compiler compiles a LET, not by time.

(defun %nalc-nest (n)
  (if (zerop n)
      1
      (let ((a (intern (format nil "NALC-A~d" n))) (i (intern (format nil "NALC-I~d" n))))
        `(let ((,a (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
           (dotimes (,i 3) (setf (aref ,a ,i) (mod ,(%nalc-nest (1- n)) 256)))
           (aref ,a 0)))))

(defun %nalc-let-compilations (n)
  (let* ((sym (find-symbol "%COMPILE-LET" "DOTCL.CIL-COMPILER"))
         (old (fdefinition sym))
         (count 0))
    (unwind-protect
         (progn
           (setf (fdefinition sym) (lambda (&rest args) (incf count) (apply old args)))
           (funcall (compile nil `(lambda () ,(%nalc-nest n))))
           count)
      (setf (fdefinition sym) old))))

(deftest-emitting-only nested-array-let-compile-cost.bounded
  (let ((c4 (%nalc-let-compilations 4))
        (c8 (%nalc-let-compilations 8)))
    ;; Twice the depth costs a small multiple of the work, not its fourth power.
    (list (< c4 200) (< c8 700)))
  (t t))
