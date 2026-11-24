;;; An access through a hoisted element buffer keeps the per-element helper as the
;;; arm for "no buffer". That arm is a call the JIT is told not to inline: in a
;;; method with hundreds of such accesses the inlined copies made the JIT stop
;;; tracking locals and spill everything (ironclad WHIRLPOOL ran 1.4 times slower).
;;; The values and the error messages are the helper's, unchanged.

(setf dotcl:*save-sil* t)

(defun %afc-read (a i)
  (declare (type (simple-array (unsigned-byte 32) (16)) a) (type (integer 0 15) i))
  (+ (aref a i) (aref a (- 15 i))))

(defun %afc-write (a i v)
  (declare (type (simple-array (unsigned-byte 32) (16)) a) (type (integer 0 15) i)
           (type (unsigned-byte 32) v))
  (setf (aref a i) v)
  (aref a i))

(defun %afc-read2 (a i j)
  (declare (type (simple-array (unsigned-byte 32) (4 4)) a) (type (integer 0 3) i j))
  (aref a i j))

(defun %afc-sil (fn) (%flat-text (dotcl:function-sil fn)))

(deftest aref-fallback-arm-cold.values
  (let ((a (make-array 16 :element-type '(unsigned-byte 32) :initial-element 3))
        (b (make-array '(4 4) :element-type '(unsigned-byte 32) :initial-element 5)))
    (setf (aref a 15) 9)
    (list (%afc-read a 0) (%afc-write a 2 77) (%afc-read2 b 1 2)))
  (12 77 5))

(deftest aref-fallback-arm-cold.out-of-range-message
  (let ((a (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)))
    (handler-case (progn (%afc-write a 3 (expt 2 32)) :no-error)
      (error () :error)))
  :error)

(deftest-emitting-only aref-fallback-arm-cold.sil-uses-cold-helper
  (list (and (search "Runtime.ArefNumLCold" (%afc-sil #'%afc-read)) t)
        (and (search "Runtime.ArefSetNumLCold" (%afc-sil #'%afc-write)) t)
        (and (search "(CALL \"Runtime.ArefNumL\")" (%afc-sil #'%afc-read)) t))
  (t t nil))
(setf dotcl:*save-sil* nil)
