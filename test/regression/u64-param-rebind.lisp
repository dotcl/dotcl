;;; A required parameter declared (UNSIGNED-BYTE 64) and used only as an operand of
;;; 64-bit arithmetic is rebound at entry as a native 64-bit local, so its uses are
;;; the plain operations instead of a check and a helper call each. Parameters that
;;; are also passed on, returned, assigned or captured, optional and keyword
;;; parameters, and functions that call themselves are left as they were (a closure keeps the variable boxed). Values,
;;; and the error for a value that is not an (unsigned-byte 64), are the same either way.

(setf dotcl:*save-sil* t)

(defun %u64pr-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defmacro %u64pr-defs (&rest names)
  `(progn ,@(mapcar (lambda (n) `(declaim (notinline ,n))) names)))

;; applies: the plain declaration
(defun %u64pr-a (x r)
  (declare (type (simple-array (unsigned-byte 64) (4)) x) (type (unsigned-byte 64) r)
           (optimize (speed 3) (safety 0)))
  (setf (aref x 0) (logxor (aref x 0) r)
        (aref x 1) (logxor (aref x 1) #x1000000000000000 r))
  ;; padding so the function is not a candidate for inlining (the rebinding leaves small ones alone)
  #.(cons 'progn (loop for i below 16 collect (copy-tree '(setf (aref x 1) (logxor (aref x 1) 0)))))
  (values))

;; applies: the synonym form, a docstring, two declare forms
(defun %u64pr-b (x r)
  "A function with a docstring."
  (declare (type (simple-array (unsigned-byte 64) (4)) x))
  (declare (type (integer 0 18446744073709551615) r) (optimize (speed 3) (safety 0)))
  (setf (aref x 0) (logxor (aref x 0) r))
  #.(cons 'progn (loop for i below 16 collect (copy-tree '(setf (aref x 1) (logxor (aref x 1) 0)))))
  (values))

;; does not apply: R is passed on
(defun %u64pr-pass (x r)
  (declare (type (simple-array (unsigned-byte 64) (4)) x) (type (unsigned-byte 64) r)
           (optimize (speed 3) (safety 0)))
  (setf (aref x 0) (logxor (aref x 0) r))
  #.(cons 'progn (loop for i below 16 collect (copy-tree '(setf (aref x 1) (logxor (aref x 1) 0)))))
  (list r))

;; does not apply: R is assigned
(defun %u64pr-setq (x r)
  (declare (type (simple-array (unsigned-byte 64) (4)) x) (type (unsigned-byte 64) r)
           (optimize (speed 3) (safety 0)))
  (setq r (logxor r 1))
  (setf (aref x 0) (logxor (aref x 0) r))
  #.(cons 'progn (loop for i below 16 collect (copy-tree '(setf (aref x 1) (logxor (aref x 1) 0)))))
  (values))

;; R is captured by a closure: the rebound variable stays boxed there (a captured variable is
;; not a native local), so the checks remain; the values are the same
(defun %u64pr-closure (x r)
  (declare (type (simple-array (unsigned-byte 64) (4)) x) (type (unsigned-byte 64) r)
           (optimize (speed 3) (safety 0)))
  (funcall (lambda () (setf (aref x 0) (logxor (aref x 0) r))))
  (values))

;; does not apply: an optional and a keyword parameter
(defun %u64pr-opt (x &optional (r 5))
  (declare (type (simple-array (unsigned-byte 64) (4)) x) (type (unsigned-byte 64) r)
           (optimize (speed 3) (safety 0)))
  (setf (aref x 0) (logxor (aref x 0) r))
  (values))
(defun %u64pr-key (x &key (r 5))
  (declare (type (simple-array (unsigned-byte 64) (4)) x) (type (unsigned-byte 64) r)
           (optimize (speed 3) (safety 0)))
  (setf (aref x 0) (logxor (aref x 0) r))
  (values))

;; a function that calls itself is left as it was
(defun %u64pr-self (x r n)
  (declare (type (simple-array (unsigned-byte 64) (4)) x) (type (unsigned-byte 64) r)
           (type fixnum n) (optimize (speed 3) (safety 0)))
  (setf (aref x 0) (logxor (aref x 0) r))
  (if (> n 0) (%u64pr-self x r (1- n)) (values)))

;; the error for a value that is not an (unsigned-byte 64) at safety 1
(defun %u64pr-checked (r)
  (declare (type (unsigned-byte 64) r) (optimize (safety 1)))
  (logxor r 1))

(defun %u64pr-fresh () (make-array 4 :element-type '(unsigned-byte 64) :initial-element 0))
(defun %u64pr-result (fn &rest args)
  (let ((x (%u64pr-fresh)))
    (apply fn x args)
    (coerce x 'list)))
(defconstant +u64pr-big+ #xF123456789ABCDEF)

(deftest u64-param-rebind.values
  (list (%u64pr-result #'%u64pr-a +u64pr-big+)
        (%u64pr-result #'%u64pr-a 0)
        (%u64pr-result #'%u64pr-a #xFFFFFFFFFFFFFFFF)
        (%u64pr-result #'%u64pr-b +u64pr-big+)
        (%u64pr-result #'%u64pr-pass +u64pr-big+)
        (%u64pr-result #'%u64pr-setq +u64pr-big+)
        (%u64pr-result #'%u64pr-closure +u64pr-big+)
        (%u64pr-result #'%u64pr-opt)
        (%u64pr-result #'%u64pr-opt +u64pr-big+)
        (%u64pr-result #'%u64pr-key :r +u64pr-big+)
        (%u64pr-result #'%u64pr-self 3 2)
        (%u64pr-checked +u64pr-big+) (%u64pr-checked 0))
  ((#xF123456789ABCDEF #xE123456789ABCDEF 0 0)
   (0 #x1000000000000000 0 0)
   (#xFFFFFFFFFFFFFFFF #xEFFFFFFFFFFFFFFF 0 0)
   (#xF123456789ABCDEF 0 0 0)
   (#xF123456789ABCDEF 0 0 0)
   (#xF123456789ABCDEE 0 0 0)
   (#xF123456789ABCDEF 0 0 0)
   (5 0 0 0)
   (#xF123456789ABCDEF 0 0 0)
   (#xF123456789ABCDEF 0 0 0)
   (3 0 0 0)
   #xF123456789ABCDEE 1))

(deftest u64-param-rebind.error-for-a-value-that-is-not-a-u64
  (list (handler-case (%u64pr-checked -1) (type-error (e) (type-error-datum e)))
        (handler-case (%u64pr-checked #x10000000000000000) (type-error (e) (type-error-datum e)))
        (handler-case (%u64pr-checked "a") (type-error (e) (type-error-datum e))))
  (-1 #x10000000000000000 "a"))

(deftest u64-param-rebind.error-message-is-the-same
  (list (handler-case (%u64pr-checked -1) (type-error (e) (princ-to-string e)))
        (handler-case (%u64pr-checked 1.5) (type-error (e) (princ-to-string e))))
  ("The value -1 is not of type (UNSIGNED-BYTE 64)"
   "The value 1.5 is not of type (UNSIGNED-BYTE 64)"))

;; what the compiler did: the declared parameter is one check at entry, not a check and
;; helper calls at each of its uses
(deftest-emitting-only u64-param-rebind.applies
  (list (%u64pr-count "Runtime.IsU64" #'%u64pr-a)
        (%u64pr-count "Runtime.IsU64" #'%u64pr-b))
  (0 0))

(deftest-emitting-only u64-param-rebind.left-as-they-were
  (list (plusp (%u64pr-count "Runtime.IsU64" #'%u64pr-pass))
        (plusp (%u64pr-count "Runtime.IsU64" #'%u64pr-setq))
        (plusp (+ (%u64pr-count "Runtime.IsU64" #'%u64pr-closure)
                  (%u64pr-count "Runtime.Low64" #'%u64pr-closure))))
  (t t t))
