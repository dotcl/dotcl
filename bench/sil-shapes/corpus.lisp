;;; Bare forms for the fixnum-leaf-range gaps. Deliberately NO (the fixnum ...)
;;; anywhere: the `the` clause hands back the full int64 range whatever it
;;; wraps, so a wrapped form proves nothing about these leaves.

(defstruct pring
  (head 0 :type fixnum)
  (tail 0 :type (signed-byte 32))
  (items (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*))))

;;; ---- leaf 1: struct slot ----

(defun p-peek (r)
  (declare (type pring r) (optimize (speed 3) (safety 0) (debug 0)))
  (aref (pring-items r) (pring-head r)))

(defun p-dec (r)
  (declare (type pring r) (optimize (speed 3) (safety 0) (debug 0)))
  (1- (pring-head r)))

(defun p-inc (r)
  (declare (type pring r) (optimize (speed 3) (safety 0) (debug 0)))
  (1+ (pring-head r)))

(defun p-add (r)
  (declare (type pring r) (optimize (speed 3) (safety 0) (debug 0)))
  (+ (pring-head r) (pring-tail r)))

(defun p-mul (r)
  (declare (type pring r) (optimize (speed 3) (safety 0) (debug 0)))
  (* 3 (pring-tail r)))

(defun p-slot-bound (r)
  (declare (type pring r) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((n (pring-head r)) (s 0))
    (dotimes (i n s) (setq s (+ s i)))))

;;; ---- leaf 2: (length x) ----

(defun p-len-dec (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (1- (length v)))

(defun p-len-bound (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((n (length v)) (s 0))
    (dotimes (i n s) (setq s (+ s i)))))

(defun p-len-mul (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (* 2 (length v)))

;;; ---- leaf 3: (char-code (schar s i)) ----

(defun p-charcode (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (+ 1 (char-code (schar s i))))

(defun p-charcode-dec (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (1- (char-code (schar s i))))

;;; ---- leaf 4: (mod a b) / (rem a b) with a constant divisor ----

(defun p-mod (i)
  (declare (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (* 3 (mod i 10)))

(defun p-rem (i)
  (declare (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (* 3 (rem i 10)))

;;; ---- leaf 5: (if c a b) with both arms fixnum ----

(defun p-if (r c)
  (declare (type pring r) (optimize (speed 3) (safety 0) (debug 0)))
  (1+ (if c (pring-head r) (pring-tail r))))
