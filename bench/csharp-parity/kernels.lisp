;;; bench/csharp-parity/kernels.lisp -- Lisp half of the C# parity benchmark.
;;;
;;; Seven small kernels that a user would write with every declaration in
;;; place. The C# half (Parity.csproj) computes the same thing over the same
;;; input sizes, so the two outputs divide into a time ratio per kernel.
;;;
;;; Run it the way `make bench-parity` does -- a RELEASE runtime, because a
;;; Debug assembly is a different implementation to the JIT:
;;;
;;;   runtime/bin/Release/net10.0/runtime --asm compiler/cil-out.sil \
;;;       bench/csharp-parity/kernels.lisp
;;;
;;; Output is one "name<TAB>milliseconds" line per kernel on stdout, the
;;; minimum of 5 timed runs after 1 warmup run. Anything else this file prints
;;; goes to stderr so the stdout stream stays machine-readable.
;;;
;;; Every OPTIMIZE declaration is written inside the function body rather than
;;; as one file-level DECLAIM. A body declaration is the form whose effect is
;;; local and unambiguous: it applies to this function whatever the surrounding
;;; file says, which is what a benchmark wants of the policy it is measuring
;;; under. The distinction used to matter for more than tidiness -- a file-level
;;; declaim of (safety 0) did not reach loop safepoints at all, and the fixnum
;;; loop measured 219 ms declared that way against 63 ms declared in the body.
;;; That gap is closed now, and these numbers no longer depend on which form is
;;; used; the declarations stay in the bodies because that is still the form
;;; that cannot be changed from a distance.

(in-package :cl-user)

;;; --- Kernels -----------------------------------------------------------

(defun k-tak (x y z)
  (declare (fixnum x y z)
           (optimize (speed 3) (safety 0) (debug 0)))
  (if (not (< y x))
      z
      (k-tak (k-tak (the fixnum (- x 1)) y z)
             (k-tak (the fixnum (- y 1)) z x)
             (k-tak (the fixnum (- z 1)) x y))))

(defun k-fib (n)
  (declare (fixnum n)
           (optimize (speed 3) (safety 0) (debug 0)))
  (if (< n 2)
      n
      (the fixnum (+ (k-fib (the fixnum (- n 1)))
                     (k-fib (the fixnum (- n 2)))))))

(defun k-fixnum-loop (n)
  (declare (fixnum n)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (the fixnum (logand i 255))))))))

(defun k-double-loop (n)
  (declare (fixnum n)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((x 1.0d0)
        (acc 0.0d0))
    (declare (double-float x acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq x (the double-float (* x 1.0000001d0)))
      (setq acc (the double-float (+ acc (the double-float (/ 1.0d0 x))))))))

(defun k-array-walk (arr passes)
  (declare (type (simple-array fixnum (*)) arr)
           (fixnum passes)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0)
        (n (length arr)))
    (declare (fixnum acc n))
    (do ((p 0 (the fixnum (1+ p))))
        ((>= p passes) acc)
      (declare (fixnum p))
      (do ((i 0 (the fixnum (1+ i))))
          ((>= i n))
        (declare (fixnum i))
        (setq acc (the fixnum (+ acc (the fixnum (aref arr i)))))))))

(defstruct (k-point (:constructor make-k-point (a b)))
  (a 0 :type fixnum)
  (b 0 :type fixnum))

(defun k-struct-loop (p n)
  (declare (type k-point p)
           (fixnum n)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc
                               (the fixnum (k-point-a p))
                               (the fixnum (k-point-b p)))))
      (setf (k-point-a p) i))))

(defun k-string-walk (s passes)
  (declare (simple-string s)
           (fixnum passes)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0)
        (n (length s)))
    (declare (fixnum acc n))
    (do ((p 0 (the fixnum (1+ p))))
        ((>= p passes) acc)
      (declare (fixnum p))
      (do ((i 0 (the fixnum (1+ i))))
          ((>= i n))
        (declare (fixnum i))
        (setq acc (the fixnum (+ acc (char-code (schar s i)))))))))

;;; --- Harness -----------------------------------------------------------
;;;
;;; Minimum of 5, not mean: the minimum is the run least disturbed by whatever
;;; else the machine was doing, and on this workload the distribution is a hard
;;; floor with a noisy tail. Five rather than three because three left tak
;;; swinging by 1.27x between processes -- enough to trip a 1.2x gate on its
;;; own -- and the extra samples are what find the fully-tiered state.
;;;
;;; Each kernel carries a repeat count, and the reported figure is the time for
;;; the whole repeated block. The counts exist to put every block above roughly
;;; 30 ms, which is where this measurement stops being dominated by tiering and
;;; scheduling: one tak 24 16 8 runs in single-digit milliseconds, and back to
;;; back runs of it disagreed by a factor of 1.9 -- more than the threshold the
;;; CI gate is supposed to detect. The same counts are used on the C# side, so
;;; the ratio is unaffected by them; only the absolute figures scale.

(defparameter *parity-runs* 5)

(defun parity-ms (repeats thunk)
  (let ((start (get-internal-real-time)))
    (dotimes (i repeats)
      (funcall thunk))
    (/ (* 1000.0d0 (- (get-internal-real-time) start))
       internal-time-units-per-second)))

(defun parity-report (name repeats thunk)
  ;; One warmup block, discarded: it pays for JIT compilation and for
  ;; first-touch of whatever the kernel allocates.
  (parity-ms repeats thunk)
  (let ((best nil))
    (dotimes (i *parity-runs*)
      (let ((ms (parity-ms repeats thunk)))
        (when (or (null best) (< ms best))
          (setq best ms))))
    (format t "~A~C~,3F~%" name #\Tab best)
    (finish-output *standard-output*)))

;;; --- Inputs ------------------------------------------------------------
;;;
;;; Built once, outside the timed region, and identical to the C# side. A
;;; kernel that builds its own input measures the construction too, which is
;;; the classic way to make two implementations look closer than they are.

(defparameter *array-size* 1024)
(defparameter *array-passes* 100000)
(defparameter *string-size* 1048576)
(defparameter *string-passes* 100)

(defparameter *k-array*
  (let ((a (make-array *array-size* :element-type 'fixnum :initial-element 0)))
    (dotimes (i *array-size* a)
      (setf (aref a i) (mod i 256)))))

(defparameter *k-string*
  (let ((s (make-string *string-size* :initial-element #\a)))
    (dotimes (i *string-size* s)
      (setf (schar s i) (code-char (+ 32 (mod i 64)))))))

(defparameter *k-point* (make-k-point 1 2))

;;; --- Main --------------------------------------------------------------

(defun parity-main ()
  (parity-report "tak" 40 (lambda () (k-tak 24 16 8)))
  (parity-report "fib" 5 (lambda () (k-fib 32)))
  (parity-report "fixnum-loop" 1 (lambda () (k-fixnum-loop 100000000)))
  (parity-report "double-loop" 1 (lambda () (k-double-loop 50000000)))
  (parity-report "array-walk" 1
                 (lambda () (k-array-walk *k-array* *array-passes*)))
  (parity-report "struct-slots" 5
                 (lambda () (k-struct-loop *k-point* 10000000)))
  (parity-report "string-walk" 1
                 (lambda () (k-string-walk *k-string* *string-passes*))))

(parity-main)
