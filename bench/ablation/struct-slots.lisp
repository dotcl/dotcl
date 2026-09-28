;;; The Lisp half of the ablation harness, holding the investigation it was
;;; written for as a worked example. See README.md.
;;;
;;; Here: bench/csharp-parity's struct-slots kernel, decomposed. Same loop
;;; scaffolding every time, one term added at a time, so the difference
;;; between two variants is the price of the term between them. L0 is the
;;; scaffolding with nothing in it, which is the question you have to settle
;;; before studying anything inside the loop: it came out at a four-instruction
;;; inner loop against the C# kernel's eight, so the loop machinery was not
;;; where the time went and everything after that looked at the slot accesses.
;;; L5 reads the slots once outside the loop, which is not equivalent Lisp but
;;; bounds what the body costs when the access guards are paid once.
;;;
;;; All variants run alternately in one process, min-of-N, input built outside
;;; every timed region.
;;;
;;; Run it with DOTNET_TieredCompilation=0:
;;;
;;;   DOTNET_gcServer=0 DOTNET_TieredCompilation=0 \
;;;     runtime/bin/Release/net10.0/runtime.exe --asm compiler/cil-out.sil \
;;;     bench/ablation/struct-slots.lisp < /dev/null
;;;
;;; With tiering on these variants do not all reach the same state, and one of
;;; them came out slower than a variant that does strictly more work.

(defstruct (k-point (:constructor make-k-point (a b)))
  (a 0 :type fixnum)
  (b 0 :type fixnum))

;; L0: the scaffolding alone -- do-loop, fixnum acc, no slot touched.
(defun l0-scaffold (p n)
  (declare (type k-point p) (fixnum n)
           (ignore p)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc i))))))

;; L1: one slot read.
(defun l1-read1 (p n)
  (declare (type k-point p) (fixnum n)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (the fixnum (k-point-a p))))))))

;; L2: two slot reads, no write.
(defun l2-read2 (p n)
  (declare (type k-point p) (fixnum n)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc
                               (the fixnum (k-point-a p))
                               (the fixnum (k-point-b p))))))))

;; L3: the write alone.
(defun l3-write (p n)
  (declare (type k-point p) (fixnum n)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (declare (fixnum acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc i)))
      (setf (k-point-a p) i))))

;; L4: the real kernel.
(defun l4-full (p n)
  (declare (type k-point p) (fixnum n)
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

;; L5: the same work with the slots read once into locals outside the loop.
;; Not equivalent Lisp (it does not re-read A after the write), but it bounds
;; what the loop costs when the guard chain is paid once instead of per access.
(defun l5-hoisted (p n)
  (declare (type k-point p) (fixnum n)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0)
        (a (the fixnum (k-point-a p)))
        (b (the fixnum (k-point-b p))))
    (declare (fixnum acc a b))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc a b))))))

(defparameter *runs* 12)

(defun ms (repeats thunk)
  (let ((start (get-internal-real-time)))
    (dotimes (i repeats)
      (funcall thunk))
    (/ (* 1000.0d0 (- (get-internal-real-time) start))
       internal-time-units-per-second)))

(defun report (name repeats thunk)
  (ms repeats thunk)
  (let ((best nil))
    (dotimes (i *runs*)
      (let ((v (ms repeats thunk)))
        (when (or (null best) (< v best)) (setq best v))))
    (format t "~A~C~,3F~%" name #\Tab best)
    (finish-output *standard-output*)))

(defparameter *p* (make-k-point 1 2))

(report "L0-scaffold" 5 (lambda () (l0-scaffold *p* 10000000)))
(report "L1-read1   " 5 (lambda () (l1-read1    *p* 10000000)))
(report "L2-read2   " 5 (lambda () (l2-read2    *p* 10000000)))
(report "L3-write   " 5 (lambda () (l3-write    *p* 10000000)))
(report "L4-full    " 5 (lambda () (l4-full     *p* 10000000)))
(report "L5-hoisted " 5 (lambda () (l5-hoisted  *p* 10000000)))
