;;; Cost of the loop back-edge interrupt safepoint, measured as
;;; polled loop vs the same loop under (optimize (safety 0)).
;;; Run: dotnet run --project runtime -- --asm compiler/cil-out.sil bench/interrupt-poll-cost.lisp

(defun spin-on (n)
  (declare (type (signed-byte 62) n))
  (let ((s 0))
    (declare (type (signed-byte 62) s))
    (dotimes (i n) (setq s (+ s 1)))
    s))

(defun spin-off (n)
  (declare (optimize (safety 0)) (type (signed-byte 62) n))
  (let ((s 0))
    (declare (type (signed-byte 62) s))
    (dotimes (i n) (setq s (+ s 1)))
    s))

(defun tco-on (i n)
  (if (>= i n) i (tco-on (+ i 1) n)))

(defun tco-off (i n)
  (declare (optimize (safety 0)))
  (if (>= i n) i (tco-off (+ i 1) n)))

(defmacro best-of (reps form)
  `(let ((best nil))
     ,form ; warmup
     (dotimes (r ,reps)
       (let ((t0 (get-internal-real-time)))
         ,form
         (let ((dt (- (get-internal-real-time) t0)))
           (when (or (null best) (< dt best)) (setq best dt)))))
     (/ (* best 1000.0) internal-time-units-per-second)))

(defparameter *n* 100000000)

;; Alternate the two variants so JIT/GC drift hits both equally.
(let ((on1  (best-of 5 (spin-on *n*)))
      (off1 (best-of 5 (spin-off *n*)))
      (on2  (best-of 5 (spin-on *n*)))
      (off2 (best-of 5 (spin-off *n*))))
  (let ((on (min on1 on2)) (off (min off1 off2)))
    (format t "~&SPIN  polled=~,1f ms  safety0=~,1f ms  overhead=~,2f ns/iter (~,1f%)~%"
            on off (/ (* (- on off) 1000000.0) *n*)
            (* 100.0 (/ (- on off) off)))))

(let ((on1  (best-of 5 (tco-on 0 *n*)))
      (off1 (best-of 5 (tco-off 0 *n*)))
      (on2  (best-of 5 (tco-on 0 *n*)))
      (off2 (best-of 5 (tco-off 0 *n*))))
  (let ((on (min on1 on2)) (off (min off1 off2)))
    (format t "~&TCO   polled=~,1f ms  safety0=~,1f ms  overhead=~,2f ns/iter (~,1f%)~%"
            on off (/ (* (- on off) 1000000.0) *n*)
            (* 100.0 (/ (- on off) off)))))
(finish-output)
