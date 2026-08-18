;;; GET-INTERNAL-REAL-TIME / GET-INTERNAL-RUN-TIME and their unit.
;;;
;;; Two bugs lived here. GET-INTERNAL-RUN-TIME returned TimeSpan ticks (100 ns,
;;; 1e7 per second) while INTERNAL-TIME-UNITS-PER-SECOND said 1000, so dividing
;;; one by the other -- which is the only thing the value is for -- was off by a
;;; factor of 10000. And GET-INTERNAL-REAL-TIME read TickCount64, which reports
;;; milliseconds but only advances at the platform timer granularity: 15.6 ms on
;;; Windows. Nothing shorter than that could be timed at all, so microbenchmarks
;;; quantized into 15.6 ms steps and quietly reported garbage for fast operations.
;;;
;;; Both now report microseconds from a high-resolution counter.
;;;
;;; The tests are deliberately loose about absolute timing (a loaded machine can
;;; stretch anything) but strict about the unit, which is what was wrong.

(defun %itu-busy (n)
  "Burn CPU without allocating; returns a value so it cannot be optimized away."
  (let ((s 0))
    (dotimes (i n s) (setq s (logand (+ s i) 1048575)))))

(deftest internal-time.units-are-microseconds
  internal-time-units-per-second
  1000000)

;; Real time must advance across a sleep, in the unit it claims. A 50 ms sleep
;; is 50000 units; allow a wide band for scheduling, but nothing near the
;; 10000x that the run-time unit was off by.
(deftest internal-time.real-time-tracks-sleep
  (let* ((t0 (get-internal-real-time))
         (ignore (sleep 0.05))
         (dt (- (get-internal-real-time) t0)))
    (declare (ignore ignore))
    (and (> dt 20000) (< dt 2000000)))
  t)

;; Run time is CPU time, so a busy loop must move it, and it must move by
;; roughly what real time moved -- same unit, same order. The 10000x bug fails
;; this by six orders of magnitude.
(deftest internal-time.run-time-same-unit-as-real-time
  (let* ((r0 (get-internal-real-time))
         (c0 (get-internal-run-time)))
    (%itu-busy 3000000)
    (let ((real (- (get-internal-real-time) r0))
          (cpu (- (get-internal-run-time) c0)))
      (and (> real 0) (> cpu 0)
           (< cpu (* real 20))
           (> (* cpu 20) real))))
  t)

;; Resolution: the whole point of the change. Sampling around work far shorter
;; than a 15.6 ms timer tick has to produce more than one distinct value.
(deftest internal-time.resolves-below-a-timer-tick
  (let ((seen '()))
    (dotimes (i 12)
      (%itu-busy 20000)
      (pushnew (get-internal-real-time) seen))
    (> (length seen) 2))
  t)

;; Monotonic: successive reads never go backwards.
(deftest internal-time.monotonic
  (let ((last (get-internal-real-time)) (ok t))
    (dotimes (i 200 ok)
      (let ((now (get-internal-real-time)))
        (when (< now last) (setq ok nil))
        (setq last now))))
  t)
