;;; Adaptive in-process repetition for the ledger's steady-state column.
(defpackage #:ledger-steady
  (:use #:cl)
  (:export #:env #:env-int #:env-number #:stable-window-p #:still-dropping-p
           #:window-after-p
           #:measure))

(in-package #:ledger-steady)

(defun env (name)
  (let ((v #+dotcl (dotcl:getenv name)
           #+sbcl (sb-ext:posix-getenv name)
           #-(or dotcl sbcl) nil))
    (and v (plusp (length v)) v)))

(defun env-int (name default)
  (let ((v (env name)))
    (if v (parse-integer v) default)))

(defun env-number (name default)
  (let ((v (env name)))
    (if v (read-from-string v) default)))

(defun stable-window-p (times count tolerance)
  (and (>= (length times) count)
       (let* ((window (subseq times 0 count))
              (low (reduce #'min window))
              (high (reduce #'max window)))
         (and (plusp low) (<= high (* low (+ 1 tolerance)))))))

(defun still-dropping-p (times count tolerance)
  "True when the last COUNT runs are faster than the COUNT runs before them by
more than TOLERANCE, i.e. the benchmark is still climbing to a faster tier.
TIMES is newest first."
  (and (>= (length times) (* 2 count))
       (let ((recent (reduce #'min (subseq times 0 count)))
             (previous (reduce #'min (subseq times count (* 2 count)))))
         (> previous (* recent (+ 1 tolerance))))))

(defun window-after-p (starts count min-seconds)
  (and (>= (length starts) count)
       (>= (nth (1- count) starts) min-seconds)))

(defun measure (thunk &key prepare since (clock #'get-internal-real-time))
  (let* ((tolerance (env-number "LEDGER_STEADY_TOLERANCE" 0.05))
         (count (env-int "LEDGER_STEADY_WINDOW" 3))
         (min-runs (env-int "LEDGER_STEADY_MIN_RUNS" 10))
         (max-runs (env-int "LEDGER_STEADY_MAX_RUNS" 20))
         (min-seconds (env-number "LEDGER_STEADY_MIN_SECONDS" 8))
         (max-seconds (env-number "LEDGER_STEADY_MAX_SECONDS" 120))
         (start (funcall clock))
         (min-start (or since start))
         (times '())
         (starts '())
         (runs 0)
         (runs-after-min 0)
         (stable nil))
    (loop
      (when prepare (funcall prepare))
      (let* ((t0 (funcall clock))
             (run-start (/ (float (- t0 min-start) 1d0)
                           internal-time-units-per-second)))
        (push run-start starts)
        (when (>= run-start min-seconds) (incf runs-after-min))
        (funcall thunk)
        (push (/ (float (- (funcall clock) t0) 1d0)
                 internal-time-units-per-second)
              times))
      (incf runs)
      (cond ((and (>= runs min-runs)
                  (window-after-p starts count min-seconds)
                  (stable-window-p times count tolerance)
                  (not (still-dropping-p times count tolerance)))
             (setf stable t)
             (return))
            ((>= runs-after-min max-runs) (return))
            ((>= (/ (float (- (funcall clock) start) 1d0)
                    internal-time-units-per-second)
                 max-seconds)
             (return))))
    (let ((window (if stable (subseq times 0 count) times)))
      (values (reduce #'min window)
              runs
              stable
              (nreverse (copy-list times))))))
