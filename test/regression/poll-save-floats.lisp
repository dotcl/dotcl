;;; A loop that carries raw DOUBLE-FLOAT or SINGLE-FLOAT locals round its
;;; back-edge saves them around the slow call of its interrupt poll and reads
;;; them back. The values must come back unchanged whether the slow path runs
;;; (poll counting raises the gate on every iteration) or not, and the same
;;; loop without a poll (SAFETY 0) is the reference.

(setf dotcl:*save-sil* t)

(defun %psf-series (n)
  (declare (fixnum n))
  (let ((x 0.0f0) (acc 0.0f0))
    (declare (single-float x acc))
    (dotimes (i n acc)
      (setq acc (+ (* acc 0.999f0) (/ (* x x) (+ 1.0f0 (* x x))))
            x (if (> x 1.0f0) 0.0f0 (+ x 0.001f0))))))

(defun %psf-series-nopoll (n)
  (declare (fixnum n) (optimize (safety 0)))
  (let ((x 0.0f0) (acc 0.0f0))
    (declare (single-float x acc))
    (dotimes (i n acc)
      (setq acc (+ (* acc 0.999f0) (/ (* x x) (+ 1.0f0 (* x x))))
            x (if (> x 1.0f0) 0.0f0 (+ x 0.001f0))))))

(defun %psf-rec (i n x acc)
  (declare (fixnum i n) (double-float x acc))
  (if (= i n)
      acc
      (%psf-rec (+ i 1) n (+ x 0.25d0) (+ (* acc 0.5d0) (sqrt x)))))

(defun %psf-rec-nopoll (i n x acc)
  (declare (fixnum i n) (double-float x acc) (optimize (safety 0)))
  (if (= i n)
      acc
      (%psf-rec-nopoll (+ i 1) n (+ x 0.25d0) (+ (* acc 0.5d0) (sqrt x)))))

(defmacro %psf-counting (&body body)
  `(progn
     (setf (dotnet:static "DotCL.ConditionSystem" "CountPolls") t)
     (unwind-protect (progn ,@body)
       (setf (dotnet:static "DotCL.ConditionSystem" "CountPolls") nil))))

(deftest poll-save-floats.single-loop
  (list (= (%psf-series 5000) (%psf-series-nopoll 5000))
        (= (%psf-counting (%psf-series 5000)) (%psf-series-nopoll 5000)))
  (t t))

(deftest poll-save-floats.double-self-call
  (list (= (%psf-rec 0 5000 0d0 1d0) (%psf-rec-nopoll 0 5000 0d0 1d0))
        (= (%psf-counting (%psf-rec 0 5000 0d0 1d0)) (%psf-rec-nopoll 0 5000 0d0 1d0)))
  (t t))

;; Counting still sees every iteration through the saving slow path.
(deftest poll-save-floats.still-polls
  (%psf-counting
    (let ((before (dotnet:static "DotCL.ConditionSystem" "PollCount")))
      (%psf-series 1000)
      (>= (- (dotnet:static "DotCL.ConditionSystem" "PollCount") before) 1000)))
  t)

(deftest-compiled-only poll-save-floats.emitted
  (list (and (search "PollLoadD" (%flat-text (dotcl:function-sil #'%psf-series))) t)
        (and (search "PollLoadD" (%flat-text (dotcl:function-sil #'%psf-rec))) t)
        (and (search "PollLoadD" (%flat-text (dotcl:function-sil #'%psf-series-nopoll))) t))
  (t t nil))
