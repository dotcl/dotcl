;;;; app.lisp — precompiled to a stable-named .NET IL assembly (appfasl.dll) at
;;;; build time; Unity IL2CPP bakes it into the WebGL build. The host calls these
;;;; from C# every frame to drive an animated curve, and a browser input box can
;;;; EVAL Lisp at run time to change the curve live — emit-free, no recompile.

;; Curve parameters as global special variables. A browser eval like
;; (setf *fx* 7) or (setf *amp* 0.5) mutates these and the animation changes on
;; the next frame, because PX/PY read them fresh each call.
(defparameter *fx* 3.0d0)   ; x angular frequency
(defparameter *fy* 2.0d0)   ; y angular frequency
(defparameter *amp* 0.9d0)  ; amplitude (fraction of the view half-extent)
(defparameter *spin* 1.0d0) ; time-scale multiplier

;; PX/PY: position of point I of N at animation TICK, in [-1,1]^2. Precompiled,
;; so calling them per point per frame is cheap. A Lissajous figure whose lobes
;; are set by *fx*/*fy* and that drifts over time.
(defun px (i n tick)
  (let ((a (/ (* 2 pi (float i 1.0d0)) n)))
    (* *amp* (sin (+ (* *fx* a) (* 0.02d0 *spin* tick))))))

(defun py (i n tick)
  (let ((a (/ (* 2 pi (float i 1.0d0)) n)))
    (* *amp* (sin (+ (* *fy* a) (* 0.013d0 *spin* tick))))))

;; HUE: animated base hue in [0,1) for the line color.
(defun hue (tick) (mod (* 0.004d0 tick) 1.0d0))

;; --- The original proof functions (still exercised once at startup) ---

(defun fib (n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(defun greet (name)
  ;; Lisp -> C#: call back into a function the host registered.
  (host-log (format nil "hello ~a, from precompiled Lisp" name))
  (length name))
