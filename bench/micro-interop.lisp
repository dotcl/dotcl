;;; bench/micro-interop.lisp -- dotnet:invoke method-resolution cost
;;;
;;; Hot loop of StringBuilder.Append(string) through dotnet:invoke. With the
;;; MethodInfo cache the per-call cost is MethodInfo.Invoke on a resolved method;
;;; without it, every call re-runs InvokeMember name lookup + binder overload
;;; resolution. The cache change is invisible to the emitted SIL, so this bench is
;;; the only place the win shows up.
;;;
;;; Run:
;;;   dotnet run --project runtime -- --asm compiler/cil-out.sil bench/micro-interop.lisp
;;;
;;; min-of-RUNS after WARMUP discards (noise floor ~±30% on 1-shot).

(defvar *reps* 1000000)   ; invoke calls per sample
(defvar *runs* 5)
(defvar *warmup* 2)

;; sb is cleared before each sample so buffer growth doesn't skew timings; the
;; measured work is the per-call dotnet:invoke dispatch + Append.
(defun survey (label sb s)
  (let ((best nil) (samples nil))
    (dotimes (r (+ *warmup* *runs*))
      (dotnet:invoke sb "Clear")
      (let ((start (get-internal-real-time)))
        (dotimes (i *reps*) (dotnet:invoke sb "Append" s))
        (let ((x (float (/ (- (get-internal-real-time) start)
                           internal-time-units-per-second))))
          (when (>= r *warmup*)
            (push x samples)
            (when (or (null best) (< x best)) (setq best x))))))
    (format t "~&~24A  min=~,4F sec  per-call=~,1F ns  samples=~{~,4F~^ ~}~%"
            label best (* (/ best *reps*) 1d9) (nreverse samples))
    best))

(format t "~&;; micro-interop: reps=~D runs=~D warmup=~D~%" *reps* *runs* *warmup*)
(survey "invoke Append(string)"
        (dotnet:new "System.Text.StringBuilder") "x")
(format t ";; done.~%")
