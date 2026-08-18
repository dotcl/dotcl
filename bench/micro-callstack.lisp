;;; bench/micro-callstack.lisp -- isolate named-call (Track / s_callStack) overhead
;;;
;;; Measures the per-call cost of LispFunction.InvokeN -> Track push/pop.
;;; Uses a CONS argument so the call goes through the generic InvokeN path
;;; (fixnum args would take InvokeNativeN, which bypasses Track entirely).
;;;
;;; Run:
;;;   dotnet run --project runtime -- --asm compiler/cil-out.sil bench/micro-callstack.lisp
;;;
;;; min-of-RUNS after WARMUP discards (noise floor ~±30% on 1-shot).

(defvar *reps* 20000000)   ; calls per sample
(defvar *runs* 5)
(defvar *warmup* 2)

;; Named leaf function: returns its arg. Goes through InvokeN (generic) when
;; called with a non-fixnum (cons) argument -> hits Track push/pop.
(defun leaf-call (x) x)

;; Control: same shape but we never call a named fn in the hot loop, so no Track.
(defun bench-named (n obj)
  (let ((acc obj))
    (dotimes (i n) (setq acc (leaf-call obj)))
    acc))

(defun bench-noop (n obj)
  (let ((acc obj))
    (dotimes (i n) (setq acc obj))
    acc))

(defun run-sample (fn n obj)
  (let ((start (get-internal-real-time)))
    (funcall fn n obj)
    (float (/ (- (get-internal-real-time) start)
              internal-time-units-per-second))))

(defun survey (label fn)
  (let ((obj (list 'a))
        (best nil)
        (samples nil))
    (dotimes (r (+ *warmup* *runs*))
      (let ((s (run-sample fn *reps* obj)))
        (when (>= r *warmup*)
          (push s samples)
          (when (or (null best) (< s best)) (setq best s)))))
    (format t "~&~20A  min=~,4F sec  per-call=~,2F ns  samples=~{~,4F~^ ~}~%"
            label best
            (* (/ best *reps*) 1d9)
            (nreverse samples))
    best))

(format t "~&;; micro-callstack: reps=~D runs=~D warmup=~D~%" *reps* *runs* *warmup*)
(let ((noop  (survey "noop (no call)" #'bench-noop))
      (named (survey "named (Track)"  #'bench-named)))
  (format t "~&;; call overhead = ~,2F ns/call (named - noop)~%"
          (* (/ (- named noop) *reps*) 1d9)))
(format t ";; done.~%")
