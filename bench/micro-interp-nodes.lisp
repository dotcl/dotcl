;;; bench/micro-interp-nodes.lisp -- per-node cost of the tree-walk interpreter
;;;
;;; %MINI-EVAL is the evaluator every emit-free target runs on (NativeAOT,
;;; IL2CPP, WASM), so its per-node cost is that of the whole product there.
;;; This measures one node at a time, driven from COMPILED code so the harness
;;; itself costs nothing, plus the empty-DOTIMES figure the interpreter work is
;;; usually quoted by.
;;;
;;; Run:
;;;   dotnet run --project runtime -- --asm compiler/cil-out.sil bench/micro-interp-nodes.lisp
;;;
;;; A/B between two compiler images (the only way to attribute a change):
;;;   runtime.exe --asm base.sil bench/micro-interp-nodes.lisp
;;;   runtime.exe --asm new.sil  bench/micro-interp-nodes.lisp
;;; alternating, several times. Do NOT run the two back to back once and
;;; compare -- cross-process spread on a loaded machine is ~10%.
;;;
;;; One row per process, when a row has to be trusted on its own:
;;;   runtime.exe --asm X.sil --eval '(defparameter cl-user::*only* "dotimes")' \
;;;               bench/micro-interp-nodes.lisp
;;; Rows can contaminate each other: on images predating the inline symbol
;;; comparison, measuring the CASE rows after the node rows inflated them 3x.
;;;
;;; Timing needs GET-INTERNAL-REAL-TIME to actually resolve below a millisecond.
;;; It does since the internal time unit became microseconds off the
;;; high-resolution counter; before that it stepped 15.6 ms at a time and every
;;; row here was quantization noise.

(defvar *reps* 300000)   ; node evaluations per sample
(defvar *runs* 5)
(defvar *warmup* 2)
(defvar *only* nil)      ; substring of a row label, or NIL for all rows

(defun %min-clock-ticks ()
  "Smallest observable step of the clock, in internal time units."
  (let ((t0 (get-internal-real-time)))
    (loop for now = (get-internal-real-time)
          until (> now t0)
          finally (return (- now t0)))))

(defun run-sample (thunk)
  (let* ((b0 (dotcl:gc-stats))
         (start (get-internal-real-time))
         (ignore (funcall thunk))
         (dt (- (get-internal-real-time) start))
         (b1 (dotcl:gc-stats)))
    (declare (ignore ignore))
    (values dt (- (nth 4 b1) (nth 4 b0)))))

(defun survey (label n thunk)
  "Report min-of-*RUNS* per unit, in ns, plus bytes from the best run."
  (when (or (null *only*) (search *only* label))
    (let ((best nil) (best-bytes 0))
      (dotimes (r (+ *warmup* *runs*))
        (multiple-value-bind (dt bytes) (run-sample thunk)
          (when (and (>= r *warmup*) (or (null best) (< dt best)))
            (setq best dt best-bytes bytes))))
      (format t "~&~30A ~9,1F ns  ~8,1F B  (~D ticks)~%"
              label
              (/ (* best 1d9) internal-time-units-per-second n)
              (/ (float best-bytes) n)
              best)
      (finish-output)
      best)))

;;; --- the evaluator under test ---

(defun me (form) (dotcl.cil-compiler::%mini-eval form nil))

(defun noop () nil)

(defun node-row (label form)
  (survey label *reps* (lambda () (dotimes (i *reps*) (me form)))))

;;; --- CASE dispatch: cost per clause walked past ---
;;; %MINI-EVAL dispatches special forms through a CASE whose default -- the
;;; function-call path, the most frequent one -- is reached only after every
;;; clause has been tested. These rows give the slope in ns per clause.

(defmacro def-miss-dispatch (name n)
  "A CASE with N symbol keys, none of which can match, then a default."
  (let ((keys (loop for i from 1 to n collect (intern (format nil "MISS-KEY-~D" i)))))
    `(defun ,name (x)
       (case x ,@(loop for k in keys collect `(,k :hit)) (t :default)))))

(def-miss-dispatch dispatch-2 2)
(def-miss-dispatch dispatch-8 8)
(def-miss-dispatch dispatch-16 16)
(def-miss-dispatch dispatch-24 24)
(def-miss-dispatch dispatch-32 32)

(defun dispatch-row (label fn)
  (let ((probe 'no-such-operator))
    (survey label *reps* (lambda () (dotimes (i *reps*) (funcall fn probe))))))

;;; --- report ---

(format t "~&;; micro-interp-nodes: reps=~D runs=~D warmup=~D~%" *reps* *runs* *warmup*)
(format t ";; internal-time-units-per-second=~D  clock step=~D unit(s)~%"
        internal-time-units-per-second (%min-clock-ticks))
(when *only* (format t ";; only rows matching ~S~%" *only*))

;; Nodes, cheapest first: the first row is the per-node preamble with no work
;; under it at all.
(node-row "node nil (preamble)" nil)
(node-row "node (quote a)" ''a)
(node-row "node (progn nil)" '(progn nil))
(node-row "node (noop)" '(noop))
(node-row "node (+ 1 2)" '(+ 1 2))

;; CASE slope. Subtract adjacent rows to get ns per clause; the relation should
;; be linear, and a non-linear one means the row order is contaminating.
(dispatch-row "case 2 keys -> default" #'dispatch-2)
(dispatch-row "case 8 keys -> default" #'dispatch-8)
(dispatch-row "case 16 keys -> default" #'dispatch-16)
(dispatch-row "case 24 keys -> default" #'dispatch-24)
(dispatch-row "case 32 keys -> default" #'dispatch-32)

;; The headline: an empty interpreted DOTIMES, per iteration.
(let ((iters 100000))
  (survey "dotimes empty (per iter)" iters
          (lambda () (me (list 'dotimes (list 'i iters))))))

(format t ";; done.~%")
