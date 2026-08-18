;;; bench/fasl-load-probe.lisp -- per-file RSS and wall time while an ASDF system loads
;;;
;;; Usage (from the project root):
;;;   dotnet run --project runtime/runtime.csproj -- --asm compiler/cil-out.sil \
;;;     --eval '(defparameter cl-user::*probe-system* "alexandria")' \
;;;     --load bench/fasl-load-probe.lisp
;;;
;;; Run it under a memory cap, always. An unconstrained run of a large system
;;; invites the global OOM killer, which takes unrelated processes with it:
;;;   systemd-run --user --scope -p MemoryMax=4G -p MemorySwapMax=256M -- <cmd>
;;;
;;; And set these, or roughly 70% of what you measure is glibc arena high-water
;;; rather than anything the implementation did:
;;;   MALLOC_ARENA_MAX=2 MALLOC_TRIM_THRESHOLD_=131072 MALLOC_MMAP_THRESHOLD_=131072
;;;
;;; Why per-file: the cost of a fasl is paid when it is LOADED, not when it is
;;; compiled. Splitting one file's definitions into N blocks has been measured to
;;; leave total fasl bytes and compile time unchanged while moving load from a
;;; 4 GB out-of-memory kill to 162 MB. A whole-run total hides that; the delta
;;; per component is what shows which file is the problem.
;;;
;;; Output is one line per operation:
;;;   #M <elapsed-s> <op> rss=<kB> drss=<kB since previous line> <component>
;;; The drss on a line is the cost of the work between the PREVIOUS line and this
;;; one — so the load cost of a file appears on the line after its LOAD-OP.
;;;
;;; Cold and warm runs differ by an order of magnitude per file (one component
;;; measured +2.74 GB cold and +175 MB warm, because a cold run compiles as it
;;; goes). Compare cold with cold and warm with warm, never across.

(require "quicklisp")

(defvar *probe-system* "alexandria"
  "ASDF/Quicklisp system to load. Override before loading this file.")

(defvar *probe-t0* (get-internal-real-time))
(defvar *probe-last-rss* 0)

(defun probe-rss-kb ()
  "Resident set size in kB, or -1 where /proc is not available."
  (handler-case
      (with-open-file (s "/proc/self/statm" :if-does-not-exist nil)
        (if s
            (let ((total (read s)) (resident (read s)))
              (declare (ignore total))
              (* resident 4))
            -1))
    (error () -1)))

(defun probe-line (tag name)
  (let* ((rss (probe-rss-kb))
         (delta (- rss *probe-last-rss*)))
    (setf *probe-last-rss* rss)
    (format t "~&#M ~,1F ~A rss=~D drss=~D ~A~%"
            (/ (float (- (get-internal-real-time) *probe-t0*))
               internal-time-units-per-second)
            tag rss delta name)
    (finish-output)))

(defmethod asdf:perform :before ((op asdf:operation) (c asdf:cl-source-file))
  (probe-line (string (type-of op)) (asdf:component-name c)))

;;; A load that dies takes its backtrace with it unless we catch it here.
(setf *debugger-hook*
      (lambda (c hook)
        (declare (ignore hook))
        (format t "~&;; DIED ~A: ~A~%" (type-of c) c)
        (format t ";; compile-file-truename=~S load-truename=~S~%"
                *compile-file-truename* *load-truename*)
        (probe-line "AT-DEATH" "-")
        (format t ";; backtrace:~%~{;;   ~A~%~}" (ignore-errors (dotcl:backtrace)))
        (finish-output)
        (dotcl:quit 1)))

(probe-line "START" *probe-system*)
(handler-bind ((warning #'muffle-warning))
  (funcall (read-from-string "ql:quickload") *probe-system*))
(probe-line "DONE" *probe-system*)
(format t "~&;; LOADED ~A~%" *probe-system*)
(finish-output)
