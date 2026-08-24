;;; bench/fasl-jit-probe.lisp -- what a single (LOAD "x.fasl") actually spends
;;;
;;; Usage (from the project root):
;;;   dotnet run --project runtime/runtime.csproj -- --asm compiler/cil-out.sil \
;;;     --eval '(defparameter cl-user::*probe-fasl* "build/x.fasl")' \
;;;     --load bench/fasl-jit-probe.lisp
;;;
;;; Companion to fasl-load-probe.lisp, which answers "which FILE costs the RSS".
;;; This one answers "what is the cost MADE OF" for one file, and it splits the
;;; load into the two things that can be traded against each other:
;;;
;;;   JIT     time spent compiling IL to machine code, plus the IL bytes and
;;;           method count that went through the JIT (System.Runtime.JitInfo).
;;;           A fasl builds its literals with IL, so every constant it holds is
;;;           code that must be JITted before it can run once.
;;;   data    the objects that survive the load (GC live bytes after a forced
;;;           collection), and the memory that is committed but not GC heap
;;;           (working set minus GC committed: code heap, loader heap, the
;;;           mapped assembly).
;;;
;;; The ratio decides whether moving literals from code to data can pay: if the
;;; JIT side is a minority of the load, the cost is the assembly work itself or
;;; the live data, and a different literal FORMAT can only move a constant
;;; factor. Measure before designing.
;;;
;;; Read the numbers as a comparison between two fasls, not as absolutes: the
;;; process has already JITted the compiler and the standard library before the
;;; probe starts, and the deltas include whatever the loaded code touches for
;;; the first time.

(defvar *probe-fasl* nil
  "Path of the .fasl to load. Set it before loading this file.")

(defun %jit-il-bytes () (dotnet:static "System.Runtime.JitInfo" "GetCompiledILBytes"))
(defun %jit-methods () (dotnet:static "System.Runtime.JitInfo" "GetCompiledMethodCount"))
(defun %jit-ms ()
  (dotnet:invoke (dotnet:static "System.Runtime.JitInfo" "GetCompilationTime")
                 "TotalMilliseconds"))
(defun %working-set ()
  (dotnet:invoke (dotnet:static "System.Diagnostics.Process" "GetCurrentProcess")
                 "WorkingSet64"))
(defun %gc-committed ()
  (dotnet:invoke (dotnet:static "System.GC" "GetGCMemoryInfo") "TotalCommittedBytes"))
(defun %gc-live ()
  ;; T = collect first, so this is live data rather than allocation high-water.
  (dotnet:static "System.GC" "GetTotalMemory" t))

(defun %mb (bytes) (/ (float bytes 1d0) 1048576d0))

(defun probe-fasl (path)
  (let ((il0 (%jit-il-bytes)) (m0 (%jit-methods)) (jit0 (%jit-ms))
        (live0 (%gc-live)) (ws0 (%working-set)) (gcc0 (%gc-committed))
        (t0 (get-internal-real-time)))
    (load path)
    (let* ((wall (/ (float (- (get-internal-real-time) t0) 1d0)
                    internal-time-units-per-second))
           (jit (- (%jit-ms) jit0))
           (il (- (%jit-il-bytes) il0))
           (methods (- (%jit-methods) m0))
           (live (- (%gc-live) live0))
           (ws (- (%working-set) ws0))
           (gcc (- (%gc-committed) gcc0)))
      (format t "~&~%=== ~a ===~%" path)
      (format t "  fasl bytes         ~12:D~%"
              (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s)))
      (format t "  load wall          ~12,3F s~%" wall)
      (format t "  JIT time           ~12,3F s   ~5,1F%% of load~%"
              (/ jit 1000d0) (if (plusp wall) (* 100d0 (/ (/ jit 1000d0) wall)) 0))
      (format t "  JIT'd IL           ~12:D bytes  in ~:D methods~%" il methods)
      (format t "  live data (GC)     ~12:D bytes  ~,1F MB~%" live (%mb live))
      (format t "  working set delta  ~12:D bytes  ~,1F MB~%" ws (%mb ws))
      (format t "  GC committed delta ~12:D bytes  ~,1F MB~%" gcc (%mb gcc))
      (format t "  non-GC delta       ~12:D bytes  ~,1F MB  (code + loader heap + image)~%"
              (- ws gcc) (%mb (- ws gcc)))
      (values))))

(if *probe-fasl*
    (probe-fasl *probe-fasl*)
    (format *error-output* "~&set *probe-fasl* to the .fasl to measure~%"))
