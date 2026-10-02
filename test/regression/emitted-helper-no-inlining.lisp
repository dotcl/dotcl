;;; The runtime helpers that code compiled in memory (COMPILE, EVAL) calls to
;;; fetch a compilation-unit constant or to build a closure are never inlined.
;;;
;;; A method built in memory is JIT-compiled with full optimization on its
;;; first call, and inlining these helpers pulled the unit table lookup, the
;;; delegate cache and the closure factory into every such method. A COMPILE
;;; of a lambda that makes a closure paid about 2 ms of JIT for that, most of
;;; its cost; code walkers that COMPILE a small test lambda per variable do so
;;; tens of thousands of times while a system loads.

(defun %ehni-flags (name)
  (let* ((type (dotnet:static "System.Type" "GetType"
                              "DotCL.Emitter.CilAssembler, DotCL.Runtime"))
         (mi (dotnet:invoke type "GetMethod" name)))
    (princ-to-string (dotnet:invoke mi "GetMethodImplementationFlags"))))

(deftest-emitting-only emitted-helper-no-inlining.flags
  (mapcar (lambda (name) (not (null (search "NoInlining" (%ehni-flags name)))))
          '("GetUnitConstant" "MakeClosure" "MakeClosureDirect"))
  (t t t))

;; The helpers still do their job: a compiled lambda that builds closures
;; over a fresh binding per call.
(deftest-emitting-only emitted-helper-no-inlining.closures
  (let ((f (compile nil '(lambda (n)
                           (let ((fs '()))
                             (dotimes (i n) (let ((j i)) (push (lambda () j) fs)))
                             (mapcar #'funcall fs))))))
    (funcall f 4))
  (3 2 1 0))
