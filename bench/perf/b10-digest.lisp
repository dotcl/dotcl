(load "bench/perf/pcommon.lisp")
(setf *bench-group* "10-digest")
(load-ql-system :ironclad)
;; ironclad digests of 256 KB; ns/op is per call, divide by 262144 for ns/byte. After the fix for macros that take
;; &environment, dotcl on mac (ns/byte): kupyna 14.4 (SBCL 14.5), blake2s 6.4-7.1 (4.1), streebog 47.7-53.9 (11.7);
;; streebog is the one left: its STREEBOG-G method has about 2000 locals and the JIT tracks 1024. Decision: streebog
;; x(min) >= 2 on the perf machine and the same with DOTNET_JitMaxLocalsToTrack=0x2000 (run this file once with it set) clearly
;; faster -> reuse temporary locals in the compiler.
(defvar *sink* nil)
(defvar *mb* (let ((a (make-array 262144 :element-type '(unsigned-byte 8)))) (dotimes (i (length a) a) (setf (aref a i) (mod (* i 31) 251)))))
(dolist (d '(:blake2s :streebog :kupyna :sha3 :blake2 :sha512))
  (let ((d d))
    (eval `(bench ,(format nil "digest-sequence ~(~A~) 256 KB" d) 10 (setq *sink* (ironclad:digest-sequence ,d *mb*))))))
