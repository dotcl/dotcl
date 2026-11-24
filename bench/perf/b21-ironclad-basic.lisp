(load "bench/perf/pcommon.lisp")
(setf *bench-group* "21-ironclad-basic")
;; ironclad sha256 / md5 / hmac / aes-cbc / aes-ctr / crc32 on 256 KB; ns/op is per call.
;; Libraries to install on the runner: ironclad
(load-ql-system :ironclad)
(defvar *sink* nil)
(defvar *mb* (let ((a (make-array 262144 :element-type '(unsigned-byte 8)))) (dotimes (i (length a) a) (setf (aref a i) (mod (* i 31) 251)))))
(defvar *key* (make-array 32 :element-type '(unsigned-byte 8) :initial-element 7))
(defvar *iv* (make-array 16 :element-type '(unsigned-byte 8) :initial-element 3))
(bench "ironclad sha256 256 KB" 10 (setq *sink* (ironclad:digest-sequence :sha256 *mb*)))
(bench "ironclad md5 256 KB" 10 (setq *sink* (ironclad:digest-sequence :md5 *mb*)))
(bench "ironclad crc32 256 KB" 10 (setq *sink* (ironclad:digest-sequence :crc32 *mb*)))
(bench "ironclad hmac-sha256 256 KB" 10
  (let ((h (ironclad:make-hmac *key* :sha256)))
    (ironclad:update-hmac h *mb*)
    (setq *sink* (ironclad:hmac-digest h))))
(bench "ironclad aes-256-cbc encrypt 256 KB" 10
  (let ((c (ironclad:make-cipher :aes :key *key* :mode :cbc :initialization-vector *iv*))
        (out (make-array 262144 :element-type '(unsigned-byte 8))))
    (ironclad:encrypt c *mb* out)
    (setq *sink* out)))
(bench "ironclad aes-256-ctr encrypt 256 KB" 10
  (let ((c (ironclad:make-cipher :aes :key *key* :mode :ctr :initialization-vector *iv*))
        (out (make-array 262144 :element-type '(unsigned-byte 8))))
    (ironclad:encrypt c *mb* out)
    (setq *sink* out)))
(bench "ironclad chacha encrypt 256 KB" 10
  (let ((c (ironclad:make-cipher :chacha :key *key* :mode :stream :initialization-vector (subseq *iv* 0 8)))
        (out (make-array 262144 :element-type '(unsigned-byte 8))))
    (ironclad:encrypt c *mb* out)
    (setq *sink* out)))
