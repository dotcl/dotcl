(load "bench/perf/pcommon.lisp")
(setf *bench-group* "26-flexi-io")
;; flexi-streams in-memory streams: READ-LINE over 20000 lines, WRITE-STRING into an octet vector, READ-SEQUENCE.
;; Libraries to install on the runner: flexi-streams
(load-ql-system :flexi-streams)
(defvar *sink* nil)
(defvar *octets* (flexi-streams:string-to-octets
                  (with-output-to-string (s) (dotimes (i 20000) (format s "line number ~D of text~%" i)))
                  :external-format :utf-8))
(bench "flexi read-line x20000 from octet stream" 10
  (flexi-streams:with-input-from-sequence (in *octets*)
    (let ((s (flexi-streams:make-flexi-stream in :external-format :utf-8)) (n 0))
      (loop for l = (read-line s nil nil) while l do (incf n))
      (setq *sink* n))))
(bench "flexi write-string x20000 to octet vector" 10
  (setq *sink* (flexi-streams:with-output-to-sequence (out)
                 (let ((s (flexi-streams:make-flexi-stream out :external-format :utf-8)))
                   (dotimes (i 20000) (write-string "line number of text" s) (write-char #\Newline s))))))
(bench "flexi read-sequence 64 KB chunks" 20
  (flexi-streams:with-input-from-sequence (in *octets*)
    (let ((buf (make-array 65536 :element-type '(unsigned-byte 8))))
      (loop for n = (read-sequence buf in) while (plusp n))
      (setq *sink* buf))))
