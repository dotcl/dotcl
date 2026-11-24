;;; A file output stream buffers 64 KB before it reaches the file: text written in
;;; many small pieces must arrive complete and in order after CLOSE, and be visible
;;; to another stream after FINISH-OUTPUT.

(deftest file-output-large-buffer.small-writes-complete
  (let ((path (regression-temp-file (format nil "dotcl-fob-~D.txt" (random 1000000)))))
    (unwind-protect
         (progn
           (with-open-file (o path :direction :output :if-exists :supersede)
             (dotimes (i 30000) (format o "line ~D~%" i)))
           (with-open-file (in path)
             (let ((n 0) (last nil))
               (loop for l = (read-line in nil nil) while l do (incf n) (setf last l))
               (list n last (with-open-file (s path) (file-length s))))))
      (delete-file path)))
  (30000 "line 29999" 318890))

(deftest file-output-large-buffer.finish-output-makes-data-visible
  (let ((path (regression-temp-file (format nil "dotcl-fob2-~D.txt" (random 1000000)))))
    (unwind-protect
         (with-open-file (o path :direction :output :if-exists :supersede)
           (write-string "hello" o)
           (finish-output o)
           (with-open-file (in path) (values (read-line in nil nil))))
      (delete-file path)))
  "hello")
