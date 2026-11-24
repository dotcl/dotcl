;;; READ-SEQUENCE into an (unsigned-byte 8) vector reads straight from the file
;;; instead of one boxed element at a time: partial fills, :start/:end, the end of
;;; the file, and interleaving with READ-BYTE must keep the per-element results.

(defun %rsob-file (n)
  (let ((path (regression-temp-file (format nil "dotcl-rsob-~D-~D.bin" (random 1000000) n))))
    (with-open-file (o path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (dotimes (i n) (write-byte (mod (* i 7) 256) o)))
    path))

(deftest read-sequence-octets-bulk.full-and-short
  (let ((path (%rsob-file 10)))
    (unwind-protect
         (with-open-file (in path :element-type '(unsigned-byte 8))
           (let* ((buf (make-array 16 :element-type '(unsigned-byte 8) :initial-element 255))
                  (n (read-sequence buf in)))
             (list n (coerce (subseq buf 0 12) 'list) (read-sequence buf in))))
      (delete-file path)))
  (10 (0 7 14 21 28 35 42 49 56 63 255 255) 0))

(deftest read-sequence-octets-bulk.start-end-and-read-byte
  (let ((path (%rsob-file 20)))
    (unwind-protect
         (with-open-file (in path :element-type '(unsigned-byte 8))
           (let* ((buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 9))
                  (first (read-byte in))
                  (n (read-sequence buf in :start 2 :end 6))
                  (next (read-byte in)))
             (list first n (coerce buf 'list) next)))
      (delete-file path)))
  (0 6 (9 9 7 14 21 28 9 9) 35))

(deftest read-sequence-octets-bulk.large-block
  (let ((path (%rsob-file 3000000)))
    (unwind-protect
         (with-open-file (in path :element-type '(unsigned-byte 8))
           (let ((buf (make-array 1048576 :element-type '(unsigned-byte 8))) (total 0) (sum 0))
             (loop for n = (read-sequence buf in) while (plusp n)
                   do (incf total n) (incf sum (aref buf (1- n))))
             (list total sum)))
      (delete-file path)))
  (3000000 #.(let ((sum 0) (pos 0))
               (loop for n in '(1048576 1048576 902848)
                     do (incf pos n) (incf sum (mod (* (1- pos) 7) 256)))
               sum)))
