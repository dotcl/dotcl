;;; WRITE-CHAR to a file opened for bytes is a TYPE-ERROR; a file opened with
;;; :element-type :default takes WRITE-BYTE as well as WRITE-CHAR (as READ-BYTE already worked
;;; on it), in the order they were written.

(defun %bfwc-path () (format nil "~A/bfwc-test.bin" (regression-temp-dir)))

(deftest binary-file-write-char.byte-stream-rejects-char
  (let ((p (%bfwc-path)))
    (unwind-protect
         (list (handler-case
                   (with-open-file (s p :direction :output :if-exists :supersede
                                        :element-type '(unsigned-byte 8))
                     (write-char #\a s) :no-error)
                 (type-error () :type-error))
               (with-open-file (s p :direction :output :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
                 (write-byte 65 s) 65)
               (with-open-file (s p :direction :output :if-exists :supersede)
                 (write-char #\a s) (write-string "bc" s) :ok))
      (ignore-errors (delete-file p))))
  (:type-error 65 :ok))

(deftest binary-file-write-char.default-element-type
  (let ((p (%bfwc-path)))
    (unwind-protect
         (progn
           (with-open-file (s p :direction :output :if-exists :supersede :element-type :default)
             (write-char #\a s) (write-byte 66 s) (write-char #\c s) (write-byte 68 s))
           (with-open-file (s p :direction :input :element-type '(unsigned-byte 8))
             (list (read-byte s) (read-byte s) (read-byte s) (read-byte s))))
      (ignore-errors (delete-file p))))
  (97 66 99 68))
