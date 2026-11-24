;;; WRITE-STRING and WRITE-LINE signal a type-error on a stream opened for bytes.

(deftest write-string-binary-stream.byte-file
  (let ((path (merge-pathnames "write-string-binary-stream.tmp" (or *load-truename* *default-pathname-defaults*))))
    (unwind-protect
         (with-open-file (o path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
           (list (handler-case (write-string "ab" o) (type-error () :type-error))
                 (handler-case (write-line "ab" o) (type-error () :type-error))
                 (handler-case (write-string "abcd" o :start 1 :end 2) (type-error () :type-error))
                 (progn (write-byte 65 o) (file-position o))))
      (ignore-errors (delete-file path))))
  (:type-error :type-error :type-error 1))

(deftest write-string-binary-stream.character-file
  (let ((path (merge-pathnames "write-string-binary-stream2.tmp" (or *load-truename* *default-pathname-defaults*))))
    (unwind-protect
         (progn
           (with-open-file (o path :direction :output :if-exists :supersede)
             (write-string "ab" o) (write-line "cd" o))
           (with-open-file (i path) (list (read-line i) (read-line i nil :eof))))
      (ignore-errors (delete-file path))))
  ("abcd" :eof))

(deftest write-string-binary-stream.other-writers
  (let ((path (merge-pathnames "write-string-binary-stream3.tmp" (or *load-truename* *default-pathname-defaults*))))
    (unwind-protect
         (with-open-file (o path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
           (list (handler-case (terpri o) (type-error () :type-error))
                 (handler-case (princ 1 o) (type-error () :type-error))
                 (handler-case (prin1 'a o) (type-error () :type-error))
                 (handler-case (print 'a o) (type-error () :type-error))
                 (handler-case (write 5 :stream o) (type-error () :type-error))
                 (handler-case (format o "z") (type-error () :type-error))
                 (handler-case (write-sequence "xy" o) (type-error () :type-error))
                 (progn (write-sequence (vector 1 2 3) o) (write-byte 4 o) (file-position o))))
      (ignore-errors (delete-file path))))
  (:type-error :type-error :type-error :type-error :type-error :type-error :type-error 4))
