;;; WRITE-SEQUENCE with NIL / T / a non-stream as the stream argument.

(deftest write-sequence-designators.nil-follows-standard-output
  (with-output-to-string (o)
    (let ((*standard-output* o)) (write-sequence "SEQ" nil) (write-sequence '(#\a #\b) nil)))
  "SEQab")

(deftest write-sequence-designators.non-stream
  (list (handler-case (write-sequence "x" 5) (type-error () :type-error))
        (handler-case (write-sequence "x" 'foo) (type-error () :type-error)))
  (:type-error :type-error))

;;; Character output to a stream that is not an output stream is an error, not output on stdout.
(deftest write-to-non-output-stream.input-and-probe
  (let ((path "test/regression/.tmp-non-output.dat"))
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string "hello" o))
    (unwind-protect
         (list
          (with-open-file (s path :direction :input)
            (list (handler-case (write-char #\X s) (type-error () :type-error))
                  (handler-case (write-string "X" s) (type-error () :type-error))
                  (handler-case (write-sequence "X" s) (type-error () :type-error))))
          (let ((s (open path :direction :probe)))
            (handler-case (write-string "X" s) (type-error () :type-error)))
          (let ((s (open path :direction :input)))
            (close s)
            (handler-case (write-string "X" s) (type-error () :type-error))))
      (delete-file path)))
  ((:type-error :type-error :type-error) :type-error :type-error))
