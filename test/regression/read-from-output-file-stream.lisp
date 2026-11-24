;;; Reading from a file stream opened :direction :output signals a TYPE-ERROR
;;; (it is not an input stream), not END-OF-FILE.

(deftest read-from-output-file-stream.read-char
  (let ((s (open (regression-temp-file "rofs.dat") :direction :output :if-exists :supersede)))
    (unwind-protect
         (list (handler-case (read-char s) (type-error () :type-error) (end-of-file () :eof))
               (handler-case (read-char s nil :x) (type-error () :type-error) (end-of-file () :eof))
               (handler-case (read s) (type-error () :type-error) (end-of-file () :eof)))
      (close s)))
  (:type-error :type-error :type-error))
