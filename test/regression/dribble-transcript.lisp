;;; DRIBBLE records what *STANDARD-OUTPUT* receives (and what *STANDARD-INPUT* delivers) in the
;;; file until (dribble) ends it. It used to do nothing and create no file.

(deftest dribble-transcript.records-output
  (let* ((p (format nil "~A/dribble-test.txt" (regression-temp-dir)))
         (lines nil))
    (ignore-errors (delete-file p))
    (unwind-protect
         (let ((*standard-output* (make-string-output-stream)))
           (dribble p)
           (write-string "hello" *standard-output*)
           (terpri)
           (format t "second ~D~%" 2)
           (dribble)
           (write-string "not recorded")
           (setq lines (with-open-file (s p)
                         (loop for l = (read-line s nil) while l collect l))))
      (ignore-errors (delete-file p)))
    lines)
  ("hello" "second 2"))

(deftest dribble-transcript.state-errors
  (let ((p (format nil "~A/dribble-test2.txt" (regression-temp-dir))))
    (unwind-protect
         (let ((*standard-output* (make-string-output-stream)))
           (list (handler-case (dribble) (error () :not-dribbling))
                 (progn (dribble p) :started)
                 (handler-case (dribble p) (error () :already))
                 (progn (dribble) :stopped)))
      (ignore-errors (delete-file p))))
  (:not-dribbling :started :already :stopped))
