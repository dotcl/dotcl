;;; Writing to a closed broadcast stream is a stream error; closing it does not
;;; close the streams it broadcasts to.

(deftest closed-broadcast.write-is-error
  (flet ((try (f) (let ((b (make-broadcast-stream))) (close b)
                    (handler-case (progn (funcall f b) :no-error) (stream-error () :stream-error)))))
    (list (try (lambda (b) (write-string "t" b))) (try (lambda (b) (write-char #\a b)))
          (try (lambda (b) (format b "x"))) (try (lambda (b) (print 1 b)))))
  (:stream-error :stream-error :stream-error :stream-error))

(deftest closed-broadcast.component-stays-open
  (let* ((s (make-string-output-stream)) (b (make-broadcast-stream s)))
    (write-string "t" b) (close b)
    (list (open-stream-p b) (open-stream-p s) (get-output-stream-string s)))
  (nil t "t"))
