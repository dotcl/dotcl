;;; WITH-OUTPUT-TO-STRING closes the stream it made, and takes the string from it, even when the
;;; body assigns its variable.

(deftest with-output-to-string-assigned-variable.setq-keeps-the-string
  (list (with-output-to-string (s) (let ((original s)) (setf s (make-string-output-stream)) (write-string "q" original)))
        (with-output-to-string (s) (write-string "a" s) (setq s *standard-output*))
        (with-output-to-string (s) (write-string "b" s) (setq s nil))
        (with-output-to-string (s) (write-string "c" s))
        (let ((str (make-array 5 :element-type 'character :fill-pointer 0 :adjustable t)))
          (with-output-to-string (s str) (write-string "xy" s) (setq s nil))
          (coerce str 'list)))
  ("q" "a" "b" "c" (#\x #\y)))

(deftest with-output-to-string-assigned-variable.standard-output-stays-open
  (progn
    (with-output-to-string (s) (setq s *standard-output*))
    (open-stream-p *standard-output*))
  t)
