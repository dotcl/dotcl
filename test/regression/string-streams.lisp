;;; Regression tests for string-input-stream file-position.
;;; (setf file-position) on string-input-streams was a no-op (returned NIL
;;; without repositioning). Also the getter must return the correct position.

;; string-input-stream repositioning via (setf file-position)
(deftest string-stream-file-position-setf
  (let ((s (make-string-input-stream "hello")))
    (read-char s)
    (file-position s 0)       ; returns new position
    (read-char s))            ; reads from start
  #\h)

(deftest string-stream-file-position-setf-return
  (let ((s (make-string-input-stream "hello")))
    (read-char s)
    (file-position s 0))
  0)

(deftest string-stream-file-position-getter
  (let ((s (make-string-input-stream "hello")))
    (read-char s)
    (read-char s)
    (file-position s))
  2)
