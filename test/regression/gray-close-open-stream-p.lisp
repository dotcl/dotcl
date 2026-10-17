;;; CLOSE on a Gray stream without its own CLOSE method makes OPEN-STREAM-P
;;; false (the Gray proposal's default methods on FUNDAMENTAL-STREAM).
(require "dotcl-gray")

(defclass %gco-in (dotcl-gray:fundamental-character-input-stream) ())
(defclass %gco-out (dotcl-gray:fundamental-binary-output-stream) ())

(deftest gray-close-open-stream-p.default
  (let ((a (make-instance '%gco-in))
        (b (make-instance '%gco-in))
        (c (make-instance '%gco-out)))
    (list (open-stream-p a) (close a) (open-stream-p a) (close a)
          (open-stream-p b) (close c) (open-stream-p c)))
  (t t nil t t t nil))
