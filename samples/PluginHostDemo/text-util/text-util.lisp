(defpackage :text-util
  (:use :cl)
  (:export #:split-words #:longest #:capitalize))
(in-package :text-util)

(defun split-words (s)
  (let ((words '()) (start nil))
    (dotimes (i (length s))
      (if (char= (char s i) #\Space)
          (when start (push (subseq s start i) words) (setf start nil))
          (unless start (setf start i))))
    (when start (push (subseq s start) words))
    (nreverse words)))

(defun longest (words)
  (reduce (lambda (a b) (if (> (length b) (length a)) b a)) words :initial-value ""))

(defun capitalize (w)
  (if (zerop (length w))
      w
      (concatenate 'string (string-upcase (subseq w 0 1)) (subseq w 1))))
