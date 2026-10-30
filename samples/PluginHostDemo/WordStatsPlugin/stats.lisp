(in-package :word-stats)

(defun title (words)
  (format nil "~{~A~^ ~}" (mapcar #'capitalize words)))

(defun summary (words)
  (format nil "~D words, longest ~S, ~A" (length words) (longest words) (title words)))
