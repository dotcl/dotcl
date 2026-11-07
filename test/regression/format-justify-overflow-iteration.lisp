;;; The ~n:; clause of ~<...~> breaks the line when the text would pass the line
;;; width. Inside ~{ ~} each pass builds its text in a buffer of its own, and the
;;; check used to take the start of the pass for column 0, so a loop over a long list
;;; never broke the line.

(defun %fjoi-lines (string)
  (let ((lines '()) (start 0))
    (loop for nl = (position #\Newline string :start start)
          do (push (subseq string start nl) lines)
             (if nl (setf start (1+ nl)) (return)))
    (nreverse lines)))

(defparameter *fjoi-words*
  '(alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu xi omicron pi rho sigma tau
    upsilon phi chi psi omega alpha beta gamma delta epsilon zeta eta theta iota kappa lambda))

(deftest format-justify-overflow-iteration.breaks-the-line
  (let* ((text (format nil "~{~<~%~1:; ~A~>~^,~}" *fjoi-words*))
         (lines (%fjoi-lines text)))
    (list (> (length lines) 1)
          (every (lambda (l) (<= (length l) 73)) lines)
          (string= (remove #\Newline text) (format nil "~{ ~A~^,~}" *fjoi-words*))))
  (t t t))

(deftest format-justify-overflow-iteration.short-list-stays-on-one-line
  (format nil "~{~<~%~1:; ~A~>~^,~}" '(a b c))
  " A, B, C")

(deftest format-justify-overflow-iteration.text-before-the-loop-counts
  (let* ((text (format nil "~A~{~<~%~1:;~A~>~^ ~}" (make-string 60 :initial-element #\x)
                       '(aaaaaaaa bbbbbbbb)))
         (lines (%fjoi-lines text)))
    (list (length lines) (length (first lines))))
  (2 69))

(deftest format-justify-overflow-iteration.without-iteration-unchanged
  (list (format nil "~<~%~1:;~S~>~<~%~1:;~S~>" 'aaaa 'bbbb)
        (format nil "~10<~A~>" "ab"))
  ("AAAABBBB" "        ab"))
