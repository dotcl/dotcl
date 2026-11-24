;;; A subscript that does not fit in 32 bits is out of bounds: it must not wrap into range and
;;; store into another element.

(defun %sib-set-schar (s i c) (setf (schar s i) c))
(defun %sib-set-char (s i c) (setf (char s i) c))
(defun %sib-set-svref (v i x) (setf (svref v i) x))
(defun %sib-set-elt (seq i x) (setf (elt seq i) x))

(defun %sib-rejected (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(deftest setf-index-beyond-int.out-of-range-signals-and-changes-nothing
  (let ((results nil))
    (dolist (i (list 4 -1 (expt 2 31) (+ (expt 2 31) 1) (expt 2 32) (+ (expt 2 32) 1) (expt 2 40) (+ (expt 2 40) 3)))
      (let ((s (make-string 4 :initial-element #\a))
            (lit (copy-seq "abcd"))
            (v (vector 1 2 3 4))
            (l (list 1 2 3 4)))
        (push (list (%sib-rejected (lambda () (%sib-set-schar s i #\x)))
                    (%sib-rejected (lambda () (%sib-set-schar lit i #\x)))
                    (%sib-rejected (lambda () (%sib-set-char s i #\y)))
                    (%sib-rejected (lambda () (%sib-set-elt s i #\z)))
                    (%sib-rejected (lambda () (%sib-set-svref v i 9)))
                    (%sib-rejected (lambda () (%sib-set-elt v i 9)))
                    (%sib-rejected (lambda () (%sib-set-elt l i 9)))
                    (equal (list s lit (coerce v 'list) l)
                           (list "aaaa" "abcd" '(1 2 3 4) '(1 2 3 4))))
              results)))
    (every (lambda (r) (every #'identity r)) results))
  t)

(deftest setf-index-beyond-int.in-range-still-stores
  (let ((s (make-string 4 :initial-element #\a)) (lit (copy-seq "abcd")) (v (vector 1 2 3 4)) (l (list 1 2 3 4))
        (adj (make-array 4 :element-type 'character :adjustable t :initial-element #\a)))
    (%sib-set-schar s 3 #\x) (%sib-set-schar lit 0 #\y) (%sib-set-svref v 2 9) (%sib-set-elt l 3 8)
    (%sib-set-elt s 0 #\q) (%sib-set-elt v 0 7) (%sib-set-char adj 1 #\w)
    (list s lit (coerce v 'list) l (coerce adj 'string)))
  ("qaax" "ybcd" (7 2 9 4) (1 2 3 8) "awaa"))
