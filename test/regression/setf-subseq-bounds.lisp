;;; (SETF (SUBSEQ seq start end) new): a negative end is an error, not "no end".

(defun %ssb-rejected (thunk) (handler-case (progn (funcall thunk) nil) (error () t)))

(deftest setf-subseq-bounds.negative-end-is-an-error
  (list (%ssb-rejected (lambda () (let ((l (list 1 2 3))) (setf (subseq l 0 -1) (list 9 9 9)) l)))
        (%ssb-rejected (lambda () (let ((v (vector 1 2 3))) (setf (subseq v 0 -1) (list 9 9 9)) v)))
        (%ssb-rejected (lambda () (let ((s (copy-seq "abc"))) (setf (subseq s 1 -2) "zz") s)))
        (%ssb-rejected (lambda () (let ((l (list 1 2 3))) (setf (subseq l 2 1) (list 9 9 9)) l)))
        (%ssb-rejected (lambda () (let ((l (list 1 2 3))) (setf (subseq l -1 2) (list 9 9 9)) l)))
        (%ssb-rejected (lambda () (let ((l (list 1 2 3))) (setf (subseq l 0 (expt 2 70)) (list 9)) l))))
  (t t t t t t))

(deftest setf-subseq-bounds.valid-ranges-still-store
  (list (let ((l (list 1 2 3 4))) (setf (subseq l 1 3) (list 8 9 7)) l)
        (let ((l (list 1 2 3 4))) (setf (subseq l 1) (list 8)) l)
        (let ((l (list 1 2 3 4))) (setf (subseq l 4 4) (list 8)) l)
        (let ((l (list 1 2 3 4))) (setf (subseq l 0 0) (list 8)) l)
        (coerce (let ((v (vector 1 2 3 4))) (setf (subseq v 0 2) (list 5 6 7)) v) 'list)
        (let ((s (copy-seq "abcd"))) (setf (subseq s 2 nil) "xyz") s))
  ((1 8 9 4) (1 8 3 4) (1 2 3 4) (1 2 3 4) (5 6 3 4) "abxy"))
