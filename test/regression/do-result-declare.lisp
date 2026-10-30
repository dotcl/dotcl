;;; A DECLARE is not a form, so it cannot be a result form of DOTIMES, DOLIST, DO or DO*.

(defun do-result-declare-error (form)
  (handler-case (progn (eval form) :no-error)
    (program-error () :program-error)
    (error (c) (type-of c))))

(deftest do-result-declare.dotimes
  (do-result-declare-error '(dotimes (i 3 (declare (fixnum i)))))
  :program-error)

(deftest do-result-declare.dolist
  (do-result-declare-error '(dolist (x '(1 2) (declare (ignore x)))))
  :program-error)

(deftest do-result-declare.do
  (list (do-result-declare-error '(do ((i 0 (1+ i))) ((>= i 3) (declare (fixnum i)))))
        (do-result-declare-error '(do ((i 0 (1+ i))) ((>= i 3) 5 (declare (fixnum i))))))
  (:program-error :program-error))

(deftest do-result-declare.do-star
  (do-result-declare-error '(do* ((i 0 (1+ i))) ((>= i 3) (declare (fixnum i)))))
  :program-error)

(deftest do-result-declare.body-declarations
  (list (let (r) (dotimes (i 3 (nreverse r)) (declare (fixnum i)) (push i r)))
        (let (r) (dolist (x '(1 2) (nreverse r)) (declare (fixnum x)) (push x r)))
        (do ((i 0 (1+ i)) (r nil (cons i r)))
            ((>= i 3) r)
          (declare (fixnum i)))
        (do* ((i 0 (1+ i)) (r nil (cons i r)))
             ((>= i 3) r)
          (declare (fixnum i))))
  ((0 1 2) (1 2) (2 1 0) (3 2 1)))
