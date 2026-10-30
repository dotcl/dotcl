;;; A LispObject temporary bound to an integer literal and read only to be
;;; unboxed (Coalton binds each operand of its arithmetic so) is replaced by the
;;; literal at the read: no box is stored and read back on every iteration.

(setf dotcl:*save-sil* t)

(defun %cfl-sum (n)
  (declare (fixnum n))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n s)
      (let ((b 3))
        (setq s (+ s b))))))

;; B is also returned boxed: it has to stay.
(defun %cfl-kept (n)
  (declare (fixnum n))
  (let ((s 0) (b 3))
    (declare (fixnum s))
    (dotimes (i n)
      (setq s (+ s b)))
    (list s b)))

(deftest constant-fixnum-local.values
  (list (%cfl-sum 10) (%cfl-kept 10))
  (30 (30 3)))

(defun %cfl-count (needle fn)
  (let ((text (%flat-text (dotcl:function-sil fn))) (n 0) (start 0))
    (loop for p = (search needle text :start2 start)
          while p do (incf n) (setf start (1+ p)))
    n))

;; One Fixnum.Make left: the boxing of the result.
(deftest-compiled-only constant-fixnum-local.no-box-in-loop
  (%cfl-count "Fixnum.Make" #'%cfl-sum)
  1)
