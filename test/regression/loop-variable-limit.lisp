;;; LOOP counting from a fixnum variable to a fixnum variable limit gets an
;;; int64 slot when nothing after the loop reads the variable. The last step
;;; can leave int64 when the limit is at the edge; it then ends the loop, which
;;; is what the end test would have done. Values were checked against SBCL with
;;; (signed-byte 64) for fixnum.

(setf dotcl:*save-sil* t)

(defun %lvl-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %lvl-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (1+ q))) (return k))))))

(defun %lvl-to (k n)
  (declare (fixnum k n))
  (let ((c 0) (s 0))
    (declare (fixnum c s))
    (loop for i from k to n do (incf c) (setq s (logxor s i)))
    (list c s)))

(defun %lvl-downto (k n)
  (declare (fixnum k n))
  (let ((c 0) (s 0))
    (declare (fixnum c s))
    (loop for i from n downto k do (incf c) (setq s (logxor s i)))
    (list c s)))

(defun %lvl-by (k n)
  (declare (fixnum k n))
  (loop for i from k upto n by 3 collect (- i k)))

(defun %lvl-down-by (k n)
  (declare (fixnum k n))
  (loop for i downfrom n to k by 2 collect (- n i)))

(defun %lvl-sum (k n)
  (declare (fixnum k n))
  (loop for i from k to n sum i))

(defun %lvl-two (k n)
  (declare (fixnum k n))
  (loop for i from k to n for j from 0 collect j))

(defun %lvl-after-list (k n l)
  (declare (fixnum k n))
  (loop for x in l for i from k to n collect (cons x (- i k))))

(defun %lvl-finish (k n)
  (declare (fixnum k n))
  (let ((c 0))
    (declare (fixnum c))
    (loop for i from k to n do (incf c) (when (= c 2) (loop-finish))
          finally (return c))))

(defun %lvl-final (k n)
  (declare (fixnum k n))
  (loop for i from k to n finally (return i)))

(defmacro %lvl-i () 'i)

(defun %lvl-final-macro (k n)
  (declare (fixnum k n))
  (loop for i from k to n finally (return (%lvl-i))))

(defun %lvl-captured (k n)
  (declare (fixnum k n))
  (let ((f nil))
    (loop for i from k to n do (setq f (lambda () i)))
    (funcall f)))

(defun %lvl-typed (k n)
  (declare (fixnum k n))
  (let ((c 0))
    (declare (fixnum c))
    (loop for i of-type integer from k to n do (incf c))
    c))

;;; BY a variable: LOOP checks it is a positive real, as SBCL does.

(defun %lvl-varby (k n d)
  (declare (fixnum k n d))
  (loop for i from k below n by d collect (- i k)))

(defun %lvl-varby-to (k n d)
  (declare (fixnum k n d))
  (let ((c 0) (s 0))
    (declare (fixnum c s))
    (loop for i from k to n by d do (incf c) (setq s (logxor s i)))
    (list c s)))

(defun %lvl-varby-down (k n d)
  (declare (fixnum k n d))
  (loop for i from n downto k by d collect (- n i)))

(defun %lvl-varby-any (k n d)
  (loop for i from k to n by d collect i))

(defun %lvl-by-error (d)
  (handler-case (loop for i from 0 below 5 by d collect i)
    (type-error (e) (list :type-error (type-error-datum e)))))

(deftest loop-variable-limit.by-variable
  (list (%lvl-varby 0 10 3) (%lvl-varby 0 10 100) (%lvl-varby 5 5 1)
        (%lvl-varby-to 1 10 4)
        (%lvl-varby-to (- most-positive-fixnum 5) most-positive-fixnum 2)
        (%lvl-varby-to 0 most-positive-fixnum most-positive-fixnum)
        (%lvl-varby-to most-positive-fixnum most-positive-fixnum most-positive-fixnum)
        (%lvl-varby-down 0 10 3)
        (%lvl-varby-down most-negative-fixnum (+ most-negative-fixnum 5) 2)
        (%lvl-varby-down most-negative-fixnum 0 most-positive-fixnum)
        (%lvl-varby-any 0 1 1/3) (%lvl-varby-any 1/2 2 1/2)
        (%lvl-varby-any (expt 2 70) (+ (expt 2 70) 5) 2)
        (%lvl-varby-any 0 1 0.5d0)
        (%lvl-by-error 0) (%lvl-by-error -2) (%lvl-by-error 2))
  ((0 3 6 9) (0) nil (3 13) (3 #.(logxor (- most-positive-fixnum 5) (- most-positive-fixnum 3)
                                          (- most-positive-fixnum 1)))
   (2 #.most-positive-fixnum) (1 #.most-positive-fixnum) (0 3 6 9) (0 2 4)
   (0 #.most-positive-fixnum)
   (0 1/3 2/3 1) (1/2 1 3/2 2) (#.(expt 2 70) #.(+ (expt 2 70) 2) #.(+ (expt 2 70) 4))
   (0 0.5d0 1.0d0) (:type-error 0) (:type-error -2) (0 2 4)))

(deftest loop-variable-limit.values
  (list (%lvl-to 3 10) (%lvl-to 5 4) (%lvl-to -3 3)
        (%lvl-to (- most-positive-fixnum 2) most-positive-fixnum)
        (%lvl-to most-positive-fixnum most-positive-fixnum)
        (%lvl-downto 3 10) (%lvl-downto 10 3)
        (%lvl-downto most-negative-fixnum (+ most-negative-fixnum 2))
        (%lvl-downto most-negative-fixnum most-negative-fixnum)
        (%lvl-by 0 10) (%lvl-by (- most-positive-fixnum 7) most-positive-fixnum)
        (%lvl-down-by 0 7)
        (%lvl-down-by most-negative-fixnum (+ most-negative-fixnum 4))
        (%lvl-sum 1 100)
        (= (%lvl-sum (1- most-positive-fixnum) most-positive-fixnum)
           (+ most-positive-fixnum most-positive-fixnum -1))
        (%lvl-two 2 5)
        (%lvl-two (- most-positive-fixnum 1) most-positive-fixnum)
        (%lvl-after-list 1 3 '(a b c d e))
        (%lvl-after-list (1- most-positive-fixnum) most-positive-fixnum '(a b c d e))
        (%lvl-finish 1 10) (%lvl-finish 1 0)
        (%lvl-typed (1- most-positive-fixnum) most-positive-fixnum))
  ((8 8) (0 0) (7 -4) (3 #.(- most-positive-fixnum 3)) (1 #.most-positive-fixnum) (8 8) (0 0)
   (3 #.(+ most-negative-fixnum 3)) (1 #.most-negative-fixnum) (0 3 6 9) (0 3 6) (0 2 4 6) (0 2 4) 5050 t (0 1 2 3) (0 1)
   ((a . 0) (b . 1) (c . 2)) ((a . 0) (b . 1)) 2 0 2))

;;; Something after the loop reads the variable: the value one step past an
;;; int64 limit is a bignum.

;;; A limit past int64: a step that leaves int64 has not passed it.

(defun %lvl-big-limit-do (k)
  (declare (fixnum k))
  (let ((c 0)) (do ((i k (1+ i))) ((> i 9223372036854775810) c) (incf c))))

(defun %lvl-big-limit-loop (k)
  (declare (fixnum k))
  (let ((c 0)) (loop for i from k to 9223372036854775810 do (incf c)) c))

(deftest loop-variable-limit.past-the-edge
  (list (%lvl-final 1 5) (%lvl-final 5 1)
        (= (%lvl-final most-positive-fixnum most-positive-fixnum) (1+ most-positive-fixnum))
        (= (%lvl-final-macro most-positive-fixnum most-positive-fixnum)
           (1+ most-positive-fixnum))
        (%lvl-final-macro 1 5)
        (= (%lvl-captured (1- most-positive-fixnum) most-positive-fixnum)
           (1+ most-positive-fixnum))
        (%lvl-big-limit-do (1- most-positive-fixnum))
        (%lvl-big-limit-loop (1- most-positive-fixnum)))
  (6 5 t t 6 t 5 5))

(deftest-emitting-only loop-variable-limit.shape
  (flet ((counts (fn)
           (let ((s (%lvl-sil fn)))
             (list (%lvl-count "IsTrue" s) (%lvl-count "Runtime.Add)" s)
                   (%lvl-count "Runtime.Increment" s) (%lvl-count "Runtime.Decrement" s)
                   (plusp (%lvl-count "STEPOK" s))))))
    (mapcar #'counts (list #'%lvl-to #'%lvl-downto #'%lvl-by #'%lvl-down-by #'%lvl-two
                           #'%lvl-after-list #'%lvl-finish #'%lvl-typed
                           #'%lvl-varby-to #'%lvl-varby-down
                           #'%lvl-final #'%lvl-final-macro #'%lvl-captured)))
  ((0 0 0 0 t) (0 0 0 0 t) (0 0 0 0 t) (0 0 0 0 t) (0 0 1 0 t)
   (2 0 0 0 t) (0 0 0 0 t) (0 0 0 0 t) (0 0 0 0 t) (0 0 0 0 t)
   (1 0 1 0 nil) (1 0 1 0 nil) (1 0 1 0 nil)))

;;; The limit and the BY value are int64 too: the end test does not unbox.

(deftest-emitting-only loop-variable-limit.limit-slot
  (mapcar (lambda (fn)
            (let ((s (%lvl-sil fn)))
              (+ (%lvl-count "(UNBOX-FIXNUM) (CGT)" s) (%lvl-count "(UNBOX-FIXNUM) (CLT)" s)
                 (%lvl-count "(UNBOX-FIXNUM) (ADD)" s) (%lvl-count "(UNBOX-FIXNUM) (SUB)" s))))
          (list #'%lvl-to #'%lvl-downto #'%lvl-varby-to #'%lvl-varby-down))
  (0 0 0 0))

(setf dotcl:*save-sil* nil)
