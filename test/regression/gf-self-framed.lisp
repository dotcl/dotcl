;;; A generic function records its own debugger frame, without a try/finally,
;;; and on a warm cache calls the method body straight from that entry. The
;;; frame list must be right inside the method, after non-local exits through
;;; the generic function, after a stack overflow, and on every arity.

(defun %gsf-depth () (length (dotcl:backtrace)))

(defgeneric %gsf-bt2 (a b))
(defmethod %gsf-bt2 ((a integer) b) (declare (ignore b)) (dotcl:backtrace))
(defmethod %gsf-bt2 ((a string) b) (declare (ignore b)) (dotcl:backtrace))

(defgeneric %gsf-bt1 (a))
(defmethod %gsf-bt1 ((a integer)) (dotcl:backtrace))

(defgeneric %gsf-bt3 (a b c))
(defmethod %gsf-bt3 ((a integer) b c) (declare (ignore b c)) (dotcl:backtrace))

(defgeneric %gsf-bt5 (a b c d e))
(defmethod %gsf-bt5 ((a integer) b c d e) (declare (ignore b c d e)) (dotcl:backtrace))

(defun %gsf-tail2 (x) (%gsf-bt2 x x))

;; %GSF-DEPTH counts its own frame, the method counts the generic function's.
;; Twice per entry: the first call fills the cache, the second takes it.
(deftest gf-self-framed.frame-inside-the-method
  (let ((base (%gsf-depth)))
    (loop repeat 2
          collect (list (first (%gsf-bt1 1)) (- (length (%gsf-bt1 1)) base)
                        (first (%gsf-bt2 1 2)) (- (length (%gsf-bt2 1 2)) base)
                        (- (length (%gsf-bt2 "s" 2)) base)
                        (- (length (funcall #'%gsf-bt2 1 2)) base)
                        (- (length (%gsf-bt3 1 2 3)) base)
                        (first (%gsf-bt5 1 2 3 4 5)) (- (length (%gsf-bt5 1 2 3 4 5)) base))))
  (("%GSF-BT1" 0 "%GSF-BT2" 0 0 0 0 "%GSF-BT5" 0)
   ("%GSF-BT1" 0 "%GSF-BT2" 0 0 0 0 "%GSF-BT5" 0)))

;; A tail call from a function to the generic function: the generic function's
;; frame takes the caller's place.
(deftest-emitting-only gf-self-framed.tail-call-into-a-generic-function
  (let ((base (%gsf-depth)))
    (loop repeat 2
          collect (let ((bt (%gsf-tail2 1)))
                    (list (first bt) (count "%GSF-TAIL2" bt :test #'equal)
                          (- (length bt) base) (- (%gsf-depth) base)))))
  (("%GSF-BT2" 0 0 0) ("%GSF-BT2" 0 0 0)))

(defgeneric %gsf-down (n kind))
(defmethod %gsf-down ((n integer) kind)
  (if (= n 0)
      (ecase kind
        (:error (error "down"))
        (:throw (throw '%gsf-tag :thrown)))
      (1+ (%gsf-down (1- n) kind))))

(defgeneric %gsf-no-bottom (n))
(defmethod %gsf-no-bottom ((n integer)) (1+ (%gsf-no-bottom (1+ n))))

(deftest gf-self-framed.frame-list-after-exits
  (let ((base (%gsf-depth)))
    (list (handler-case (%gsf-down 50 :error) (error () :caught))
          (- (%gsf-depth) base)
          (catch '%gsf-tag (%gsf-down 50 :throw))
          (- (%gsf-depth) base)
          (- (length (%gsf-bt2 1 2)) base)))
  (:caught 0 :thrown 0 0))

(deftest-emitting-only gf-self-framed.stack-overflow-is-a-condition
  (let ((base (%gsf-depth)))
    (list (handler-case (%gsf-no-bottom 0) (storage-condition () :overflow))
          (- (%gsf-depth) base)
          (handler-case (%gsf-down 50 :error) (error () :caught))
          (- (%gsf-depth) base)))
  (:overflow 0 :caught 0))

;; A method that needs its next-method state and one that does not, on the
;; same warm cache.
(defgeneric %gsf-next (x))
(defmethod %gsf-next ((x integer)) (list :integer (call-next-method)))
(defmethod %gsf-next ((x number)) :number)
(defmethod %gsf-next ((x symbol)) :symbol)

(deftest gf-self-framed.next-method-and-plain-on-one-cache
  (loop repeat 3 collect (list (%gsf-next 1) (%gsf-next 1.5) (%gsf-next 'a)))
  (((:integer :number) :number :symbol)
   ((:integer :number) :number :symbol)
   ((:integer :number) :number :symbol)))

;; The warm path calls a method body through its function pointer only for the
;; argument count the body was made for, and not for a body that closes over
;; variables.
(defgeneric %gsf-opt (x &optional y))
(defmethod %gsf-opt ((x integer) &optional (y 100)) (+ x y))
(let ((k 10))
  (defgeneric %gsf-closure (x))
  (defmethod %gsf-closure ((x integer)) (+ x k)))

(deftest gf-self-framed.argument-counts-and-closures
  (loop repeat 3 collect (list (%gsf-opt 1) (%gsf-opt 1 2) (%gsf-closure 1)))
  ((101 3 11) (101 3 11) (101 3 11)))
