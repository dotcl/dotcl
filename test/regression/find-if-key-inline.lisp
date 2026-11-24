;;; (FIND-IF (LAMBDA (X) ...) SEQ :KEY K) and the POSITION-IF / COUNT-IF / -IF-NOT forms with one :KEY are
;;; opened into a loop like the forms without :KEY: no closure per call, and the same answers, evaluation
;;; order and scope as the function call.

(defstruct fik-g ex)

(defun fik-find (gs k) (find-if (lambda (x) (> x k)) gs :key #'fik-g-ex))

(defun fik-bytes (thunk)
  (funcall thunk)
  (let ((b0 (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread")))
    (dotimes (i 1000) (funcall thunk))
    (- (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread") b0)))

(deftest find-if-key-inline.answers
  (let ((gs (list (make-fik-g :ex 3) (make-fik-g :ex 5) (make-fik-g :ex 8))) (k 5))
    (list (fik-g-ex (find-if (lambda (x) (> x k)) gs :key #'fik-g-ex))
          (fik-g-ex (find-if (lambda (x) (> x 4)) (coerce gs 'vector) :key #'fik-g-ex))
          (find-if (lambda (x) (> x 99)) gs :key #'fik-g-ex)
          (position-if (lambda (x) (> x 4)) gs :key #'fik-g-ex)
          (count-if (lambda (x) (oddp x)) gs :key #'fik-g-ex)
          (fik-g-ex (find-if-not (lambda (x) (< x 4)) gs :key #'fik-g-ex))
          (find-if (lambda (x) (> x 3)) '(1 2 3 4 5) :key nil)
          (let ((f #'fik-g-ex)) (fik-g-ex (find-if (lambda (x) (> x 6)) gs :key f)))
          (fik-g-ex (find-if (lambda (x) (> x 6)) gs :key 'fik-g-ex))
          (find-if (lambda (x) (> x 6)) '(1 2 9) :key (lambda (v) (* v 2)))
          (flet ((fik-g-ex (g) (* 100 (fik-g-ex g)))) (fik-g-ex (find-if (lambda (x) (> x 600)) gs :key #'fik-g-ex)))
          (find-if (lambda (x) (> x 1)) '(1 2 3) :from-end t :key #'identity)))
  (8 5 nil 1 2 5 4 8 8 9 800 3))

(deftest find-if-key-inline.order-and-return
  (list (let ((log nil))
          (find-if (lambda (x) (push :pred log) (> x 3)) (progn (push :seq log) '(1 5)) :key (progn (push :key log) #'identity))
          (reverse log))
        (block b (find-if (lambda (x) (when (> x 2) (return-from b :ret)) nil) '(1 2 3) :key #'identity)))
  ((:seq :key :pred :pred) :ret))

(deftest-emitting-only find-if-key-inline.no-closure-per-call
  (let ((gs (list (make-fik-g :ex 3) (make-fik-g :ex 5) (make-fik-g :ex 8))))
    (< (fik-bytes (lambda () (fik-find gs 4))) 20000))
  t)
