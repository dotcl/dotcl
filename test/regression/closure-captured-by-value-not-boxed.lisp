;;; A variable a closure captures by value (the enclosing scope never assigns it) stays a value in
;;; the closures nested inside, even when the body of the first closure has a macro with
;;; &ENVIRONMENT that the analysis cannot expand (it then assumes any variable may be assigned).
;;; The nested closure used to be told to expect a cell the slot did not hold: an InvalidCast.

(defmacro %cbv-opaque (&environment env) (declare (ignore env)) nil)

(defun %cbv-id (x) x)

;; Expanded at compile time only, so the analysis of the function using it never sees these closures.
(define-compiler-macro %cbv-id (x)
  `(let ((c1 (lambda () (%cbv-opaque) (let ((c2 (lambda () ,x))) (funcall c2)))))
     (funcall c1)))

(defun %cbv-one () (let ((str (list 1 2))) (%cbv-id (car str))))

(defun %cbv-two ()
  (let ((a (list 1 2)) (b (list 3 4)))
    (list (%cbv-id (car a)) (%cbv-id (cadr b)))))

(defun %cbv-string-stream ()
  (let ((out (make-string-output-stream)))
    (%cbv-id (write-string "ok" out))
    (get-output-stream-string out)))

(deftest closure-captured-by-value-not-boxed.one
  (%cbv-one)
  1)

(deftest closure-captured-by-value-not-boxed.two
  (%cbv-two)
  (1 4))

(deftest closure-captured-by-value-not-boxed.stream
  (%cbv-string-stream)
  "ok")
