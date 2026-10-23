;;; A LET or PROGN in a single-value position whose code already ends in the
;;; unwrap of its last form is not unwrapped again. The value it leaves must
;;; still be the primary value only, whatever shape the body ends in.

(defun %ceut-two () (values 7 8))

(defun %ceut-nested (x)
  (list (let ((a x))
          (let ((b (1+ a)))
            (progn b (%ceut-two))))))

(defun %ceut-if-tail (x)
  (list (let ((a x))
          (if a (%ceut-two) (values 1 2)))))

(defun %ceut-block (x)
  (list (block b
          (when x (return-from b (values 3 4)))
          (let ((y 5)) y (%ceut-two)))))

(defun %ceut-deep (n)
  (let ((form '(%ceut-two)))
    (dotimes (i n)
      (setq form `(let ((,(gensym) ,i)) ,form)))
    (funcall (compile nil `(lambda () (list ,form))))))

(deftest compile-expr-unwrapped-tail.nested
  (%ceut-nested 1)
  (7))

(deftest compile-expr-unwrapped-tail.if-tail
  (list (%ceut-if-tail t) (%ceut-if-tail nil))
  ((7) (1)))

(deftest compile-expr-unwrapped-tail.block
  (list (%ceut-block t) (%ceut-block nil))
  ((3) (7)))

(deftest-compiled-only compile-expr-unwrapped-tail.deep
  (%ceut-deep 300)
  (7))

(deftest compile-expr-unwrapped-tail.mv-after
  (multiple-value-list
   (let ((r (car (%ceut-nested 1))))
     (values r (%ceut-if-tail nil))))
  (7 (1)))

;;; Compiling a chain of nested LETs analyses each level without walking the
;;; levels below it again: doubling the depth does not quadruple the work.
(defun %ceut-id (x) x)

(defun %ceut-chain (n read-p)
  (let* ((vars (loop repeat n collect (gensym)))
         (form (if read-p `(%ceut-id ,(car (last vars))) '(%ceut-two))))
    (loop for (v prev) on (reverse vars)
          do (setq form `(let ((,v ,(if (and read-p prev) `(%ceut-id ,prev) 0))) ,form)))
    `(lambda () (list ,form))))

(defun %ceut-normalize-calls (n read-p)
  (let* ((name (find-symbol "%LET-SUBST-NORMALIZE" "DOTCL.CIL-COMPILER"))
         (orig (symbol-function name))
         (calls 0))
    (setf (symbol-function name) (lambda (form) (incf calls) (funcall orig form)))
    (unwind-protect (compile nil (%ceut-chain n read-p))
      (setf (symbol-function name) orig))
    calls))

(deftest-emitting-only compile-expr-unwrapped-tail.deep-linear
  (loop for read-p in '(nil t)
        collect (let ((a (%ceut-normalize-calls 100 read-p))
                      (b (%ceut-normalize-calls 200 read-p)))
                  (<= b (+ 10 (* 3 a)))))
  (t t))
