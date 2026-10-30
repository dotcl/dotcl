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

;;; Nor does compiling each level look through the bindings of all the levels
;;; around it, or through the whole body below it, or copy the levels around
;;; it: the instructions walked and the bytes allocated grow with the depth.
(defun %ceut-deep-form (n)
  (let ((form '(%ceut-two)))
    (dotimes (i n) (setq form `(let ((,(gensym) ,i)) ,form)))
    `(lambda () (list ,form))))

(defun %ceut-if-chain (n)
  (let ((form '(%ceut-id 1)))
    (dotimes (i n) (setq form `(if (%ceut-id ,i) ,form (%ceut-id 2))))
    `(lambda () (list ,form))))

(defun %ceut-progn-chain (n)
  (let ((form '(%ceut-id 1)))
    (dotimes (i n) (setq form `(progn (%ceut-id ,i) ,form)))
    `(lambda () (list ,form))))

(defun %ceut-work (form)
  "(CALLS STEPS BYTES) of compiling FORM: calls of compiler functions that walk
   a body, conses the instruction passes walk or copy, and bytes allocated."
  (let* ((names '("SAME-VAR-PACKAGE-P" "%INLINE-INT-PARAM-USE-P" "STRIP-VALUE-WRAPPERS"
                  "FIND-MACRO-EXPANDER" "%SYMBOL-MACRO-NAME-P"))
         (syms (mapcar (lambda (n) (find-symbol n "DOTCL.CIL-COMPILER")) names))
         (origs (mapcar #'symbol-function syms))
         (steps (find-symbol "*INSTR-WALK-STEPS*" "DOTCL-INTERNAL"))
         (cell (list 0))
         (calls 0)
         (bytes 0))
    (loop for s in syms for f in origs
          do (let ((f f))
               (setf (symbol-function s)
                     (lambda (&rest args) (incf calls) (apply f args)))))
    (setf (symbol-value steps) cell)
    (unwind-protect
         (let ((a0 (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread")))
           (compile nil form)
           (setf bytes (- (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread") a0)))
      (setf (symbol-value steps) nil)
      (loop for s in syms for f in origs do (setf (symbol-function s) f)))
    (list calls (car cell) bytes)))

;;; Doubling the depth at most doubles the calls and the instructions walked
;;; (plus a little), and the bytes stay within 2.5 times (they were 2.7 to 3
;;; times when each level copied the bindings or the code around it).
(deftest-emitting-only compile-expr-unwrapped-tail.deep-linear-walks
  (progn
    (compile nil (%ceut-deep-form 20))
    (loop for make in (list #'%ceut-deep-form
                            (lambda (n) (%ceut-chain n t))
                            #'%ceut-if-chain
                            #'%ceut-progn-chain)
          collect (destructuring-bind (c1 s1 b1) (%ceut-work (funcall make 100))
                    (destructuring-bind (c2 s2 b2) (%ceut-work (funcall make 200))
                      (list (<= c2 (+ 10 (* 2.2 c1)))
                            (<= s2 (+ 100 (* 2.2 s1)))
                            (<= b2 (* 2.5 b1)))))))
  ((t t t) (t t t) (t t t) (t t t)))
