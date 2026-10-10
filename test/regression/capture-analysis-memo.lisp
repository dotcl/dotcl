;;; Boxing analysis (which variables a body assigns and which it references
;;; from inside a closure) keeps a summary per body form and reuses it for the
;;; LETs nested inside. These shapes reach the reuse: an inner LET compiled
;;; after the outer one walked it, the same form placed both outside and inside
;;; a closure, and bodies walked under a symbol macro or MACROLET.

(deftest capture-memo-nested-let
  (let ((a 0))
    (let ((b 0))
      (let ((f (lambda () (incf a) (incf b))))
        (let ((c 0))
          (let ((g (lambda () (setq c (+ c 10)))))
            (funcall f)
            (funcall f)
            (funcall g)
            (list a b c))))))
  (2 2 10))

;;; The same (EQ) form outside and inside a closure: the second placement must
;;; still count its assignment to N as a capture.
(defmacro capture-memo-twice (form)
  `(progn ,form (funcall (lambda () ,form))))

(deftest capture-memo-shared-form
  (let ((n 0))
    (capture-memo-twice (let ((m 1)) (setq n (+ n m))))
    n)
  2)

(deftest capture-memo-shared-form-read
  (let ((n 1))
    (let ((get nil))
      (capture-memo-twice (let ((m n)) (setq get (lambda () (+ m n)))))
      (setq n 10)
      (funcall get)))
  11)

(deftest capture-memo-symbol-macro-shadow
  (symbol-macrolet ((x (car cell)))
    (let ((cell (list 5)))
      (let ((x 1))
        (let ((f (lambda () (setq x (+ x 1)))))
          (funcall f)
          (funcall f)
          (list x (car cell))))))
  (3 5))

(deftest capture-memo-macrolet-body
  (let ((k 0))
    (macrolet ((bump () '(setq k (1+ k))))
      (let ((f (lambda () (bump))))
        (funcall f)
        (funcall f)
        (funcall f)
        k)))
  3)

(deftest capture-memo-flet-body
  (let ((acc '()))
    (flet ((add (x) (push x acc)))
      (let ((i 0))
        (let ((step (lambda () (incf i) (add i))))
          (funcall step)
          (funcall step)
          (list i acc)))))
  (2 (2 1)))

;; The same (EQ) body forms spliced into two MACROLETs whose local macro
;; assigns a different variable in each: the summary taken under the first
;; MACROLET must not be reused under the second. proc-parse's
;; WITH-VECTOR-PARSING has this shape (one branch per vector type), and
;; cl-cookie's date parser lost its position updates.
(defmacro capture-memo-with-two (&body body)
  (let ((a (gensym "A")) (b (gensym "B")))
    `(list (let ((,a 0))
             (macrolet ((bump () '(incf ,a)))
               (let ((f (lambda () ,a))) ,@body (funcall f))))
           (let ((,b 0))
             (macrolet ((bump () '(incf ,b)))
               (let ((f (lambda () ,b))) ,@body (funcall f)))))))

(defun capture-memo-two-macrolets () (capture-memo-with-two (bump) (bump)))

(deftest capture-memo-spliced-body-two-macrolets
  (capture-memo-two-macrolets)
  (2 2))


;; An assignment to a local symbol macro assigns the variable it stands for,
;; so a closure over that variable needs it boxed.
(defun capture-sm-incf ()
  (let ((a 0))
    (symbol-macrolet ((pos a))
      (let ((f (lambda () a))) (incf pos) (funcall f)))))
(defun capture-sm-setq ()
  (let ((a 0))
    (symbol-macrolet ((pos a))
      (let ((f (lambda () a))) (setq pos 5) (funcall f)))))
(defun capture-sm-closure-reads-macro ()
  (let ((a 0))
    (symbol-macrolet ((pos a))
      (let ((f (lambda () pos))) (incf pos) (funcall f)))))
(defun capture-sm-chain-the ()
  (let ((a 0))
    (symbol-macrolet ((inner (the fixnum a)))
      (symbol-macrolet ((pos inner))
        (let ((f (lambda () a))) (setf pos 7) (funcall f))))))

(deftest capture-symbol-macro-assignment
  (list (capture-sm-incf) (capture-sm-setq) (capture-sm-closure-reads-macro)
        (capture-sm-chain-the))
  (1 5 1 7))

;; The same with SYMBOL-MACROLET: one body, two expansions of one name.
(defmacro capture-memo-with-two-sm (&body body)
  (let ((a (gensym "A")) (b (gensym "B")))
    `(list (let ((,a 0))
             (symbol-macrolet ((pos ,a))
               (let ((f (lambda () ,a))) ,@body (funcall f))))
           (let ((,b 0))
             (symbol-macrolet ((pos ,b))
               (let ((f (lambda () ,b))) ,@body (funcall f)))))))

(defun capture-memo-two-symbol-macrolets () (capture-memo-with-two-sm (incf pos) (incf pos)))

(deftest capture-memo-spliced-body-two-symbol-macrolets
  (capture-memo-two-symbol-macrolets)
  (2 2))
