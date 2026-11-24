;;; A global macro that takes &ENVIRONMENT in a function body does not make every variable in scope
;;; assigned and captured: the analysis expands it with the macro environment code generation uses,
;;; once. Typed locals stay raw (no allocation), a macro expanded once, and the answers do not change
;;; when the expansion assigns a variable, closes over one, or asks about declarations.

(defvar *emt-count* 0)

(defmacro emt-unroll ((var limit) &body body &environment env)
  (let ((n (eval (macroexpand limit env))))
    `(progn ,@(loop for i below n collect `(symbol-macrolet ((,var ,i)) ,@body)))))

(defmacro emt-counted (x &environment env)
  (declare (ignore env))
  (incf *emt-count*)
  x)

(defmacro emt-set (v e &environment env)
  (declare (ignore env))
  `(setq ,v ,e))

(defmacro emt-closure (v &environment env)
  (declare (ignore env))
  `(funcall (lambda () ,v)))

(defun %emt-xor (v n) (declare (type (simple-array (unsigned-byte 64) (4)) v) (fixnum n))
  (let ((a (aref v 0)) (b (aref v 1)))
    (declare (type (unsigned-byte 64) a b))
    (dotimes (i n)
      (emt-unroll (k 2)
        (setq a (logand (logxor a (ash b k)) #xffffffffffffffff))
        (setq b (logand (+ b a) #xffffffffffffffff))))
    (setf (aref v 0) a (aref v 1) b)
    (list a b)))

(defun %emt-expanded-once (x)
  (let ((a x)) (declare (fixnum a))
    (dotimes (i 3) (setq a (+ a (emt-counted 1))))
    a))

(defun %emt-assign (x)
  (let ((a x))
    (emt-set a (+ a 10))
    (emt-set a (* a 2))
    a))

(defun %emt-capture (x)
  (let ((a x))
    (emt-set a (+ a 1))
    (list (emt-closure a) (funcall (lambda () (emt-set a (+ a 5)))) a)))

(deftest env-macro-typed-locals.answers
  (list (%emt-xor (make-array 4 :element-type '(unsigned-byte 64) :initial-contents '(#xf123456789abcdef 3 0 0)) 5)
        (%emt-assign 4)
        (%emt-capture 1))
  (#.(let ((a #xf123456789abcdef) (b 3))
        (dotimes (i 5)
          (dotimes (k 2)
            (setq a (logand (logxor a (ash b k)) #xffffffffffffffff))
            (setq b (logand (+ b a) #xffffffffffffffff))))
        (list a b))
    28 (2 7 7)))

(deftest-compiled-only env-macro-typed-locals.expanded-once
  (progn (setq *emt-count* 0) (compile '%emt-expanded-once2 '(lambda (x) (let ((a x)) (declare (fixnum a)) (setq a (+ a (emt-counted 1))) a)))
         (list *emt-count* (funcall '%emt-expanded-once2 4)))
  (1 5))

(deftest-compiled-only env-macro-typed-locals.no-alloc
  (let ((v (make-array 4 :element-type '(unsigned-byte 64) :initial-contents '(#xf123456789abcdef 3 0 0)))
        (best nil))
    (%emt-xor v 10)
    (dotimes (w 3)
      (let ((b0 (nth 4 (dotcl:gc-stats))))
        (%emt-xor v 1000)
        (let ((d (- (nth 4 (dotcl:gc-stats)) b0)))
          (setf best (if best (min best d) d)))))
    (< best 20000))
  t)
