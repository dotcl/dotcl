;;; A generic function whose method takes &KEY, called with a keyword, hands the caller's value
;;; mode to the method: a (VALUES ...) the caller takes with MULTIPLE-VALUE-BIND needs no values
;;; record. The values are the same whichever way the call is made.

(defgeneric %gkm-scan (rx s &key start))
(defmethod %gkm-scan ((rx function) s &key (start 0)) (funcall rx s start))
(defgeneric %gkm-opt (x &optional y &key k))
(defmethod %gkm-opt ((x integer) &optional (y 10) &key (k 100)) (values x y k (+ x y k)))
(defgeneric %gkm-around (x &key k))
(defmethod %gkm-around ((x integer) &key (k 1)) (values x k))
(defmethod %gkm-around :around ((x integer) &key (k 1)) (multiple-value-bind (a b) (call-next-method) (values b a :around)))
(defgeneric %gkm-cnm (x &key k))
(defmethod %gkm-cnm ((x number) &key (k 1)) (values :number x k))
(defmethod %gkm-cnm ((x integer) &key (k 1)) (multiple-value-bind (a b c) (call-next-method) (values :integer a b c)))
(defgeneric %gkm-allow (x &key a &allow-other-keys))
(defmethod %gkm-allow ((x integer) &key a &allow-other-keys) (values x a))

(defun %gkm-mk () (lambda (s st) (values st (1+ st) (list s) nil)))

(deftest gf-key-mode.mvb-four-values
  (let ((f (%gkm-mk)))
    (multiple-value-bind (a b c d) (%gkm-scan f "x" :start 5) (list a b c d)))
  (5 6 ("x") nil))

(deftest gf-key-mode.mvb-no-keyword
  (let ((f (%gkm-mk)))
    (multiple-value-bind (a b c d) (%gkm-scan f "x") (list a b c d)))
  (0 1 ("x") nil))

(deftest gf-key-mode.multiple-value-list
  (multiple-value-list (%gkm-scan (%gkm-mk) "y" :start 2))
  (2 3 ("y") nil))

(deftest gf-key-mode.primary-only
  (list (%gkm-scan (%gkm-mk) "y" :start 7) (let ((x (%gkm-scan (%gkm-mk) "z" :start 8))) x))
  (7 8))

(deftest gf-key-mode.funcall-and-apply
  (let ((f (%gkm-mk)))
    (list (multiple-value-list (funcall #'%gkm-scan f "q" :start 3))
          (multiple-value-list (apply #'%gkm-scan f "q" '(:start 4)))))
  ((3 4 ("q") nil) (4 5 ("q") nil)))

(deftest gf-key-mode.optional-and-key
  (list (multiple-value-list (%gkm-opt 1 2 :k 3))
        (multiple-value-list (%gkm-opt 1 2))
        (multiple-value-list (%gkm-opt 1)))
  ((1 2 3 6) (1 2 100 103) (1 10 100 111)))

(deftest gf-key-mode.around-method-unchanged
  (multiple-value-list (%gkm-around 5 :k 9))
  (9 5 :around))

(deftest gf-key-mode.call-next-method-chain
  (multiple-value-list (%gkm-cnm 4 :k 2))
  (:integer :number 4 2))

(deftest gf-key-mode.allow-other-keys
  (multiple-value-list (%gkm-allow 3 :a 1 :zzz 2))
  (3 1))

(deftest gf-key-mode.unknown-keyword-still-an-error
  (handler-case (%gkm-scan (%gkm-mk) "x" :bogus 1)
    (error () :error))
  :error)

(deftest gf-key-mode.mvb-in-a-loop-keeps-each-call-apart
  (let ((f (%gkm-mk)) (l nil))
    (dotimes (i 4) (multiple-value-bind (a b) (%gkm-scan f "x" :start i) (push (+ a b) l)))
    (nreverse l))
  (1 3 5 7))

(defun %gkm-loop (f n)
  (let ((s 0))
    (dotimes (i n) (multiple-value-bind (a b c d) (%gkm-scan f "x" :start 1) (declare (ignore c d)) (setq s (+ s a b))))
    s))

(deftest-emitting-only gf-key-mode.no-values-record-per-call
  (let ((f (%gkm-mk)))
    (%gkm-loop f 1000)
    ;; An array of the call's four arguments is left (56 bytes); the values record is not (it
    ;; made it more than twice that).
    (< (min-allocated-bytes (lambda () (%gkm-loop f 20000))) (* 20000 100)))
  t)
