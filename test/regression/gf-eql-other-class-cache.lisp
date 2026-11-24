;;; A generic function with an EQL method on a LIST argument is still cached by class for calls whose
;;; arguments no EQL method can match, and a method whose body holds a MACROLET without
;;; CALL-NEXT-METHOD does not keep its next-method state. The values a method returns reach the
;;; caller with no MvReturn built, and EQL dispatch and CALL-NEXT-METHOD inside MACROLET still work.

(defgeneric %geoc-lookup (coll key))
(defmethod %geoc-lookup ((ls list) (key (eql 'first))) (values (car ls) t))
(defmethod %geoc-lookup ((ls list) (key (eql 'second))) (values (cadr ls) t))
(defmethod %geoc-lookup ((v vector) key)
  (macrolet ((pick (a b &rest args) (declare (ignore b)) (list* a args)))
    (let ((val (if (typep key 'fixnum) (pick aref unused v key) nil)))
      (values val (and val t)))))
(defmethod %geoc-lookup ((ls list) key) (values :other nil))

(defgeneric %geoc-next (x))
(defmethod %geoc-next ((x integer)) (list :integer))
(defmethod %geoc-next ((x fixnum))
  (macrolet ((up () '(call-next-method)))
    (cons :fixnum (up))))

(defvar *geoc-vec* (vector 10 20 30))

(defun %geoc-sum (n)
  (declare (fixnum n))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n) (incf s (the fixnum (%geoc-lookup *geoc-vec* (mod i 3)))))
    s))

(deftest gf-eql-other-class-cache.values
  (list (multiple-value-list (%geoc-lookup '(1 2 3) 'first))
        (multiple-value-list (%geoc-lookup '(1 2 3) 'second))
        (multiple-value-list (%geoc-lookup '(1 2 3) 'third))
        (multiple-value-list (%geoc-lookup *geoc-vec* 1))
        (multiple-value-list (%geoc-lookup *geoc-vec* 'x))
        (%geoc-next 1)
        (%geoc-next (expt 2 70)))
  ((1 t) (2 t) (:other nil) (20 t) (nil nil) (:fixnum :integer) (:integer)))

(deftest-compiled-only gf-eql-other-class-cache.no-allocation
  (progn (%geoc-sum 100)
         (list (%geoc-sum 3)
               (< (min-allocated-bytes (lambda () (%geoc-sum 100000))) 100000)))
  (60 t))
