;;; The generic function a DEFMETHOD makes when there is none has the method's own parameter names
;;; in its lambda list (GENERIC-FUNCTION-LAMBDA-LIST, DESCRIBE, editor arglists), not gensyms.
;;; An explicit DEFGENERIC keeps its own lambda list, also when a method names the parameters
;;; differently.

(defmethod igl-a ((x integer) &optional (y 5) (z nil zp)) (list x y z zp))
(defmethod igl-b ((x integer) (y string)) (list x y))
(defmethod igl-c ((x integer) &rest r &key a b &allow-other-keys) (list x r a b))
(defmethod igl-d () :none)
(defmethod igl-e ((x integer) &aux (w (1+ x))) w)

(deftest implicit-generic-function-lambda-list.names
  (list (generic-function-lambda-list #'igl-a)
        (generic-function-lambda-list #'igl-b)
        (generic-function-lambda-list #'igl-c)
        (generic-function-lambda-list #'igl-d)
        (generic-function-lambda-list #'igl-e))
  ((x &optional y z) (x y) (x &rest r &key &allow-other-keys) nil (x)))

(deftest implicit-generic-function-lambda-list.still-callable
  (list (igl-a 1) (igl-a 1 2) (igl-b 1 "s") (igl-c 1 :a 2) (igl-d) (igl-e 4))
  ((1 5 nil nil) (1 2 nil nil) (1 "s") (1 (:a 2) 2 nil) :none 5))

(defgeneric igl-f (a b))
(defmethod igl-f ((x integer) (y integer)) (+ x y))
(defgeneric igl-g (&optional p))
(defmethod igl-g (&optional (q 3)) q)
(deftest implicit-generic-function-lambda-list.explicit-defgeneric-kept
  (list (generic-function-lambda-list #'igl-f) (igl-f 1 2)
        (generic-function-lambda-list #'igl-g) (igl-g))
  ((a b) 3 (&optional p) 3))

;; A later DEFGENERIC replaces the lambda list the implicit one took from the method.
(defgeneric igl-a (x &optional y z))
(deftest implicit-generic-function-lambda-list.defgeneric-after
  (generic-function-lambda-list #'igl-a)
  (x &optional y z))
