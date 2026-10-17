;;; Operator method combinations: a method's value used by the operator is its
;;; primary value, the combination returns one value unless the operator form
;;; returns the last method's values (AND, OR, PROGN) or a single method is the
;;; whole effective method (:identity-with-one-argument).

(defmacro %gsc-def (name comb &rest methods)
  `(progn (defgeneric ,name (x) (:method-combination ,comb))
          ,@(loop for (spec body) in methods
                  collect `(defmethod ,name ,comb ((x ,spec)) ,body))))

(%gsc-def %gsc-and1 and (integer (values t 5)))
(%gsc-def %gsc-and2 and (integer (values nil 5)) (t t))
(%gsc-def %gsc-and3 and (integer (values t 5)) (t t))
(%gsc-def %gsc-and4 and (integer t) (t (values 3 4)))
(%gsc-def %gsc-or1 or (integer (values nil 5)))
(%gsc-def %gsc-or2 or (integer (values nil 5)) (t nil))
(%gsc-def %gsc-or3 or (integer (values 7 5)) (t nil))
(%gsc-def %gsc-or4 or (integer nil) (t (values 3 4)))
(%gsc-def %gsc-progn1 progn (integer (values 1 5)))
(%gsc-def %gsc-progn2 progn (integer (values nil 5)) (t nil))
(%gsc-def %gsc-progn3 progn (integer nil) (t (values 3 4)))
(%gsc-def %gsc-list1 list (integer (values 1 5)))
(%gsc-def %gsc-list2 list (integer (values 1 5)) (t (values nil 2)))
(%gsc-def %gsc-app1 append (integer (values nil 5)))
(%gsc-def %gsc-app2 append (integer (values (list 1) 5)) (t (values nil 2)))
(%gsc-def %gsc-nc1 nconc (integer (values nil 5)))
(%gsc-def %gsc-nc2 nconc (integer (values (list 1) 5)) (t (values nil 2)))
(%gsc-def %gsc-max1 max (integer (values 3 5)))
(%gsc-def %gsc-max2 max (integer (values 3 5)) (t 3))
(%gsc-def %gsc-max3 max (integer (values 3 5)) (t (values 1 5)))
(%gsc-def %gsc-min2 min (integer (values 3 5)) (t (values 3 9)))
(%gsc-def %gsc-plus1 + (integer (values 3 5)))
(%gsc-def %gsc-plus2 + (integer (values 3 5)) (t (values 0 9)))
(define-method-combination %gsc-my-and :operator and :identity-with-one-argument t)
(%gsc-def %gsc-my1 %gsc-my-and (integer (values t 5)))
(%gsc-def %gsc-my2 %gsc-my-and (integer (values nil 5)) (t t))
(define-method-combination %gsc-my-list :operator list)
(%gsc-def %gsc-ml1 %gsc-my-list (integer (values 1 5)) (t (values 2 6)))
(define-method-combination %gsc-my-values :operator values)
(%gsc-def %gsc-mv %gsc-my-values (integer (values 1 5)) (t (values 2 6)))
(%gsc-def %gsc-around progn (integer nil))
(defmethod %gsc-around :around ((x integer)) (call-next-method))
(defmethod %gsc-around progn ((x t)) (values nil 2))
(%gsc-def %gsc-around2 and (integer (values nil 5)))
(defmethod %gsc-around2 :around ((x integer)) (multiple-value-list (call-next-method)))
(defmethod %gsc-around2 and ((x t)) t)

(deftest gf-short-combination-values
  (mapcar (lambda (f)
            (let (r) (dotimes (i 30) (setf r (multiple-value-list (funcall f 1)))) r))
          '(%gsc-and1 %gsc-and2 %gsc-and3 %gsc-and4 %gsc-or1 %gsc-or2 %gsc-or3 %gsc-or4
            %gsc-progn1 %gsc-progn2 %gsc-progn3 %gsc-list1 %gsc-list2 %gsc-app1 %gsc-app2
            %gsc-nc1 %gsc-nc2 %gsc-max1 %gsc-max2 %gsc-max3 %gsc-min2 %gsc-plus1 %gsc-plus2
            %gsc-my1 %gsc-my2 %gsc-ml1 %gsc-mv %gsc-around %gsc-around2))
  ((t 5) (nil) (t) (3 4) (nil 5) (nil) (7) (3 4)
   (1 5) (nil) (3 4) ((1)) ((1 nil)) (nil) ((1))
   (nil 5) ((1)) (3 5) (3) (3) (3) (3 5) (3)
   (t 5) (nil) ((1 2)) (1 2) (nil 2) ((nil))))
