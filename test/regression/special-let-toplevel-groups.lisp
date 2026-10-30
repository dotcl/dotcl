;;; A top-level LET binds its specials with the unthreaded PushGroup entries.

(defvar *stg-a* :a)
(defvar *stg-b* :b)
(defvar *stg-c* :c)
(defvar *stg-d* :d)
(defvar *stg-e* :e)
(defvar *stg-f* :f)
(defvar *stg-g* :g)
(defvar *stg-results* nil)

(setf *stg-results*
      (list
       (let ((*stg-a* 1) (*stg-b* 2) (*stg-c* 3))
         (list *stg-a* *stg-b* *stg-c*))
       (let ((*stg-a* 10) (*stg-b* 11) (*stg-c* 12) (*stg-d* 13))
         (list *stg-a* *stg-b* *stg-c* *stg-d*))
       (let ((*stg-a* 20) (*stg-b* 21) (*stg-c* 22) (*stg-d* 23)
             (*stg-e* 24) (*stg-f* 25))
         (list *stg-a* *stg-b* *stg-c* *stg-d* *stg-e* *stg-f*))
       (let ((*stg-a* 30) (*stg-b* 31) (*stg-c* 32) (*stg-d* 33)
             (*stg-e* 34) (*stg-f* 35) (*stg-g* 36))
         (list *stg-a* *stg-b* *stg-c* *stg-d* *stg-e* *stg-f* *stg-g*))))

(defun %stg-shape (form)
  (%flat-text (dotcl.cil-compiler:compile-toplevel form)))

(deftest special-let-toplevel-groups.values
  (list *stg-results*
        (list *stg-a* *stg-b* *stg-c* *stg-d* *stg-e* *stg-f* *stg-g*))
  (((1 2 3) (10 11 12 13) (20 21 22 23 24 25) (30 31 32 33 34 35 36))
   (:a :b :c :d :e :f :g)))

(deftest-emitting-only special-let-toplevel-groups.shapes
  (list (and (search "DynamicBindings.PushGroup3)"
                     (%stg-shape '(let ((*stg-a* 1) (*stg-b* 2) (*stg-c* 3))
                                    (list *stg-a* *stg-b* *stg-c*))))
             t)
        (and (search "DynamicBindings.PushGroup4)"
                     (%stg-shape '(let ((*stg-a* 1) (*stg-b* 2) (*stg-c* 3)
                                        (*stg-d* 4))
                                    (list *stg-a* *stg-b* *stg-c* *stg-d*))))
             t)
        (and (search "DynamicBindings.PushGroup6)"
                     (%stg-shape '(let ((*stg-a* 1) (*stg-b* 2) (*stg-c* 3)
                                        (*stg-d* 4) (*stg-e* 5) (*stg-f* 6))
                                    (list *stg-a* *stg-b* *stg-c* *stg-d*
                                          *stg-e* *stg-f*))))
             t)
        (and (search "DynamicBindings.PushGroup7)"
                     (%stg-shape '(let ((*stg-a* 1) (*stg-b* 2) (*stg-c* 3)
                                        (*stg-d* 4) (*stg-e* 5) (*stg-f* 6)
                                        (*stg-g* 7))
                                    (list *stg-a* *stg-b* *stg-c* *stg-d*
                                          *stg-e* *stg-f* *stg-g*))))
             t))
  (t t t t))
