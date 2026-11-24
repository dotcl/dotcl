;;; *PRINT-CIRCLE* numbers what is shared in what gets printed. An instance or structure that
;;; has a PRINT-OBJECT method of the program's own prints what that method prints, not its
;;; slots, so a list held in two of its slots but printed once gets no label. A structure printed
;;; by default still shows its slots. Values from SBCL 2.6.8.
(defclass pcupo-ev ()
  ((a :initarg :a) (b :initarg :b)))
(defmethod print-object ((e pcupo-ev) s) (format s "#<EV ~S>" (slot-value e 'a)))
(defstruct pcupo-st a b)
(defmethod print-object ((e pcupo-st) s) (format s "#<ST ~S>" (pcupo-st-a e)))
(defstruct pcupo-st2 a b)

(deftest print-circle-user-print-object.instance
  (let ((l (list 1)) (*print-circle* t))
    (list (prin1-to-string (make-instance 'pcupo-ev :a l :b l))
          (prin1-to-string (list (make-instance 'pcupo-ev :a l :b l)))
          (prin1-to-string (list l (make-instance 'pcupo-ev :a l :b l)))))
  ("#<EV (1)>" "(#<EV (1)>)" "(#1=(1) #<EV #1#>)"))

(deftest print-circle-user-print-object.structure
  (let ((l (list 1)) (*print-circle* t))
    (list (prin1-to-string (make-pcupo-st :a l :b l))
          (prin1-to-string (list (make-pcupo-st :a l :b l)))
          (prin1-to-string (make-pcupo-st2 :a l :b l))
          (prin1-to-string (list (make-pcupo-st2 :a l :b l)))))
  ("#<ST (1)>" "(#<ST (1)>)" "#S(PCUPO-ST2 :A #1=(1) :B #1#)" "(#S(PCUPO-ST2 :A #1=(1) :B #1#))"))

;; A slot that leads back to the object that owns it: the printed text has to end, and has to say
;; where the cycle closes. Direct, through a list, through a vector, and between two objects.
(defstruct pcupo-cy a b)
(defmethod print-object ((e pcupo-cy) s) (format s "#<CY ~S ~S>" (pcupo-cy-a e) (pcupo-cy-b e)))
(defclass pcupo-ci ()
  ((a :initarg :a :initform nil :accessor pcupo-ci-a)
   (b :initarg :b :initform nil :accessor pcupo-ci-b)))
(defmethod print-object ((e pcupo-ci) s) (format s "#<CI ~S ~S>" (pcupo-ci-a e) (pcupo-ci-b e)))

(deftest print-circle-user-print-object.cycle-structure
  (let ((*print-circle* t))
    (flet ((cyc (maker)
             (let ((s (make-pcupo-cy))) (setf (pcupo-cy-a s) (funcall maker s)) s)))
      (let ((u (make-pcupo-cy)) (v (make-pcupo-cy)))
        (setf (pcupo-cy-a u) v (pcupo-cy-a v) u)
        (list (prin1-to-string (cyc #'identity))
              (prin1-to-string (list (cyc #'identity)))
              (prin1-to-string (cyc (lambda (s) (list s s))))
              (prin1-to-string (cyc (lambda (s) (vector s 1))))
              (prin1-to-string u)))))
  ("#1=#<CY #1# NIL>" "(#1=#<CY #1# NIL>)" "#1=#<CY (#1# #1#) NIL>" "#1=#<CY #(#1# 1) NIL>"
   "#1=#<CY #<CY #1# NIL> NIL>"))

(deftest print-circle-user-print-object.cycle-instance
  (let ((*print-circle* t))
    (flet ((cyc (maker)
             (let ((s (make-instance 'pcupo-ci))) (setf (pcupo-ci-a s) (funcall maker s)) s)))
      (let ((u (make-instance 'pcupo-ci)) (v (make-instance 'pcupo-ci)))
        (setf (pcupo-ci-a u) v (pcupo-ci-a v) u)
        (list (prin1-to-string (cyc #'identity))
              (prin1-to-string (cyc (lambda (s) (list s s))))
              (prin1-to-string (cyc (lambda (s) (vector s 1))))
              (prin1-to-string u)))))
  ("#1=#<CI #1# NIL>" "#1=#<CI (#1# #1#) NIL>" "#1=#<CI #(#1# 1) NIL>" "#1=#<CI #<CI #1# NIL> NIL>"))

;; A ring of three or more: only the object where the ring closes is labelled, not the ones in
;; between. And what *print-length* / *print-level* cut is not counted as shared.
(defun pcupo-ring (n)
  (let ((l (loop repeat n collect (make-pcupo-cy))))
    (loop for (a b) on l do (when b (setf (pcupo-cy-a a) b)))
    (setf (pcupo-cy-a (car (last l))) (car l))
    (car l)))

(deftest print-circle-user-print-object.ring
  (let ((*print-circle* t))
    (let ((r (pcupo-ring 3)))
      (let ((w (pcupo-cy-a (pcupo-cy-a r))))
        (list (prin1-to-string (pcupo-ring 2))
              (prin1-to-string (pcupo-ring 3))
              (prin1-to-string (pcupo-ring 4))
              (prin1-to-string (list (pcupo-ring 3)))
              (prin1-to-string (list r r))
              (progn (setf (pcupo-cy-b r) w) (prin1-to-string r))))))
  ("#1=#<CY #<CY #1# NIL> NIL>"
   "#1=#<CY #<CY #<CY #1# NIL> NIL> NIL>"
   "#1=#<CY #<CY #<CY #<CY #1# NIL> NIL> NIL> NIL>"
   "(#1=#<CY #<CY #<CY #1# NIL> NIL> NIL>)"
   "(#1=#<CY #<CY #<CY #1# NIL> NIL> NIL> #1#)"
   "#1=#<CY #<CY #2=#<CY #1# NIL> NIL> #2#>"))

(deftest print-circle-user-print-object.ring-length-level
  (let ((*print-circle* t))
    (let ((s (make-pcupo-cy)))
      (setf (pcupo-cy-a s) (list (list s)))
      (list (let ((*print-level* 3)) (prin1-to-string s))
            (let ((*print-length* 2)) (prin1-to-string (pcupo-ring 3)))
            (progn (setf (pcupo-cy-a s) (list s s))
                   (let ((*print-length* 1)) (prin1-to-string s))))))
  ("#1=#<CY ((#1#)) NIL>" "#1=#<CY #<CY #<CY #1# NIL> NIL> NIL>" "#1=#<CY (#1# ...) NIL>"))

;; Instances and structures with a PRINT-OBJECT method of their own, nested in each other and
;; sharing a list or a string: the labels have to survive the nesting.
(defclass pcupo-pi ()
  ((a :initarg :a :initform nil) (b :initarg :b :initform nil)))
(defmethod print-object ((x pcupo-pi) s) (format s "#<PI ~S ~S>" (slot-value x 'a) (slot-value x 'b)))
(defstruct pcupo-ps a b)
(defmethod print-object ((x pcupo-ps) s) (format s "#<PS ~S ~S>" (pcupo-ps-a x) (pcupo-ps-b x)))

(deftest print-circle-user-print-object.nested
  (let ((*print-circle* t) (sh (list 1 2)) (str (copy-seq "ab")))
    (flet ((i (&rest r) (apply #'make-instance 'pcupo-pi r))
           (st (a b) (make-pcupo-ps :a a :b b)))
      (list (prin1-to-string (i :a (i :a sh :b sh) :b sh))
            (prin1-to-string (i :a (i :a str :b str) :b str))
            (prin1-to-string (i :a (i :a (i :a sh :b sh) :b sh) :b sh))
            (prin1-to-string (i :a (st sh sh) :b sh))
            (prin1-to-string (st (i :a sh :b sh) sh))
            (prin1-to-string (st (st sh sh) sh))
            (prin1-to-string (list (i :a sh) (i :b sh)))
            (prin1-to-string (i :a (list (i :a sh :b sh)) :b sh))
            (prin1-to-string (st (i :a (st sh sh) :b sh) (i :a sh)))
            (prin1-to-string (i :a (i :a (list 1) :b (list 2)) :b (list 3)))
            (let ((in (i :a sh))) (prin1-to-string (i :a in :b in))))))
  ("#<PI #<PI #1=(1 2) #1#> #1#>"
   "#<PI #<PI #1=\"ab\" #1#> #1#>"
   "#<PI #<PI #<PI #1=(1 2) #1#> #1#> #1#>"
   "#<PI #<PS #1=(1 2) #1#> #1#>"
   "#<PS #<PI #1=(1 2) #1#> #1#>"
   "#<PS #<PS #1=(1 2) #1#> #1#>"
   "(#<PI #1=(1 2) NIL> #<PI NIL #1#>)"
   "#<PI (#<PI #1=(1 2) #1#>) #1#>"
   "#<PS #<PI #<PS #1=(1 2) #1#> #1#> #<PI #1# NIL>>"
   "#<PI #<PI (1) (2)> (3)>"
   "#<PI #1=#<PI (1 2) NIL> #1#>"))
