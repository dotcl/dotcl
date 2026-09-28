;;; What makes an array simple (CLHS 1.4.4): not displaced, not actually
;;; adjustable, and no fill pointer. SIMPLE-STRING-P and its siblings used to
;;; look at the fill pointer alone, so an adjustable or displaced character
;;; vector answered T.
;;;
;;; The predicates, TYPEP, TYPE-OF and COERCE each had their own copy of the
;;; test. They must keep agreeing: ANSI's TYPE-OF.3 checks
;;; (typep x (type-of x)) over a universe that contains adjustable and
;;; displaced arrays, and SIMPLE-STRING-P.1 checks the predicate against TYPEP.
;;; So every test here asks all of them about the same objects.

(defun %sap-strings ()
  (let ((base (make-array 6 :element-type 'character :initial-element #\z)))
    (list (make-string 4 :initial-element #\a)
          (make-array 4 :element-type 'character :initial-element #\b)
          (make-array 4 :element-type 'character :adjustable t :initial-element #\c)
          (make-array 4 :element-type 'character :displaced-to base)
          (make-array 4 :element-type 'character :fill-pointer 2 :initial-element #\d))))

(defun %sap-vectors (element-type initial)
  (let ((base (make-array 6 :element-type element-type :initial-element initial)))
    (list (make-array 4 :element-type element-type :initial-element initial)
          (make-array 4 :element-type element-type :adjustable t :initial-element initial)
          (make-array 4 :element-type element-type :displaced-to base)
          (make-array 4 :element-type element-type :fill-pointer 2 :initial-element initial))))

(defun %sap-bool (x) (if x t nil))

(deftest simple-array-predicates.simple-string-p
  (mapcar #'%sap-bool (mapcar #'simple-string-p (%sap-strings)))
  (t t nil nil nil))

(deftest simple-array-predicates.typep-simple-string
  (mapcar (lambda (s)
            (list (%sap-bool (typep s 'simple-string))
                  (%sap-bool (typep s 'simple-base-string))
                  (%sap-bool (typep s '(simple-string 4)))
                  (%sap-bool (typep s '(simple-array character (*))))
                  (%sap-bool (typep s 'simple-array))
                  (%sap-bool (typep s 'string))))
          (%sap-strings))
  ((t t t t t t) (t t t t t t)
   (nil nil nil nil nil t) (nil nil nil nil nil t) (nil nil nil nil nil t)))

(deftest simple-array-predicates.simple-vector-p
  (mapcar (lambda (v)
            (list (%sap-bool (simple-vector-p v))
                  (%sap-bool (typep v 'simple-vector))
                  (%sap-bool (typep v '(simple-array t (*))))
                  (%sap-bool (typep v 'vector))))
          (%sap-vectors t 0))
  ((t t t t) (nil nil nil t) (nil nil nil t) (nil nil nil t)))

(deftest simple-array-predicates.simple-bit-vector-p
  (mapcar (lambda (v)
            (list (%sap-bool (simple-bit-vector-p v))
                  (%sap-bool (typep v 'simple-bit-vector))
                  (%sap-bool (typep v 'bit-vector))))
          (%sap-vectors 'bit 1))
  ((t t t) (nil nil t) (nil nil t) (nil nil t)))

(deftest simple-array-predicates.specialized-simple-array
  (mapcar (lambda (v) (%sap-bool (typep v '(simple-array (unsigned-byte 8) (*)))))
          (%sap-vectors '(unsigned-byte 8) 3))
  (t nil nil nil))

;; Rank 0 and rank 2 go through a different TYPE-OF branch from vectors.
(deftest simple-array-predicates.other-ranks
  (let* ((plain2 (make-array '(2 3) :initial-element 0))
         (adj2 (make-array '(2 3) :adjustable t :initial-element 0))
         (disp2 (make-array '(2 2) :displaced-to (make-array 6 :initial-element 0)))
         (plain0 (make-array nil :initial-element 0))
         (adj0 (make-array nil :adjustable t :initial-element 0)))
    (mapcar (lambda (a)
              (list (%sap-bool (typep a 'simple-array))
                    (%sap-bool (typep a '(simple-array t *)))
                    (car (type-of a))))
            (list plain2 adj2 disp2 plain0 adj0)))
  ((t t simple-array) (nil nil array) (nil nil array)
   (t t simple-array) (nil nil array)))

;; TYPE-OF must name a type the object is in, for every shape above.
(deftest simple-array-predicates.typep-of-type-of
  (let ((objs (append (%sap-strings) (%sap-vectors t 0) (%sap-vectors 'bit 1)
                      (%sap-vectors '(unsigned-byte 8) 3)
                      (list (make-array '(2 3) :adjustable t :initial-element 0)
                            (make-array nil :adjustable t :initial-element 0)))))
    (remove-if (lambda (x) (typep x (type-of x))) objs))
  nil)

(deftest simple-array-predicates.type-of-names
  (mapcar #'type-of (append (%sap-strings) (%sap-vectors t 0) (%sap-vectors 'bit 1)))
  (simple-base-string simple-base-string base-string base-string base-string
   simple-vector vector vector vector
   simple-bit-vector bit-vector bit-vector bit-vector))

;; ADJUST-ARRAY of a non-adjustable array onto a displacement returns a new
;; array that is displaced without being adjustable: still not simple.
(deftest simple-array-predicates.adjust-array-to-displaced
  (let* ((base (make-array 6 :element-type 'character :initial-element #\q))
         (v (make-array 3 :element-type 'character :initial-element #\r))
         (d (adjust-array v 3 :displaced-to base)))
    (list (%sap-bool (simple-string-p d)) (%sap-bool (typep d 'simple-string))
          (%sap-bool (typep d (type-of d))) (coerce d 'list)))
  (nil nil t (#\q #\q #\q)))

;; COERCE returns the object itself only when it already is of the type; a
;; non-simple vector coerced to a SIMPLE-* type is copied into a simple one.
(deftest simple-array-predicates.coerce-to-simple
  (destructuring-bind (plain adj disp fp) (%sap-vectors t 5)
    (declare (ignore fp))
    (let ((sadj (coerce adj 'simple-vector))
          (sdisp (coerce disp 'simple-vector)))
      (list (eq (coerce plain 'simple-vector) plain)
            (eq sadj adj) (%sap-bool (simple-vector-p sadj)) (coerce sadj 'list)
            (eq sdisp disp) (%sap-bool (simple-vector-p sdisp))
            (eq (coerce adj 'vector) adj))))
  (t nil t (5 5 5 5) nil t t))

(deftest simple-array-predicates.coerce-to-simple-string
  (destructuring-bind (s1 s2 adj disp fp) (%sap-strings)
    (declare (ignore s1 fp))
    (let ((sa (coerce adj 'simple-string)) (sd (coerce disp 'simple-string)))
      (list (eq (coerce s2 'simple-string) s2)
            (eq sa adj) (%sap-bool (simple-string-p sa)) sa
            (eq sd disp) (%sap-bool (simple-string-p sd)) sd)))
  (t nil t "cccc" nil t "zzzz"))

(deftest simple-array-predicates.coerce-to-simple-bit-vector
  (destructuring-bind (plain adj disp fp) (%sap-vectors 'bit 1)
    (declare (ignore disp fp))
    (let ((sa (coerce adj 'simple-bit-vector)))
      (list (eq (coerce plain 'simple-bit-vector) plain)
            (eq sa adj) (%sap-bool (simple-bit-vector-p sa)) (coerce sa 'list)
            (eq (coerce adj 'bit-vector) adj))))
  (t nil t (1 1 1 1) t))

;; SUBTYPEP is about types, not objects, and was already right. Pinned so a
;; later change to the object test does not drag it along.
(deftest simple-array-predicates.subtypep-unchanged
  (list (multiple-value-list (subtypep 'simple-string 'simple-array))
        (multiple-value-list (subtypep 'simple-string 'string))
        (multiple-value-list (subtypep 'string 'simple-string))
        (multiple-value-list (subtypep 'simple-vector '(simple-array t (*)))))
  ((t t) (t t) (nil t) (t t)))
