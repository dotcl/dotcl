;;; A structure slot whose declared type is a DEFTYPE for an integer range that fits an int64 is stored
;;; raw, like the same range written out: reads are raw, arithmetic on them is native, and the answers
;;; (including the checks on a store) are the same.

(deftype %ssd-sym () '(integer -2000000000 4000000000))
(deftype %ssd-sym64 () `(integer ,(- (ash 1 61)) ,(ash 1 61)))
(deftype %ssd-small () '(integer 0 100))
(deftype %ssd-alias () 'fixnum)
(deftype %ssd-wide () '(integer 0 #.(ash 1 70)))

(defstruct (%ssd-a (:constructor %ssd-a (raw small alias wide l)))
  (raw 0 :type %ssd-sym) (small 0 :type %ssd-small) (alias 0 :type %ssd-alias) (wide 0 :type %ssd-wide) l)

(defstruct (%ssd-b (:constructor %ssd-b (raw l))) (raw 0 :type %ssd-sym64) l)

(setf dotcl:*save-sil* t)

(defun %ssd-abs (x) (declare (optimize (speed 3) (safety 0))) (let ((v (%ssd-a-raw x))) (if (minusp v) (- v) v)))

(deftest struct-slot-deftype-raw.answers
  (let ((x (%ssd-a -7 5 -3 (ash 1 65) :l)))
    (setf (%ssd-a-raw x) 12 (%ssd-a-wide x) (ash 1 66))
    (list (%ssd-abs (%ssd-a -7 1 1 1 nil)) (%ssd-a-raw x) (%ssd-a-small x) (%ssd-a-alias x)
          (%ssd-a-wide x) (%ssd-a-l x)
          (handler-case (setf (%ssd-a-small x) 101) (type-error () :type-error))
          (handler-case (%ssd-a 0 0 0 -1 nil) (type-error () :type-error))
          (handler-case (setf (%ssd-a-raw x) (ash 1 33)) (type-error () :type-error))
          (let ((b (%ssd-b -5 :l))) (setf (%ssd-b-raw b) (- (ash 1 60))) (list (%ssd-b-raw b) (handler-case (setf (%ssd-b-raw b) (ash 1 62)) (type-error () :type-error))))
          (let ((c (copy-%ssd-a x))) (list (%ssd-a-raw c) (%ssd-a-wide c) (%ssd-a-l c)))))
  (7 12 5 -3 #.(ash 1 66) :l :type-error :type-error :type-error (#.(- (ash 1 60)) :type-error) (12 #.(ash 1 66) :L)))

(defun %ssd-abs64 (x) (declare (optimize (speed 3) (safety 0))) (let ((v (%ssd-b-raw x))) (if (minusp v) (- v) v)))

(deftest-emitting-only struct-slot-deftype-raw.reads-are-raw
  (let ((s (%flat-text (dotcl:function-sil #'%ssd-abs)))
        (s64 (%flat-text (dotcl:function-sil #'%ssd-abs64))))
    ;; a 32-bit class range behind a DEFTYPE is raw; a 64-bit class one stays boxed
    (list (and (search "StructRefLK" s) t) (and (search "IsTrueMinusp" s) t)
          (and (search "StructRefLK" s64) t)))
  (t nil nil))

(setf dotcl:*save-sil* nil)
