;;; A #n= label may be defined only once in the object being read (CLHS
;;; 2.4.8.15); the second definition used to replace the first.

(defun %rdl-read (string)
  (handler-case (values (read-from-string string))
    (reader-error () :reader-error)))

(deftest reader-duplicate-label.twice
  (list (%rdl-read "(#1=a #1=b)") (%rdl-read "(#1=(x) #1=(y))") (%rdl-read "#1=(#1=a)"))
  (:reader-error :reader-error :reader-error))

(deftest reader-duplicate-label.distinct-and-shared
  (let ((x (%rdl-read "(#1=a #2=b #1# #2#)")))
    x)
  (a b a b))

(deftest reader-duplicate-label.circular
  (let ((x (%rdl-read "#1=(a . #1#)")))
    (and (eq x (cdr x)) (car x)))
  a)

(deftest reader-duplicate-label.new-read-may-reuse
  (list (%rdl-read "#1=(a)") (%rdl-read "#1=(b)"))
  ((a) (b)))
