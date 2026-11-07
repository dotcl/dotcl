;;; MAKE-PATHNAME keeps a string without * ? [ as the very object it was given, so a
;;; name and a type built from one string stay EQL.

(deftest make-pathname-shared-string.name-type
  (let* ((x (copy-seq "foo")) (p (make-pathname :name x :type x)))
    (list (eql (pathname-name p) (pathname-type p)) (eql (pathname-name p) x)))
  (t t))

;; * ? [ are quoted off Windows only; there a string with them is wild and is kept as given.
#-windows
(deftest make-pathname-shared-string.quoted-copy
  (let* ((x (copy-seq "a*b")) (p (make-pathname :name x)))
    (list (string= (pathname-name p) x) (wild-pathname-p p :name)))
  (t nil))

#+windows
(deftest make-pathname-shared-string.wild-kept
  (let* ((x (copy-seq "a*b")) (p (make-pathname :name x)))
    (list (eql (pathname-name p) x) (and (wild-pathname-p p :name) t)))
  (t t))
