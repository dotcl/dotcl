;;; (coerce <non-simple string> 'simple-string) makes a fresh simple string of the
;;; active elements.

(defun %cas-adj (s)
  (let ((b (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
    (loop for c across s do (vector-push-extend c b))
    b))

(deftest coerce-adjustable-string.adjustable
  (let* ((b (%cas-adj "hello")) (r (coerce b 'simple-string)))
    (list r (simple-string-p r) (eq r b) (type-of r)))
  ("hello" t nil (simple-array character (5))))

(deftest coerce-adjustable-string.fill-pointer-respected
  (let ((b (make-array 8 :element-type 'character :initial-element #\x :adjustable t :fill-pointer 3)))
    (coerce b 'simple-string))
  "xxx")

(deftest coerce-adjustable-string.empty
  (let ((r (coerce (%cas-adj "") 'simple-string)))
    (list r (length r) (simple-string-p r)))
  ("" 0 t))

(deftest coerce-adjustable-string.fresh-copy
  (let* ((b (%cas-adj "abc")) (r (coerce b 'simple-string)))
    (vector-push-extend #\d b)
    (setf (char b 0) #\z)
    (list r b))
  ("abc" "zbcd"))

(deftest coerce-adjustable-string.displaced
  (let* ((base (copy-seq "0123456789"))
         (d (make-array 4 :element-type 'character :displaced-to base :displaced-index-offset 3))
         (r (coerce d 'simple-string)))
    (setf (char base 3) #\X)
    (list r (simple-string-p r)))
  ("3456" t))

(deftest coerce-adjustable-string.simple-unchanged
  (let ((s (copy-seq "abc")))
    (eq s (coerce s 'simple-string)))
  t)

(deftest coerce-adjustable-string.string-target-keeps-object
  (let ((b (%cas-adj "abc")))
    (eq b (coerce b 'string)))
  t)

(deftest coerce-adjustable-string.non-string-adjustable-vector
  (coerce (make-array 2 :adjustable t :fill-pointer 2 :initial-contents '(#\a #\b)) 'simple-string)
  "ab")

(deftest coerce-adjustable-string.non-character-element-errors
  (handler-case (coerce (make-array 2 :adjustable t :fill-pointer 2 :initial-contents '(1 2)) 'simple-string)
    (error () :error))
  :error)

(deftest coerce-adjustable-string.unicode
  (coerce (%cas-adj (coerce (list (code-char 955) #\a (code-char 12354)) 'string)) 'simple-string)
  #.(coerce (list (code-char 955) #\a (code-char 12354)) 'string))
