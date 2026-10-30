;;; FORMAT appends to a fill-pointer string and FRESH-LINE counts its text.

(deftest format-fill-pointer.append
  (let ((s (make-array 3 :element-type 'character :fill-pointer 3
                         :adjustable t :initial-contents "abc")))
    (list (format s "de~d" 4) (coerce s 'string) (fill-pointer s)))
  (nil "abcde4" 6))

(deftest format-fill-pointer.existing-column
  (let ((s (make-array 6 :element-type 'character :fill-pointer 3
                         :adjustable t :initial-contents "abc___")))
    (format s "~&xy")
    (list (coerce s 'string) (fill-pointer s)))
  (#.(format nil "abc~%xy") 6))

(deftest fresh-line-fill-pointer.column-before-text
  (let ((s (make-array 3 :element-type 'character :fill-pointer 3
                         :adjustable t :initial-contents "pre")))
    (with-output-to-string (out s)
      (let ((r (fresh-line out)))
        (write-string "x" out)
        (list r (coerce s 'string) (fill-pointer s)))))
  (t #.(format nil "pre~%x") 5))

(deftest fresh-line-fill-pointer.empty
  (let ((s (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (with-output-to-string (out s)
      (let ((r (fresh-line out)))
        (write-string "x" out)
        (list r (coerce s 'string)))))
  (nil "x"))

(deftest fresh-line-fill-pointer.ends-in-newline
  (let ((s (make-array 4 :element-type 'character :fill-pointer 2
                         :adjustable t :initial-contents #.(format nil "a~%__"))))
    (with-output-to-string (out s)
      (let ((r (fresh-line out)))
        (write-string "x" out)
        (list r (coerce s 'string) (fill-pointer s)))))
  (nil #.(format nil "a~%x") 3))
