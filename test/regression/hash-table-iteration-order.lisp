;;; MAPHASH, WITH-HASH-TABLE-ITERATOR and LOOP's hash-key / hash-value paths
;;; walk one table in the same order: insertion order when nothing was removed,
;;; as SBCL does. The standard leaves the order open, but libraries serialize
;;; tables by iterating them and compare the text (lquery's CSS test writes a
;;; style attribute back out and expects "color:red;foo:bar;").
;;;
;;; WITH-HASH-TABLE-ITERATOR and LOOP used to walk backwards while MAPHASH
;;; walked forwards.

(defun htorder-table (test)
  (let ((h (make-hash-table :test test)))
    (setf (gethash "color" h) 1
          (gethash "foo" h) 2
          (gethash "abc" h) 3
          (gethash "zz" h) 4)
    h))

(defun htorder-maphash (h)
  (let ((keys '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k keys)) h)
    (nreverse keys)))

(defun htorder-iterator (h)
  (let ((keys '()))
    (with-hash-table-iterator (next h)
      (loop (multiple-value-bind (more k) (next)
              (unless more (return))
              (push k keys))))
    (nreverse keys)))

(deftest hash-table-iteration-order-maphash
  (htorder-maphash (htorder-table 'equal))
  ("color" "foo" "abc" "zz"))

(deftest hash-table-iteration-order-with-hash-table-iterator
  (htorder-iterator (htorder-table 'equal))
  ("color" "foo" "abc" "zz"))

(deftest hash-table-iteration-order-loop-hash-keys
  (let ((h (htorder-table 'equalp)))
    (loop for k being the hash-keys of h collect k))
  ("color" "foo" "abc" "zz"))

(deftest hash-table-iteration-order-loop-hash-values
  (let ((h (htorder-table 'equal)))
    (loop for v being the hash-values of h using (hash-key k) collect (cons k v)))
  (("color" . 1) ("foo" . 2) ("abc" . 3) ("zz" . 4)))

(deftest hash-table-iteration-order-all-agree
  (let ((h (htorder-table 'equal)))
    (equal (htorder-maphash h) (htorder-iterator h)))
  t)
