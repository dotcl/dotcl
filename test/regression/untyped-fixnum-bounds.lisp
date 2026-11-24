;;; (SETF (SCHAR S I) C) with a fixnum subscript, a character vector made by MAKE-ARRAY, and a
;;; LOOP FOR I OF-TYPE FIXNUM FROM START BELOW END whose START and END are not declared: none builds a
;;; Fixnum per iteration (a subscript above the cache), and the errors are the same.

(defun %ufb-schar-fill (s n)
  (declare (type (simple-array character (*)) s) (fixnum n))
  (dotimes (i n) (setf (schar s i) #\a))
  s)

(defun %ufb-array-fill (n)
  (let ((s (make-array n :element-type 'character)))
    (loop for j of-type fixnum from 0 below n do (setf (schar s j) #\b))
    s))

(defun %ufb-find (s start end)
  (loop for i of-type fixnum from start below end
        thereis (and (char= (char s i) #\b) i)))

(defun %ufb-sum (start end)
  (let ((n 0))
    (declare (fixnum n))
    (loop for i of-type fixnum from start below end do (incf n i))
    n))

(deftest untyped-fixnum-bounds.results
  (list (let ((s (make-string 5 :initial-element #\x))) (%ufb-schar-fill s 3) s)
        (%ufb-array-fill 4)
        (%ufb-find "aabab" 0 5) (%ufb-find "aabab" 3 5) (%ufb-find "aaaa" 0 4)
        (%ufb-sum 0 10) (%ufb-sum 5 5) (%ufb-sum 7 3))
  ("aaaxx" "bbbb" 2 4 nil 45 0 0))

(deftest untyped-fixnum-bounds.errors
  (list (handler-case (%ufb-find "abc" "x" 3) (error (e) (type-of e)))
        (handler-case (%ufb-find "abc" 1.5 3) (error (e) (type-of e)))
        (handler-case (let ((s (make-array 3 :element-type 'character))) (setf (schar s 3) #\a)) (error (e) (type-of e))))
  (type-error type-error type-error))

(defun %ufb-big (n) (%ufb-array-fill n))
(defun %ufb-big-schar (n) (%ufb-schar-fill (make-string n) n))
(defun %ufb-big-find (s n) (%ufb-find s 0 n))
(defun %ufb-big-sum (n) (%ufb-sum 0 n))

(deftest-compiled-only untyped-fixnum-bounds.no-fixnum-per-iteration
  (let ((n 300000) (s (make-string 300000 :initial-element #\a)))
    (%ufb-big 10) (%ufb-big-schar 10) (%ufb-big-find s 10) (%ufb-big-sum 10)
    ;; The array itself is 2 bytes a character (600 KB); a Fixnum per iteration would be 7 MB more.
    (list (< (min-allocated-bytes (lambda () (%ufb-big n))) 1500000)
          (< (min-allocated-bytes (lambda () (%ufb-big-schar n))) 1500000)
          (< (min-allocated-bytes (lambda () (%ufb-big-find s n))) 100000)
          (< (min-allocated-bytes (lambda () (%ufb-big-sum n))) 100000)))
  (t t t t))
