;;; EQUALP on the common cases: numbers by =, characters by CHAR-EQUAL,
;;; strings case-insensitively, conses and structures elementwise, symbols by
;;; identity. Called through a function, through FUNCALL, and as a test.

(defstruct eqpc-a x y)
(defstruct eqpc-b x y)

(defun %eqpc (a b) (equalp a b))
(defun %eqpc-if (a b) (if (equalp a b) t nil))

(defun %eqpc-all (a b)
  (let ((r (list (%eqpc a b) (funcall #'equalp a b) (%eqpc-if a b)
                 (%eqpc b a) (funcall 'equalp b a) (%eqpc-if b a))))
    (cond ((every #'identity r) t)
          ((notany #'identity r) nil)
          (t (list :mixed r)))))

(deftest equalp-common-cases.numbers
  (let ((mpf most-positive-fixnum))
    (mapcar (lambda (p) (%eqpc-all (car p) (cdr p)))
            (list (cons 1 1) (cons 1 2) (cons -5 -5)
                  (cons mpf (1+ mpf)) (cons (1+ mpf) (+ mpf 1))
                  (cons (1- most-negative-fixnum) (1- most-negative-fixnum))
                  (cons 1 1.0d0) (cons 1 1.0f0) (cons 1 1.5d0) (cons 2 1.0d0)
                  (cons 1.0d0 1.0f0) (cons 0.0d0 -0.0d0) (cons 1/2 0.5d0)
                  (cons 1/2 1/3) (cons #c(1.0 0.0) 1) (cons #c(1 2) #c(1.0 2.0))
                  (cons 1 "1") (cons 1 #\1) (cons 1 nil))))
  (t nil t nil t t t t nil nil t t t nil t t nil nil nil))

(deftest equalp-common-cases.characters-and-strings
  (mapcar (lambda (p) (%eqpc-all (car p) (cdr p)))
          (list (cons #\a #\A) (cons #\a #\b) (cons #\a "a") (cons #\a 97)
                (cons (copy-seq "abc") (copy-seq "ABC")) (cons "abc" "abd")
                (cons "ab" "abc") (cons "" (copy-seq ""))
                (cons "abc" (make-array 3 :element-type 'character :adjustable t
                                          :initial-contents "ABC"))
                (cons "abc" (vector #\A #\b #\C))
                (cons "abc" 'abc)))
  (t nil nil nil t nil nil t t t nil))

(deftest equalp-common-cases.conses-symbols-structures
  (mapcar (lambda (p) (%eqpc-all (car p) (cdr p)))
          (list (cons (list 1 "A" #\b) (list 1.0 "a" #\B))
                (cons (cons 1 2) (cons 1 2.0))
                (cons (list 1 2) (list 1 2 3))
                (cons (list 1) nil)
                (cons :a :a) (cons :a :b) (cons nil '()) (cons t t) (cons 'a "A")
                (cons (make-eqpc-a :x 1 :y "S") (make-eqpc-a :x 1.0 :y "s"))
                (cons (make-eqpc-a :x 1 :y 2) (make-eqpc-a :x 1 :y 3))
                (cons (make-eqpc-a :x 1 :y 2) (make-eqpc-b :x 1 :y 2))
                (cons (make-eqpc-a) nil)
                (cons #*101 (make-array 3 :element-type 'bit :initial-contents '(1 0 1)))))
  (t t nil nil t nil t t nil t nil nil nil t))

(deftest equalp-common-cases.hash-table
  (let ((h (make-hash-table :test 'equalp)))
    (setf (gethash 1 h) :one (gethash "abc" h) :abc (gethash #\a h) :a
          (gethash (list 1 "x") h) :list)
    (list (gethash 1.0d0 h) (gethash 1 h) (gethash 2 h) (gethash "ABC" h)
          (gethash #\A h) (gethash (list 1.0 "X") h)))
  (:one :one nil :abc :a :list))

(deftest equalp-common-cases.hash-table-string-keys
  (let ((h (make-hash-table :test 'equalp))
        (cv (make-array 5 :element-type 'character :adjustable t :initial-contents "KeY-1")))
    (setf (gethash "key-1" h) :k1 (gethash cv h) :again (gethash "KEY-2" h) :k2)
    (list (hash-table-count h) (gethash "KEY-1" h) (gethash "key-2" h)
          (gethash (make-array 5 :element-type 'character :adjustable t :initial-contents "kEy-2") h)
          (gethash "key-3" h) (gethash "" h)))
  (2 :again :k2 :k2 nil nil))
