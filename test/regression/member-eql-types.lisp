;;; MEMBER (two arguments, and :TEST #'EQL) on lists of characters, fixnums, floats, bignums, ratios, complexes
;;; and symbols: EQL semantics of every type, at the boundaries, and the keyword forms.

(defvar *met-chars* (list #\a (code-char 0) (code-char 255) (code-char 256) (code-char 65535) #\Space))
(defvar *met-nums* (list 1 -1 0 most-positive-fixnum most-negative-fixnum 4611686018427387904 (expt 10 30)
                         1.0 -0.0 0.0 1.5d0 -0.0d0 1/2 -3/4 #c(1 2) #c(1.0 2.0) 255 256 65535 65536))

(deftest member-eql-types.chars
  (list (length (member #\a *met-chars*)) (length (member (code-char 0) *met-chars*))
        (length (member (code-char 255) *met-chars*)) (length (member (code-char 256) *met-chars*))
        (length (member (code-char 65535) *met-chars*)) (member #\b *met-chars*)
        (length (member (code-char 97) *met-chars*)) (member 97 *met-chars*)
        (length (member #\Space *met-chars* :test #'eql))
        (length (member #\A *met-chars* :test #'char-equal)))
  (6 5 4 3 2 nil 6 nil 1 6))

(deftest member-eql-types.numbers
  (flet ((pos (x) (let ((m (member x *met-nums*))) (and m (- (length *met-nums*) (length m))))))
    (list (pos 1) (pos -1) (pos 0) (pos most-positive-fixnum) (pos most-negative-fixnum)
          (pos 4611686018427387904) (pos (expt 10 30)) (pos (1+ (expt 10 30)))
          (pos 1.0) (pos 1.0d0) (pos 0.0) (pos -0.0) (pos -0.0d0) (pos 0.0d0) (pos 1.5d0)
          (pos 1/2) (pos 2/4) (pos -3/4) (pos #c(1 2)) (pos #c(1.0 2.0)) (pos #c(2 1))
          (pos 255) (pos 256) (pos 65535) (pos 65536) (pos 65537) (pos #\a) (pos 'a)))
  (0 1 2 3 4 5 6 nil 7 nil 9 8 11 nil 10 12 12 13 14 15 nil 16 17 18 19 nil nil nil))

(deftest member-eql-types.symbols-and-keywords
  (list (member 'c '(a b c d)) (member 'z '(a b c d)) (member nil '(a nil c)) (member nil '(a b))
        (member 3 '(1 2 3 4) :test #'=) (member 3 '(1 2 3 4) :test-not #'eql)
        (member 3 '((1) (2) (3)) :key #'car) (member 4 '(1 2 3 4 5 6 4 8))
        (member "a" '("a" "b") :test #'string=) (member "a" '("a" "b")))
  ((c d) nil (nil c) nil (3 4) (1 2 3 4) ((3)) (4 5 6 4 8) ("a" "b") nil))

(deftest member-eql-types.errors
  (list (handler-case (member #\a '(#\b #\c . 5)) (type-error () :type-error))
        (handler-case (member 3 '(1 2 . 3)) (type-error () :type-error))
        (member #\a '(#\a . 5)) (member 3 '(3 . 5))
        (handler-case (member 1 5) (type-error () :type-error)))
  (:type-error :type-error (#\a . 5) (3 . 5) :type-error))
