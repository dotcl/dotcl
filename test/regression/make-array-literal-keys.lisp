;;; (make-array dims :key value ...) with literal keywords goes through Runtime.MakeArrayKw: the
;;; same results and the same errors as the generic call, the forms evaluated in the order written.
;;; Every expected value below is what SBCL gives as well; the element contents the standard leaves
;;; unspecified (0 or NIL in a fresh general vector) print as :Z.

(defun %mak-desc (x)
  (if (arrayp x)
      (list :arr (array-dimensions x) (array-element-type x)
            (if (array-has-fill-pointer-p x) (fill-pointer x) :nofp) (adjustable-array-p x)
            (and (= (array-rank x) 1)
                 (mapcar (lambda (e) (if (member e '(0 nil)) :z e)) (coerce x 'list))))
      x))

(defmacro %mak-val (form)
  `(mapcar #'%mak-desc (multiple-value-list ,form)))

(defmacro %mak-err (form)
  `(handler-case (progn ,form :no-error) (error () :error)))

(deftest make-array-kw.val-1
  (%mak-val (make-array 3 ))
  ((:ARR (3) T :NOFP NIL (:Z :Z :Z))))

(deftest make-array-kw.val-2
  (%mak-val (make-array 3 :element-type 'character))
  ((:ARR (3) CHARACTER :NOFP NIL (#\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-3
  (%mak-val (make-array 3 :fill-pointer t))
  ((:ARR (3) T 3 T (:Z :Z :Z))))

(deftest make-array-kw.val-4
  (%mak-val (make-array 3 :element-type 'character :fill-pointer t))
  ((:ARR (3) CHARACTER 3 T (#\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-5
  (%mak-val (make-array 3 :fill-pointer 2))
  ((:ARR (3) T 2 T (:Z :Z))))

(deftest make-array-kw.val-6
  (%mak-val (make-array 3 :element-type 'character :fill-pointer 2))
  ((:ARR (3) CHARACTER 2 T (#\Nul #\Nul))))

(deftest make-array-kw.val-7
  (%mak-val (make-array 3 :fill-pointer nil))
  ((:ARR (3) T :NOFP NIL (:Z :Z :Z))))

(deftest make-array-kw.val-8
  (%mak-val (make-array 3 :element-type 'character :fill-pointer nil))
  ((:ARR (3) CHARACTER :NOFP NIL (#\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-9
  (%mak-val (make-array 3 :adjustable t))
  ((:ARR (3) T :NOFP T (:Z :Z :Z))))

(deftest make-array-kw.val-10
  (%mak-val (make-array 3 :element-type 'character :adjustable t))
  ((:ARR (3) CHARACTER :NOFP T (#\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-11
  (%mak-val (make-array 3 :adjustable t :fill-pointer t))
  ((:ARR (3) T 3 T (:Z :Z :Z))))

(deftest make-array-kw.val-12
  (%mak-val (make-array 3 :element-type 'character :adjustable t :fill-pointer t))
  ((:ARR (3) CHARACTER 3 T (#\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-13
  (%mak-val (make-array 3 :adjustable t :fill-pointer 2))
  ((:ARR (3) T 2 T (:Z :Z))))

(deftest make-array-kw.val-14
  (%mak-val (make-array 3 :element-type 'character :adjustable t :fill-pointer 2))
  ((:ARR (3) CHARACTER 2 T (#\Nul #\Nul))))

(deftest make-array-kw.val-15
  (%mak-val (make-array 3 :adjustable t :fill-pointer nil))
  ((:ARR (3) T :NOFP T (:Z :Z :Z))))

(deftest make-array-kw.val-16
  (%mak-val (make-array 3 :element-type 'character :adjustable t :fill-pointer nil))
  ((:ARR (3) CHARACTER :NOFP T (#\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-17
  (%mak-val (make-array 3 :adjustable nil))
  ((:ARR (3) T :NOFP NIL (:Z :Z :Z))))

(deftest make-array-kw.val-18
  (%mak-val (make-array 3 :element-type 'character :adjustable nil))
  ((:ARR (3) CHARACTER :NOFP NIL (#\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-19
  (%mak-val (make-array 3 :adjustable nil :fill-pointer t))
  ((:ARR (3) T 3 T (:Z :Z :Z))))

(deftest make-array-kw.val-20
  (%mak-val (make-array 3 :element-type 'character :adjustable nil :fill-pointer t))
  ((:ARR (3) CHARACTER 3 T (#\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-21
  (%mak-val (make-array 3 :adjustable nil :fill-pointer 2))
  ((:ARR (3) T 2 T (:Z :Z))))

(deftest make-array-kw.val-22
  (%mak-val (make-array 3 :element-type 'character :adjustable nil :fill-pointer 2))
  ((:ARR (3) CHARACTER 2 T (#\Nul #\Nul))))

(deftest make-array-kw.val-23
  (%mak-val (make-array 3 :adjustable nil :fill-pointer nil))
  ((:ARR (3) T :NOFP NIL (:Z :Z :Z))))

(deftest make-array-kw.val-24
  (%mak-val (make-array 3 :element-type 'character :adjustable nil :fill-pointer nil))
  ((:ARR (3) CHARACTER :NOFP NIL (#\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-25
  (%mak-val (make-array 0 ))
  ((:ARR (0) T :NOFP NIL NIL)))

(deftest make-array-kw.val-26
  (%mak-val (make-array 0 :element-type 'character))
  ((:ARR (0) CHARACTER :NOFP NIL NIL)))

(deftest make-array-kw.val-27
  (%mak-val (make-array 0 :fill-pointer t))
  ((:ARR (0) T 0 T NIL)))

(deftest make-array-kw.val-28
  (%mak-val (make-array 0 :element-type 'character :fill-pointer t))
  ((:ARR (0) CHARACTER 0 T NIL)))

(deftest make-array-kw.val-29
  (%mak-val (make-array 0 :fill-pointer nil))
  ((:ARR (0) T :NOFP NIL NIL)))

(deftest make-array-kw.val-30
  (%mak-val (make-array 0 :element-type 'character :fill-pointer nil))
  ((:ARR (0) CHARACTER :NOFP NIL NIL)))

(deftest make-array-kw.val-31
  (%mak-val (make-array 0 :adjustable t))
  ((:ARR (0) T :NOFP T NIL)))

(deftest make-array-kw.val-32
  (%mak-val (make-array 0 :element-type 'character :adjustable t))
  ((:ARR (0) CHARACTER :NOFP T NIL)))

(deftest make-array-kw.val-33
  (%mak-val (make-array 0 :adjustable t :fill-pointer t))
  ((:ARR (0) T 0 T NIL)))

(deftest make-array-kw.val-34
  (%mak-val (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
  ((:ARR (0) CHARACTER 0 T NIL)))

(deftest make-array-kw.val-35
  (%mak-val (make-array 0 :adjustable t :fill-pointer nil))
  ((:ARR (0) T :NOFP T NIL)))

(deftest make-array-kw.val-36
  (%mak-val (make-array 0 :element-type 'character :adjustable t :fill-pointer nil))
  ((:ARR (0) CHARACTER :NOFP T NIL)))

(deftest make-array-kw.val-37
  (%mak-val (make-array 0 :adjustable nil))
  ((:ARR (0) T :NOFP NIL NIL)))

(deftest make-array-kw.val-38
  (%mak-val (make-array 0 :element-type 'character :adjustable nil))
  ((:ARR (0) CHARACTER :NOFP NIL NIL)))

(deftest make-array-kw.val-39
  (%mak-val (make-array 0 :adjustable nil :fill-pointer t))
  ((:ARR (0) T 0 T NIL)))

(deftest make-array-kw.val-40
  (%mak-val (make-array 0 :element-type 'character :adjustable nil :fill-pointer t))
  ((:ARR (0) CHARACTER 0 T NIL)))

(deftest make-array-kw.val-41
  (%mak-val (make-array 0 :adjustable nil :fill-pointer nil))
  ((:ARR (0) T :NOFP NIL NIL)))

(deftest make-array-kw.val-42
  (%mak-val (make-array 0 :element-type 'character :adjustable nil :fill-pointer nil))
  ((:ARR (0) CHARACTER :NOFP NIL NIL)))

(deftest make-array-kw.val-43
  (%mak-val (make-array 5 ))
  ((:ARR (5) T :NOFP NIL (:Z :Z :Z :Z :Z))))

(deftest make-array-kw.val-44
  (%mak-val (make-array 5 :element-type 'character))
  ((:ARR (5) CHARACTER :NOFP NIL (#\Nul #\Nul #\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-45
  (%mak-val (make-array 5 :fill-pointer t))
  ((:ARR (5) T 5 T (:Z :Z :Z :Z :Z))))

(deftest make-array-kw.val-46
  (%mak-val (make-array 5 :element-type 'character :fill-pointer t))
  ((:ARR (5) CHARACTER 5 T (#\Nul #\Nul #\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-47
  (%mak-val (make-array 5 :fill-pointer 2))
  ((:ARR (5) T 2 T (:Z :Z))))

(deftest make-array-kw.val-48
  (%mak-val (make-array 5 :element-type 'character :fill-pointer 2))
  ((:ARR (5) CHARACTER 2 T (#\Nul #\Nul))))

(deftest make-array-kw.val-49
  (%mak-val (make-array 5 :fill-pointer nil))
  ((:ARR (5) T :NOFP NIL (:Z :Z :Z :Z :Z))))

(deftest make-array-kw.val-50
  (%mak-val (make-array 5 :element-type 'character :fill-pointer nil))
  ((:ARR (5) CHARACTER :NOFP NIL (#\Nul #\Nul #\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-51
  (%mak-val (make-array 5 :adjustable t))
  ((:ARR (5) T :NOFP T (:Z :Z :Z :Z :Z))))

(deftest make-array-kw.val-52
  (%mak-val (make-array 5 :element-type 'character :adjustable t))
  ((:ARR (5) CHARACTER :NOFP T (#\Nul #\Nul #\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-53
  (%mak-val (make-array 5 :adjustable t :fill-pointer t))
  ((:ARR (5) T 5 T (:Z :Z :Z :Z :Z))))

(deftest make-array-kw.val-54
  (%mak-val (make-array 5 :element-type 'character :adjustable t :fill-pointer t))
  ((:ARR (5) CHARACTER 5 T (#\Nul #\Nul #\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-55
  (%mak-val (make-array 5 :adjustable t :fill-pointer 2))
  ((:ARR (5) T 2 T (:Z :Z))))

(deftest make-array-kw.val-56
  (%mak-val (make-array 5 :element-type 'character :adjustable t :fill-pointer 2))
  ((:ARR (5) CHARACTER 2 T (#\Nul #\Nul))))

(deftest make-array-kw.val-57
  (%mak-val (make-array 5 :adjustable t :fill-pointer nil))
  ((:ARR (5) T :NOFP T (:Z :Z :Z :Z :Z))))

(deftest make-array-kw.val-58
  (%mak-val (make-array 5 :element-type 'character :adjustable t :fill-pointer nil))
  ((:ARR (5) CHARACTER :NOFP T (#\Nul #\Nul #\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-59
  (%mak-val (make-array 5 :adjustable nil))
  ((:ARR (5) T :NOFP NIL (:Z :Z :Z :Z :Z))))

(deftest make-array-kw.val-60
  (%mak-val (make-array 5 :element-type 'character :adjustable nil))
  ((:ARR (5) CHARACTER :NOFP NIL (#\Nul #\Nul #\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-61
  (%mak-val (make-array 5 :adjustable nil :fill-pointer t))
  ((:ARR (5) T 5 T (:Z :Z :Z :Z :Z))))

(deftest make-array-kw.val-62
  (%mak-val (make-array 5 :element-type 'character :adjustable nil :fill-pointer t))
  ((:ARR (5) CHARACTER 5 T (#\Nul #\Nul #\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-63
  (%mak-val (make-array 5 :adjustable nil :fill-pointer 2))
  ((:ARR (5) T 2 T (:Z :Z))))

(deftest make-array-kw.val-64
  (%mak-val (make-array 5 :element-type 'character :adjustable nil :fill-pointer 2))
  ((:ARR (5) CHARACTER 2 T (#\Nul #\Nul))))

(deftest make-array-kw.val-65
  (%mak-val (make-array 5 :adjustable nil :fill-pointer nil))
  ((:ARR (5) T :NOFP NIL (:Z :Z :Z :Z :Z))))

(deftest make-array-kw.val-66
  (%mak-val (make-array 5 :element-type 'character :adjustable nil :fill-pointer nil))
  ((:ARR (5) CHARACTER :NOFP NIL (#\Nul #\Nul #\Nul #\Nul #\Nul))))

(deftest make-array-kw.val-67
  (%mak-val (make-array 3 ))
  ((:ARR (3) T :NOFP NIL (:Z :Z :Z))))

(deftest make-array-kw.val-68
  (%mak-val (make-array 3 :fill-pointer 1))
  ((:ARR (3) T 1 T (:Z))))

(deftest make-array-kw.val-69
  (%mak-val (make-array 3 :adjustable t))
  ((:ARR (3) T :NOFP T (:Z :Z :Z))))

(deftest make-array-kw.val-70
  (%mak-val (make-array 3 :adjustable t :fill-pointer 1))
  ((:ARR (3) T 1 T (:Z))))

(deftest make-array-kw.val-71
  (%mak-val (make-array 3 :initial-element 7))
  ((:ARR (3) T :NOFP NIL (7 7 7))))

(deftest make-array-kw.val-72
  (%mak-val (make-array 3 :initial-element 7 :fill-pointer 1))
  ((:ARR (3) T 1 T (7))))

(deftest make-array-kw.val-73
  (%mak-val (make-array 3 :initial-element 7 :adjustable t))
  ((:ARR (3) T :NOFP T (7 7 7))))

(deftest make-array-kw.val-74
  (%mak-val (make-array 3 :initial-element 7 :adjustable t :fill-pointer 1))
  ((:ARR (3) T 1 T (7))))

(deftest make-array-kw.val-75
  (%mak-val (make-array 3 :initial-element nil))
  ((:ARR (3) T :NOFP NIL (:Z :Z :Z))))

(deftest make-array-kw.val-76
  (%mak-val (make-array 3 :initial-element nil :fill-pointer 1))
  ((:ARR (3) T 1 T (:Z))))

(deftest make-array-kw.val-77
  (%mak-val (make-array 3 :initial-element nil :adjustable t))
  ((:ARR (3) T :NOFP T (:Z :Z :Z))))

(deftest make-array-kw.val-78
  (%mak-val (make-array 3 :initial-element nil :adjustable t :fill-pointer 1))
  ((:ARR (3) T 1 T (:Z))))

(deftest make-array-kw.val-79
  (%mak-val (make-array 3 :initial-contents '(a b c) :adjustable t :fill-pointer 2))
  ((:ARR (3) T 2 T (A B))))

(deftest make-array-kw.val-80
  (%mak-val (make-array 3 :element-type 'character :initial-contents "xyz" :fill-pointer t))
  ((:ARR (3) CHARACTER 3 T (#\x #\y #\z))))

(deftest make-array-kw.val-81
  (%mak-val (make-array '(2 2) :initial-contents '((1 2) (3 4)) :adjustable t))
  ((:ARR (2 2) T :NOFP T NIL)))

(deftest make-array-kw.val-82
  (%mak-val (make-array '(2 3) :adjustable t :initial-element 0))
  ((:ARR (2 3) T :NOFP T NIL)))

(deftest make-array-kw.val-83
  (%mak-val (make-array '(2 3) :element-type 'bit :adjustable t))
  ((:ARR (2 3) BIT :NOFP T NIL)))

(deftest make-array-kw.val-84
  (%mak-val (make-array 4 :element-type 'bit :initial-element 1 :adjustable t :fill-pointer 2))
  ((:ARR (4) BIT 2 T (1 1))))

(deftest make-array-kw.val-85
  (%mak-val (make-array 3 :element-type '(unsigned-byte 8) :initial-element 255 :adjustable t))
  ((:ARR (3) (UNSIGNED-BYTE 8) :NOFP T (255 255 255))))

(deftest make-array-kw.val-86
  (%mak-val (make-array 2 :element-type 'double-float :initial-element 1.5d0 :fill-pointer t))
  ((:ARR (2) DOUBLE-FLOAT 2 T (1.5d0 1.5d0))))

(deftest make-array-kw.val-87
  (%mak-val (let ((b (make-array 6 :initial-contents '(0 1 2 3 4 5)))) (make-array 3 :displaced-to b :displaced-index-offset 2)))
  ((:ARR (3) T :NOFP T (2 3 4))))

(deftest make-array-kw.val-88
  (%mak-val (let ((b (make-array 6 :initial-contents '(0 1 2 3 4 5)))) (make-array 3 :displaced-to b)))
  ((:ARR (3) T :NOFP T (:Z 1 2))))

(deftest make-array-kw.val-89
  (%mak-val (let ((b (make-array 6 :initial-contents '(0 1 2 3 4 5)))) (make-array 3 :displaced-to b :displaced-index-offset 3 :fill-pointer 1)))
  ((:ARR (3) T 1 T (3))))

(deftest make-array-kw.val-90
  (%mak-val (let ((b (make-array 6 :element-type 'character :initial-element #\a))) (make-array 3 :displaced-to b :element-type 'character :adjustable t)))
  ((:ARR (3) CHARACTER :NOFP T (#\a #\a #\a))))

(deftest make-array-kw.val-91
  (%mak-val (let ((b (make-array 6 :initial-contents '(0 1 2 3 4 5)))) (make-array 3 :displaced-to nil)))
  ((:ARR (3) T :NOFP NIL (:Z :Z :Z))))

(deftest make-array-kw.val-92
  (%mak-val (let ((n 4)) (make-array n :adjustable t :fill-pointer (- n 1))))
  ((:ARR (4) T 3 T (:Z :Z :Z))))

(deftest make-array-kw.val-95
  (%mak-val (let ((v (vector 1 2 3))) (make-array 2 :displaced-to v :displaced-index-offset 1 :adjustable nil)))
  ((:ARR (2) T :NOFP T (2 3))))

(deftest make-array-kw.err-96
  (%mak-err (make-array 3 :initial-element 1 :initial-contents '(1 2 3)))
  :error)

(deftest make-array-kw.err-97
  (%mak-err (make-array 3 :initial-contents '(1 2)))
  :error)

(deftest make-array-kw.err-98
  (%mak-err (make-array 3 :initial-contents '(1 2 3 4) :adjustable t))
  :error)

(deftest make-array-kw.err-100
  (%mak-err (make-array 3 :fill-pointer -1 :adjustable t))
  :error)

(deftest make-array-kw.err-101
  (%mak-err (make-array 3 :fill-pointer 'x))
  :error)

(deftest make-array-kw.err-102
  (%mak-err (make-array '(2 2) :fill-pointer t))
  :error)

(deftest make-array-kw.err-103
  (%mak-err (make-array -1 :adjustable t))
  :error)

(deftest make-array-kw.err-104
  (%mak-err (make-array 'x :adjustable t))
  :error)

(deftest make-array-kw.err-105
  (%mak-err (make-array 3 :element-type 'character :initial-element 1 :adjustable t))
  :error)

(deftest make-array-kw.err-106
  (%mak-err (make-array 3 :element-type 'bit :initial-element 2 :adjustable t))
  :error)

(deftest make-array-kw.err-107
  (%mak-err (make-array 3 :element-type '(unsigned-byte 8) :initial-element -1 :fill-pointer t))
  :error)

(deftest make-array-kw.err-108
  (%mak-err (make-array 3 :element-type 'nonexistent-type-zz :adjustable t))
  :error)

(deftest make-array-kw.err-109
  (%mak-err (let ((b (make-array 2))) (make-array 3 :displaced-to b)))
  :error)

(deftest make-array-kw.err-110
  (%mak-err (let ((b (make-array 4))) (make-array 3 :displaced-to b :displaced-index-offset 2)))
  :error)

(deftest make-array-kw.err-111
  (%mak-err (let ((b (make-array 4))) (make-array 3 :displaced-to b :displaced-index-offset -1)))
  :error)

(deftest make-array-kw.err-112
  (%mak-err (let ((b (make-array 4))) (make-array 3 :displaced-to b :initial-element 1)))
  :error)

(deftest make-array-kw.err-113
  (%mak-err (let ((b (make-array 4))) (make-array 3 :displaced-to b :initial-contents '(1 2 3))))
  :error)

(deftest make-array-kw.err-116
  (%mak-err (make-array 3 :displaced-to 5))
  :error)

(deftest make-array-kw.err-117
  (%mak-err (make-array 3 :displaced-index-offset 1))
  :error)

(deftest make-array-kw.err-118
  (%mak-err (make-array 3 :displaced-index-offset 'x :displaced-to (make-array 5)))
  :error)

(deftest make-array-kw.err-119
  (%mak-err (make-array 3 :adjustable t :bogus 1))
  :error)

(deftest make-array-kw.val-120
  (%mak-val (make-array 3 :adjustable t :allow-other-keys t :bogus 1))
  ((:ARR (3) T :NOFP T (:Z :Z :Z))))

(deftest make-array-kw.val-121
  (%mak-val (make-array 3 :adjustable t :adjustable nil))
  ((:ARR (3) T :NOFP T (:Z :Z :Z))))

(deftest make-array-kw.val-122
  (%mak-val (make-array 3 :fill-pointer 1 :fill-pointer 2))
  ((:ARR (3) T 1 T (:Z))))

(deftest make-array-kw.err-123
  (%mak-err (make-array 3 :adjustable))
  :error)

(deftest make-array-kw.val-124
  (%mak-val (let ((k :adjustable)) (make-array 3 k t)))
  ((:ARR (3) T :NOFP T (:Z :Z :Z))))

(deftest make-array-kw.val-125
  (%mak-val (apply #'make-array 3 '(:adjustable t :fill-pointer 1)))
  ((:ARR (3) T 1 T (:Z))))

(deftest make-array-kw.val-126
  (%mak-val (funcall #'make-array 3 :adjustable t :fill-pointer 2))
  ((:ARR (3) T 2 T (:Z :Z))))

(deftest make-array-kw.val-127
  (%mak-val (let ((a (make-array 0 :element-type 'character :adjustable t :fill-pointer t))) (dotimes (i 5) (vector-push-extend (code-char (+ 97 i)) a)) (list a (length a) (fill-pointer a) (coerce a 'simple-string))))
  (("abcde" 5 5 "abcde")))

(deftest make-array-kw.val-129
  (%mak-val (let ((a (make-array 2 :adjustable t :fill-pointer 2))) (adjust-array a 5 :fill-pointer 4) (list (length a) (array-dimensions a) (array-total-size a))))
  ((4 (5) 5)))

(deftest make-array-kw.val-130
  (%mak-val (let ((a (make-array 2 :adjustable t :initial-element 'z))) (adjust-array a 4 :initial-element 'y) a))
  ((:ARR (4) T :NOFP T (Z Z Y Y))))

(deftest make-array-kw.val-131
  (%mak-val (let ((a (make-array '(2 2) :adjustable t :initial-element 0))) (setf (aref a 1 1) 9) (adjust-array a '(3 3)) (aref a 1 1)))
  (9))

(deftest make-array-kw.eval-order-fp-adj
  (let ((l nil))
    (make-array (progn (push :dims l) 2) :fill-pointer (progn (push :fp l) 1) :adjustable (progn (push :adj l) t))
    (reverse l))
  (:dims :fp :adj))

(deftest make-array-kw.eval-order-adj-fp
  (let ((l nil))
    (make-array (progn (push :dims l) 2) :adjustable (progn (push :adj l) t) :fill-pointer (progn (push :fp l) 1))
    (reverse l))
  (:dims :adj :fp))

(deftest make-array-kw.eval-order-side-effect-reads
  (let ((x 1))
    (let ((a (make-array 4 :initial-element x :fill-pointer (incf x) :adjustable t)))
      (list (aref a 0) (fill-pointer a) x)))
  (1 2 2))

;; Of two invalid keywords the first written is the one reported, as in the generic call.
(deftest make-array-kw.first-invalid-keyword-fill-pointer-first
  (handler-case (make-array 3 :fill-pointer 9 :element-type 'make-array-kw-no-such-type)
    (type-error () :type-error)
    (error () :other-error))
  :type-error)

(deftest make-array-kw.first-invalid-keyword-element-type-first
  (handler-case (make-array 3 :element-type 'make-array-kw-no-such-type :fill-pointer 9)
    (type-error () :type-error)
    (error () :other-error))
  :other-error)

(deftest make-array-kw.initial-element-only-unchanged
  (let ((a (make-array 8 :initial-element 0)))
    (list (length a) (aref a 7) (adjustable-array-p a)))
  (8 0 nil))
