;;; Neighbouring inputs for the recent GETHASH, COERCE and string comparison changes.

(defun %rn-extremes (test)
  (let ((h (make-hash-table :test test)))
    (setf (gethash most-positive-fixnum h) :mp
          (gethash most-negative-fixnum h) :mn
          (gethash 0 h) :zero
          (gethash (1+ most-positive-fixnum) h) :big)
    (list (gethash most-positive-fixnum h :miss)
          (gethash most-negative-fixnum h :miss)
          (gethash 0 h :miss)
          (if (eq test 'eq) :unspecified (gethash (1+ most-positive-fixnum) h :miss)))))

(deftest recent-neighbours.gethash-fixnum-extremes
  (mapcar #'%rn-extremes '(eq eql equal equalp))
  ((:mp :mn :zero :unspecified)
   (:mp :mn :zero :big)
   (:mp :mn :zero :big)
   (:mp :mn :zero :big)))

(defun %rn-cross (test)
  (let ((h (make-hash-table :test test)))
    (setf (gethash 1 h) :one
          (gethash 65 h) :sixty-five)
    (list (gethash 1.0d0 h :miss)
          (gethash 1.0f0 h :miss)
          (gethash #\A h :miss)
          (gethash 65.0f0 h :miss))))

(deftest recent-neighbours.gethash-cross-type-keys
  (mapcar #'%rn-cross '(eq eql equal equalp))
  ((:miss :miss :miss :miss)
   (:miss :miss :miss :miss)
   (:miss :miss :miss :miss)
   (:one :one :miss :sixty-five)))

(defun %rn-mutate (h k)
  (setf (gethash k h) :value)
  (let ((before (gethash k h :miss)))
    (list before (remhash k h) (gethash k h :miss))))

;; DOTCL FIXNUM is a full signed 64-bit integer, so (SIGNED-BYTE 64) adds no value.
(defun %rn-declared (h)
  (let ((a 11) (b 12) (c 13))
    (declare (type fixnum a) (type t b) (type (signed-byte 64) c))
    (setf (gethash a h) :fixnum
          (gethash b h) :t
          (gethash c h) :signed)
    (list (gethash a h :miss) (gethash b h :miss) (gethash c h :miss))))

(deftest recent-neighbours.gethash-mutate-and-declared
  (let ((h (make-hash-table :test 'eql)))
    (list (%rn-mutate h 123456789)
          (%rn-declared h)
          (eval '(let ((h (make-hash-table :test 'eql)))
                   (setf (gethash 7 h) :seven)
                   (list (gethash 7 h :miss) (remhash 7 h) (gethash 7 h :miss))))))
  ((:value t :miss) (:fixnum :t :signed) (:seven t :miss)))

(deftest recent-neighbours.gethash-weak-and-synchronized
  (let ((weak (make-hash-table :test 'eql :weakness :value))
        (sync (make-hash-table :test 'eql :synchronized t)))
    (setf (gethash most-positive-fixnum weak) :weak
          (gethash most-negative-fixnum sync) :sync)
    (list (gethash most-positive-fixnum weak :miss)
          (gethash most-negative-fixnum sync :miss)
          (remhash most-negative-fixnum sync)
          (gethash most-negative-fixnum sync :miss)))
  (:weak :sync t :miss))

(deftest recent-neighbours.coerce-fixnum-to-floats
  (let ((xs (list 0 -1 (ash 1 53) (1+ (ash 1 53))
                  most-positive-fixnum most-negative-fixnum)))
    (list (mapcar (lambda (x) (coerce x 'double-float)) xs)
          (mapcar (lambda (x) (coerce x 'single-float)) xs)))
  ((0.0d0 -1.0d0 9007199254740992.0d0 9007199254740992.0d0
    9223372036854775808.0d0 -9223372036854775808.0d0)
   (0.0f0 -1.0f0 9007199254740992.0f0 9007199254740992.0f0
    9223372036854775808.0f0 -9223372036854775808.0f0)))

(deftest recent-neighbours.coerce-bignum-to-double
  (list (coerce (ash 1 64) 'double-float)
        (coerce (ash 1 1023) 'double-float))
  (18446744073709551616.0d0 8.98846567431158d307))

(deftest recent-neighbours.coerce-other-targets
  (let ((x 5) (d 1.5d0) (s 0.1f0))
    (list (coerce x 'float)
          (coerce x 'real)
          (coerce x 'rational)
          (coerce x 'number)
          (coerce x 't)
          (coerce x 'complex)
          (coerce x '(complex double-float))
          (coerce d 'double-float)
          (= (coerce s 'double-float) 0.10000000149011612d0)))
  (5.0 5 5 5 5 5 #C(5.0d0 0.0d0) 1.5d0 t))

(deftest recent-neighbours.string-cmp-edges
  (list (string= "abc" "xyz" :start1 2 :end1 2 :start2 1 :end2 1)
        (string= "abc" "xyz" :start1 2 :end1 2 :start2 0 :end2 3)
        (string< "abc" "xyz" :start1 2 :end1 2 :start2 1 :end2 1)
        (string<= "abc" "xyz" :start1 2 :end1 2 :start2 1 :end2 1)
        (string>= "abc" "xyz" :start1 2 :end1 2 :start2 1 :end2 1)
        (string/= "abc" "xyz" :start1 2 :end1 2 :start2 1 :end2 1)
        (string-equal "xxABC" "yyabc" :start1 2 :start2 2)
        (string-not-equal "xxABC" "yyabd" :start1 2 :start2 2)
        (string-lessp "xxABC" "yyabd" :start1 2 :start2 2)
        (string< "abcdef" "abczzz" :start1 0 :end1 6 :start2 0 :end2 6)
        (string> "abczzz" "abcdef" :start1 0 :end1 6 :start2 0 :end2 6)
        (string< "abcdef" "abcdzz" :start1 0 :end1 6 :start2 0 :end2 6)
        (string< "ab" "abc" :start1 0 :end1 2 :start2 0 :end2 3)
        (string< "abc" "ab" :start1 0 :end1 3 :start2 0 :end2 2)
        (string<= "abcxyz" "abcxyz" :start1 1 :end1 4 :start2 1 :end2 4)
        (string>= "abcxyz" "abcxyz" :start1 1 :end1 4 :start2 1 :end2 4)
        (string/= "abcxyz" "abcxyz" :start1 1 :end1 4 :start2 1 :end2 4))
  (t nil nil 2 2 nil t 4 4 3 3 4 2 nil 4 4 nil))

(deftest recent-neighbours.string-cmp-designators-and-storage
  (let ((f (make-array 5 :element-type 'character :fill-pointer 3 :initial-element #\z)))
    (replace f "abc")
    (let* ((base (make-string 6 :initial-element #\.))
           (d (make-array 3 :element-type 'character :displaced-to base
                            :displaced-index-offset 2)))
      (replace base "abc" :start1 2)
      (let ((b (make-array 3 :element-type 'base-char :initial-contents "abc")))
        (list (string= f "abc" :start1 0 :end1 3 :start2 0 :end2 3)
              (string< f "abd" :start1 0 :end1 3 :start2 0 :end2 3)
              (string= d "abc" :start1 0 :end1 3 :start2 0 :end2 3)
              (string= d b :start1 0 :end1 3 :start2 0 :end2 3)
              (string= 'abc "ABC" :start1 0 :end1 3 :start2 0 :end2 3)
              (string= #\a "abc" :start1 0 :end1 1 :start2 0 :end2 1)
              (string-equal 'abc "ABC" :start1 0 :end1 3 :start2 0 :end2 3)
              (string-equal #\A "abc" :start1 0 :end1 1 :start2 0 :end2 1)))))
  (t 2 t t t t t t))

(deftest recent-neighbours.string-cmp-bounds-errors
  (list (handler-case (progn (string= "abc" "abc" :start1 0 :end1 99
                                      :start2 0 :end2 3) :no-error)
          (type-error () :type-error))
        (handler-case (progn (string< "abc" "abc" :start1 2 :end1 1
                                      :start2 0 :end2 3) :no-error)
          (type-error () :type-error))
        (handler-case (progn (string= "abc" "abc" :start1 -1 :end1 3
                                      :start2 0 :end2 3) :no-error)
          (type-error () :type-error))
        (handler-case (progn (string= "abc" "abc" :start1 0 :end1 3
                                      :start2 0 :end2 99) :no-error)
          (type-error () :type-error)))
  (:type-error :type-error :type-error :type-error))
