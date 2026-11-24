;;; (ARRAY X) names the arrays whose element type is what X upgrades to (CLHS 15.1.2.1),
;;; so TYPEP of a MAKE-ARRAY result against (ARRAY X), UPGRADED-ARRAY-ELEMENT-TYPE and
;;; ARRAY-ELEMENT-TYPE say the same thing, whatever the form of X. Sets of characters
;;; and of integers, the empty type and EXTENDED-CHAR used to upgrade to CHARACTER or T
;;; while TYPEP compared them differently.

(defparameter *aetc-specs*
  '(t nil bit character base-char standard-char extended-char integer unsigned-byte signed-byte
    (unsigned-byte 1) (unsigned-byte 2) (unsigned-byte 8) (unsigned-byte 9) (unsigned-byte 16) (unsigned-byte 33)
    (signed-byte 8) (signed-byte 9) (signed-byte 32) (integer 0 1) (integer 0 255) (integer 0 256) (integer 3 5)
    (mod 2) (mod 3) (eql 0) (eql 7) (member 0 1) (member 1 2 3) (member 0 255) (member -1 1)
    (eql #\a) (member #\a #\b) (member #\a #\b #\c) (member #\a #\é) (member #\é) (member nil) (member a b) (member 1 a)
    single-float double-float float real number (complex single-float) (complex double-float) (complex rational) complex
    (or bit character) (or character null) (or (eql #\a) (eql #\b)) (or (integer 0 1) (integer 2 3))
    (or (integer 0 100) (integer 200 300)) (or single-float double-float)
    (and character (not base-char)) (and integer (not (eql 0))) (and unsigned-byte (integer 0 7))
    (and character (member #\a #\b)) (satisfies characterp) (not character) (not t)))

(defun %aetc-same-p (a b)
  (or (equal a b) (and (member a '(fixnum (signed-byte 64)) :test #'equal)
                       (member b '(fixnum (signed-byte 64)) :test #'equal))))

(deftest array-element-type-consistency.typep-matches-upgrade
  (let ((bad '()))
    (dolist (e1 *aetc-specs*)
      (let ((a (make-array 2 :element-type e1)))
        (dolist (e2 *aetc-specs*)
          (let ((want (%aetc-same-p (upgraded-array-element-type e1) (upgraded-array-element-type e2))))
            (unless (and (eq want (and (typep a (list 'array e2)) t))
                         (eq want (and (typep a (list 'vector e2 2)) t))
                         (eq want (and (typep a (list 'simple-array e2 '(*))) t)))
              (push (list e1 e2) bad))))))
    bad)
  nil)

(deftest array-element-type-consistency.array-element-type-is-upgraded
  (loop for e in *aetc-specs*
        unless (equal (array-element-type (make-array 1 :element-type e)) (upgraded-array-element-type e))
          collect e)
  nil)

(deftest array-element-type-consistency.upgrading-is-monotonic
  (list (upgraded-array-element-type '(member 0 1))
        (upgraded-array-element-type '(member 0 255))
        (upgraded-array-element-type '(member -1 1))
        (upgraded-array-element-type '(or (integer 0 1) (integer 2 3)))
        (upgraded-array-element-type '(eql #\a))
        (upgraded-array-element-type '(member #\a #\b))
        (upgraded-array-element-type 'extended-char)
        (upgraded-array-element-type '(not t))
        (upgraded-array-element-type '(member #\a #\é)))
  (bit (unsigned-byte 8) (signed-byte 8) (unsigned-byte 2) base-char base-char
   character nil character))

(deftest array-element-type-consistency.strings
  (list (typep "aaa" '(vector character)) (typep "aaa" '(vector base-char))
        (typep "aaa" '(vector (member #\a #\é))) (typep "aaa" '(vector (member #\a #\b)))
        (typep (make-string 3 :element-type 'base-char) '(vector (member #\a #\b)))
        (typep (make-sequence '(vector (member #\a #\b)) 2) '(vector (member #\a #\b))))
  (t nil t nil t t))

(deftest-emitting-only array-element-type-consistency.declared-vector-of-members
  (let ((s (make-array 3 :element-type '(member #\a #\b) :initial-element #\a)))
    (list (funcall (compile nil '(lambda (x) (declare (type (vector (member #\a #\b)) x)) (find #\a x))) s)
          (funcall (compile nil '(lambda (x) (declare (type (vector (member #\a #\b)) x)) (length x))) s)))
  (#\a 3))
