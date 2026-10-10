;;; An integer range written as (INTEGER LO HI), (MOD N) or (EQL N) is the same
;;; type as the (UNSIGNED-BYTE N) / (SIGNED-BYTE N) of its width, so it has to
;;; upgrade to the same array element type (CLHS 15.1.2.1), as in SBCL.
;;; (integer 0 4294967295) used to upgrade to (SIGNED-BYTE 64), so an
;;; (UNSIGNED-BYTE 32) vector was not of type
;;; (SIMPLE-ARRAY (INTEGER 0 4294967295) (*)), and a variable declared so
;;; rejected one.

(deftest integer-range-element-type.upgraded
  (mapcar #'upgraded-array-element-type
          '((integer 0 4294967295) (mod 4294967296) (integer 0 100) (integer 0 255)
            (integer 0 (256)) (integer -128 127) (integer -32768 32767)
            (integer -2147483648 2147483647) (integer 0 9223372036854775807)
            (eql 3) (integer 0 1) (integer 0 18446744073709551615) (integer 0 *)
            (integer -1 0) (signed-byte 1)))
  ((unsigned-byte 32) (unsigned-byte 32) (unsigned-byte 7) (unsigned-byte 8)
   (unsigned-byte 8) (signed-byte 8) (signed-byte 16)
   (signed-byte 32) (unsigned-byte 63)
   (unsigned-byte 2) bit (unsigned-byte 64) t
   (signed-byte 2) (signed-byte 2)))

;; Upgrading preserves subtype relations (CLHS 15.1.2.1): (eql 0) is in
;; (integer -1 0) and upgrades to BIT, so a signed range upgrades to at least
;; (signed-byte 2), which holds BIT. (ANSI UPGRADED-ARRAY-ELEMENT-TYPE.8.)
(deftest integer-range-element-type.subtype-preserved
  (let* ((types '((eql 0) (eql -1) (integer 0 (1)) (integer -1 (1)) (signed-byte 1)
                  (unsigned-byte 1) bit (integer 0 (256)) (integer -256 (256))
                  (unsigned-byte 32) (integer 0 4294967295) (signed-byte 8)))
         (ups (mapcar #'upgraded-array-element-type types)))
    (loop for t1 in types for u1 in ups
          append (loop for t2 in types for u2 in ups
                       when (and (subtypep t1 t2) (not (subtypep u1 u2)))
                         collect (list t1 t2))))
  nil)

(deftest integer-range-element-type.typep
  (list (typep (make-array 4 :element-type '(unsigned-byte 32))
               '(simple-array (integer 0 4294967295) (*)))
        (typep (make-array 4 :element-type '(mod 4294967296))
               '(simple-array (unsigned-byte 32) (*)))
        (typep (make-array 4 :element-type '(unsigned-byte 7))
               '(simple-array (integer 0 100) (*)))
        (typep (make-array 4 :element-type '(unsigned-byte 8))
               '(simple-array (integer 0 100) (*)))
        (array-element-type (make-array 4 :element-type '(integer -128 127))))
  (t t t nil (signed-byte 8)))

(defun %iret-u32 (x)
  (declare (type (simple-array (integer 0 4294967295) (*)) x)
           (optimize (safety 1)))
  (setf (aref x 0) (logxor (aref x 0) 7))
  x)

(defun %iret-u7 (x)
  (declare (type (simple-array (integer 0 100) (*)) x)
           (optimize (safety 1)))
  (setf (aref x 0) (1+ (aref x 0)))
  x)

(defun %iret-try (thunk)
  (handler-case (funcall thunk) (type-error () :type-error)))

;; The declaration accepts the vectors of the same element type and stores
;; only what the element type admits.
(deftest integer-range-element-type.declared
  (list (aref (%iret-u32 (make-array 2 :element-type '(unsigned-byte 32)
                                       :initial-element #xfffffff0))
              0)
        (aref (%iret-u7 (make-array 2 :element-type '(unsigned-byte 7)
                                      :initial-element 5))
              0)
        (%iret-try (lambda ()
                     (%iret-u7 (make-array 2 :element-type '(integer 0 100)
                                             :initial-element 127)))))
  (#xfffffff7 6 :type-error))

;; A false declaration is a TYPE-ERROR above (safety 0); the interpreter may
;; ignore declarations.
(deftest-emitting-only integer-range-element-type.false-declaration
  (list (%iret-try (lambda () (%iret-u32 (make-array 2 :element-type 'fixnum))))
        (%iret-try (lambda () (%iret-u7 (make-array 2 :element-type '(unsigned-byte 8))))))
  (:type-error :type-error))
