;;; A runtime entry that works out an element type with SUBTYPEP must not leave SUBTYPEP's second
;;; value behind: (stringp (make-string 3 :element-type 'base-char)) returned two values (T T).

(defun %etsv-count (thunk) (length (multiple-value-list (funcall thunk))))

(deftest element-type-stale-values.make-string
  (list (%etsv-count (lambda () (stringp (make-string 3 :element-type 'base-char))))
        (%etsv-count (lambda () (stringp (make-string 3 :element-type 'character))))
        (%etsv-count (lambda () (typep (make-string 3 :element-type 'base-char) 'simple-base-string)))
        (%etsv-count (lambda () (atom (make-string 3 :element-type 'character)))))
  (1 1 1 1))

(deftest element-type-stale-values.make-array
  (list (%etsv-count (lambda () (typep (array-element-type (make-array 2 :element-type '(member 1 2))) t)))
        (%etsv-count (lambda () (typep (make-array 2 :element-type '(integer 0 100)) 'vector)))
        (%etsv-count (lambda () (typep (upgraded-array-element-type '(unsigned-byte 8)) t))))
  (1 1 1))

;; The other runtime entries that ask SUBTYPEP for a yes / no: OPEN's element type, DEFINE-CONDITION,
;; DEFCLASS slot types, TYPEP of a complex type.
(deftest element-type-stale-values.other-entries
  (list (%etsv-count (lambda () (typep (stream-element-type (make-string-output-stream :element-type 'character)) t)))
        (%etsv-count (lambda () (typep (typep #c(1 2) '(complex integer)) t)))
        (%etsv-count (lambda () (typep (make-condition 'simple-error) 'condition)))
        (%etsv-count (lambda () (typep (subtypep 'integer 'number) t)))
        (%etsv-count (lambda () (typep (make-array 2 :element-type '(complex double-float)) t))))
  (1 1 1 1 1))
