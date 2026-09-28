;;; The default DESCRIBE-OBJECT says what an object is, not only its type.
;;;
;;; A symbol is described by what it names: a function, macro or special
;;; operator with its lambda list and documentation, a variable with its value,
;;; a class. A function object carries its lambda list and documentation, an
;;; instance its slots. A user's own DESCRIBE-OBJECT method still wins.

(defun dso-describe (object)
  (with-output-to-string (s) (describe object s)))

(defun dso-has (object &rest pieces)
  "T when describing OBJECT prints every one of PIECES, or the text if not."
  (let ((text (dso-describe object)))
    (if (every (lambda (piece) (search piece text)) pieces)
        t
        text)))

(defvar *dso-variable* 17 "Documentation of a describe test variable.")

(defun dso-function (alpha &optional beta &key gamma)
  "Documentation of a describe test function."
  (list alpha beta gamma))

(defgeneric dso-generic (thing)
  (:documentation "Documentation of a describe test generic function."))
(defmethod dso-generic ((thing integer)) thing)

(defclass dso-class ()
  ((left :initarg :left)
   (right :initform 2)
   (unset))
  (:documentation "Documentation of a describe test class."))

(defstruct dso-struct first second)

;;; A symbol: its package, and what it names.

(deftest describe-symbol-shows-package
  (dso-has 'car "COMMON-LISP:CAR" "[symbol]")
  t)

(deftest describe-symbol-builtin-function
  (dso-has 'car "CAR names a compiled function" "Lambda-list: (")
  t)

(deftest describe-symbol-user-function
  (dso-has 'dso-function
           "DSO-FUNCTION names a compiled function"
           "Lambda-list: (ALPHA &OPTIONAL BETA &KEY GAMMA)"
           "Documentation of a describe test function.")
  t)

(deftest describe-symbol-macro
  (dso-has 'when "WHEN names a macro")
  t)

(deftest describe-symbol-special-operator
  (dso-has 'if "IF names a special operator")
  t)

(deftest describe-symbol-special-variable
  (dso-has '*dso-variable*
           "names a special variable"
           "Value: 17"
           "Documentation of a describe test variable.")
  t)

(deftest describe-symbol-constant
  (dso-has :some-keyword ":SOME-KEYWORD" "names a constant variable")
  t)

(deftest describe-symbol-generic-function
  (dso-has 'dso-generic
           "names a generic function"
           "Lambda-list: (THING)"
           "Documentation of a describe test generic function."
           "(INTEGER)")
  t)

(deftest describe-symbol-class
  (dso-has 'dso-class
           "names the standard-class DSO-CLASS"
           "Documentation of a describe test class."
           "Direct slots: LEFT, RIGHT, UNSET")
  t)

;;; A function object: its type, lambda list and documentation.

(deftest describe-function-object-builtin
  (dso-has #'car "[compiled-function]" "Lambda-list: (")
  t)

(deftest describe-function-object-user
  (dso-has #'dso-function
           "[compiled-function]"
           "Lambda-list: (ALPHA &OPTIONAL BETA &KEY GAMMA)"
           "Documentation of a describe test function.")
  t)

;;; Instances: their slots, bound or not.

(deftest describe-standard-object-slots
  (dso-has (make-instance 'dso-class :left 1)
           "[standard-object]" "LEFT" "= 1" "RIGHT" "= 2" "#<unbound slot>")
  t)

(deftest describe-structure-slots
  (dso-has (make-dso-struct :first 'one)
           "[structure-object]" "FIRST" "= ONE" "SECOND" "= NIL")
  t)

;;; Other common types.

(deftest describe-string
  (dso-has "abc" "Length: 3" "Element-type:")
  t)

(deftest describe-proper-list
  (dso-has (list 1 2 3) "[list]" "Length: 3")
  t)

(deftest describe-dotted-list
  (dso-has (list* 1 2 3) "dotted list with 2 elements")
  t)

(deftest describe-circular-list
  (let ((l (list 1 2 3)))
    (setf (cdddr l) l)
    (let ((*print-circle* t))
      (dso-has l "circular list")))
  t)

(deftest describe-hash-table
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "k" h) 1)
    (dso-has h "Test: EQUAL" "Count: 1"))
  t)

(deftest describe-number-kind
  (list (dso-has 42 "[fixnum]")
        (dso-has (expt 2 100) "[bignum]")
        (dso-has 3/4 "[ratio]"))
  (t t t))

(deftest describe-character
  (dso-has #\a "Char-code: 97")
  t)

;;; DESCRIBE returns no values, and a user's method is what runs for its class.

(deftest describe-returns-no-values
  (let ((*standard-output* (make-broadcast-stream)))
    (multiple-value-list (describe 'car)))
  nil)

(defclass dso-own-class () ())
(defmethod describe-object ((object dso-own-class) stream)
  (format stream "my own description"))

(deftest describe-user-method-wins
  (dso-describe (make-instance 'dso-own-class))
  "my own description")
