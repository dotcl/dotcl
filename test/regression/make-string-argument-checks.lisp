;;; MAKE-STRING checks its arguments (SBCL: TYPE-ERROR): the size is a non-negative
;;; integer, :INITIAL-ELEMENT a character, :ELEMENT-TYPE a subtype of CHARACTER. A
;;; fraction, a non-character initial element and :element-type 'bit used to give a
;;; string of the wrong length or of NULs. Valid calls are unchanged.

(defun %msac (thunk) (handler-case (funcall thunk) (type-error () :type-error)))

(deftest make-string-argument-checks.errors
  (list (%msac (lambda () (make-string 3 :initial-element 65)))
        (%msac (lambda () (make-string 1.5)))
        (%msac (lambda () (make-string -1)))
        (%msac (lambda () (make-string "3")))
        (%msac (lambda () (make-string 3 :element-type 'bit)))
        (%msac (lambda () (make-string 3 :element-type t)))
        (%msac (lambda () (make-string 3 :initial-element "a"))))
  (:type-error :type-error :type-error :type-error :type-error :type-error :type-error))

(deftest make-string-argument-checks.valid-calls
  (list (make-string 3 :initial-element #\a)
        (length (make-string 4))
        (make-string 0)
        (make-string 2 :element-type 'character :initial-element #\z)
        (make-string 2 :element-type 'base-char :initial-element #\y)
        (length (make-string 2 :element-type 'standard-char)))
  ("aaa" 4 "" "zz" "yy" 2))
