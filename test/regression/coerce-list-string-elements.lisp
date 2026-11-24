;;; COERCE of a list to a string accepts only a proper list of characters.

(defun %coerce-list-string-type-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (type-error () t)))

(deftest coerce-list-string-elements.non-character
  (list (%coerce-list-string-type-error-p (lambda () (coerce (list 1 2) 'string)))
        (%coerce-list-string-type-error-p (lambda () (coerce (list #\a nil) 'string)))
        (%coerce-list-string-type-error-p (lambda () (coerce (list* #\a 1) 'string))))
  (t t t))

(deftest coerce-list-string-elements.dotted
  (list (%coerce-list-string-type-error-p (lambda () (coerce (cons #\a #\b) 'string)))
        (%coerce-list-string-type-error-p (lambda () (coerce (list* #\a #\b 42) 'simple-string)))
        (%coerce-list-string-type-error-p (lambda () (coerce (cons #\a #\b) '(vector character)))))
  (t t t))

(deftest coerce-list-string-elements.valid
  (list (coerce nil 'string)
        (coerce (list #\a #\b) 'string)
        (coerce (vector #\a #\b) 'string)
        (coerce (list #\a #\b) 'simple-string)
        (coerce (list #\a #\b) 'base-string)
        (coerce (list #\a #\b) '(vector character)))
  ("" "ab" "ab" "ab" "ab" "ab"))
