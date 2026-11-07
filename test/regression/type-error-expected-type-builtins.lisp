;;; TYPE-ERROR-EXPECTED-TYPE of an error the runtime signals names the type that was
;;; expected, as SBCL's does, not NIL (the empty type, which makes a handler that
;;; prints or checks it wrong). Types are compared as the standard names them.

(defmacro %teet (form)
  `(handler-case (progn ,form :no-error)
     (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))))

(deftest type-error-expected-type.hash-table
  (%teet (gethash 1 1))
  (1 hash-table))

(deftest type-error-expected-type.list
  (list (second (%teet (values-list 1)))
        (second (%teet (coerce 1 'list)))
        (second (%teet (append 1 '(2)))))
  (list list list))

(deftest type-error-expected-type.sequence
  (list (second (%teet (elt 1 0))) (second (%teet (length 1))) (second (%teet (find 1 1))))
  (sequence sequence sequence))

(deftest type-error-expected-type.function-designator
  (%teet (funcall 1))
  (1 (or function symbol)))

(deftest type-error-expected-type.pathname
  (second (%teet (pathname 1)))
  pathname)

(deftest type-error-expected-type.array
  (list (second (%teet (aref 1 0))) (second (%teet (vector-push 1 1))))
  (array vector))

(deftest type-error-expected-type.unchanged-ones
  (list (second (%teet (car 1))) (second (%teet (+ 1 "a"))) (second (%teet (symbol-name 1))))
  (list number symbol))

(deftest type-error-expected-type.no-error-for-valid-calls
  (list (gethash 1 (make-hash-table)) (values-list '(1)) (elt '(1) 0) (aref #(1) 0))
  (nil 1 1 1))

;;; A dotted list is a LIST: the datum must not be of the expected type, so nothing is
;;; named for it (the empty type, as before the expected type was read from the message).
(deftest type-error-expected-type.dotted-list-datum-is-not-of-the-expected-type
  (let ((r (handler-case (values-list '(a b c . d))
             (type-error (e) (list (type-error-datum e) (type-error-expected-type e))))))
    (list (first r) (not (typep (first r) (second r)))))
  ((a b c . d) t))

(deftest type-error-expected-type.dotted-list-in-sequence-functions
  (loop for form in '((find 'e '(a b c . d)) (position 'e '(a b c . d)) (find-if 'null '(a . b)))
        collect (handler-case (eval form)
                  (type-error (e) (not (typep (type-error-datum e) (type-error-expected-type e))))))
  (t t t))
