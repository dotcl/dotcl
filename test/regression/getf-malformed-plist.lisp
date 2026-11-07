;;; GETF on a malformed plist reports the plist, not the dangling tail.

(defun %gmp-message (plist)
  (handler-case (progn (getf plist :a) nil)
    (type-error (e) (princ-to-string e))))

(deftest getf-malformed-plist.odd-length
  (and (search "(1)" (%gmp-message (list 1))) t)
  t)

(deftest getf-malformed-plist.dotted
  (and (search "(1 2 . 3)" (%gmp-message (list* 1 2 3))) t)
  t)

;; The datum is the bad tail, which is not a list, so the TYPE-ERROR is true to its type.
(deftest getf-malformed-plist.datum-is-not-a-list
  (handler-case (getf (list* 'a 10 'b) 'c)
    (type-error (e) (not (typep (type-error-datum e) 'list))))
  t)
