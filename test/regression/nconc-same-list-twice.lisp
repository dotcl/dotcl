;;; (NCONC H R R) makes R's last cdr point at R: the result is circular, and the end of the last argument
;;; is not looked for (series builds its scan templates this way). The expected values are SBCL's.

(deftest nconc-same-list-twice.circular
  (let* ((r (list 1 2)) (c (nconc (list 0) r r)))
    (list (car c) (cadr c) (caddr c) (eq (cdddr c) (cdr c)) (eq (cdr (cdr c)) r)))
  (0 1 2 t nil))

(deftest nconc-same-list-twice.nil-head
  (let* ((r (list 1 2)) (c (nconc nil r r)))
    (list (eq c r) (eq (cddr c) c)))
  (t t))

(deftest nconc-same-list-twice.ordinary
  (list (nconc) (nconc nil) (nconc (list 1) (list 2) (list 3)) (nconc nil (list 1) nil (list 2) nil)
        (nconc (list 1) 2) (nconc nil 5)
        (handler-case (nconc (list 1) 2 (list 3)) (type-error () :type-error)))
  (nil nil (1 2 3) (1 2) (1 . 2) 5 :type-error))
