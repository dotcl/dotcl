;;; GCD / LCM name INTEGER, FLOAT / COMPLEX name REAL, and a non-list first argument of a
;;; two-argument NCONC names LIST (as SBCL does).

(defmacro %ten (form)
  `(handler-case ,form
     (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))
     (error (e) (list :other (type-of e)))))

(deftest type-error-number-funs.gcd-lcm
  (list (%ten (gcd 'a 2)) (%ten (gcd 2 'a)) (%ten (lcm 'a 2)) (%ten (lcm 2 'a)))
  ((a integer) (a integer) (a integer) (a integer)))

(deftest type-error-number-funs.float-complex
  (list (%ten (float 'a)) (%ten (complex 'a 2)) (%ten (complex 2 'a)))
  ((a real) (a real) (a real)))

(deftest type-error-number-funs.nconc
  (list (%ten (nconc 5 '(3))) (%ten (nconc 5 nil)) (nconc nil 7) (nconc (list 1) (list 2)))
  ((5 list) (5 list) 7 (1 2)))
