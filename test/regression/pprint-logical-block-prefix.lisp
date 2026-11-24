;;; The :PREFIX / :PER-LINE-PREFIX expression of PPRINT-LOGICAL-BLOCK is evaluated
;;; once, and a value that is not a string (NIL included) is a TYPE-ERROR.

(deftest pprint-logical-block-prefix.string-prefix
  (with-output-to-string (s)
    (let ((*print-pretty* t))
      (pprint-logical-block (s '("a" "b") :prefix "<")
        (write (pprint-pop) :stream s) (write-char #\Space s)
        (write (pprint-pop) :stream s))))
  "<\"a\" \"b\"")

(deftest pprint-logical-block-prefix.evaluated-once
  (let ((n 0))
    (with-output-to-string (s)
      (let ((*print-pretty* t))
        (pprint-logical-block (s '("a") :prefix (progn (incf n) "<") :suffix ">")
          (write (pprint-pop) :stream s))))
    n)
  1)

(deftest pprint-logical-block-prefix.per-line-prefix-evaluated-once
  (let ((n 0))
    (with-output-to-string (s)
      (let ((*print-pretty* t))
        (pprint-logical-block (s '("a") :per-line-prefix (progn (incf n) "; "))
          (write (pprint-pop) :stream s))))
    n)
  1)

(deftest pprint-logical-block-prefix.nil-is-a-type-error
  (list (handler-case (with-output-to-string (s)
                        (let ((*print-pretty* t) (p nil))
                          (pprint-logical-block (s '("a") :prefix p) (write (pprint-pop) :stream s))))
          (type-error () :type-error))
        (handler-case (with-output-to-string (s)
                        (let ((*print-pretty* t) (p nil))
                          (pprint-logical-block (s '("a") :per-line-prefix p) (write (pprint-pop) :stream s))))
          (type-error () :type-error)))
  (:type-error :type-error))
