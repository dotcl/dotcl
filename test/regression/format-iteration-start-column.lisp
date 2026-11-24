;;; An object printed inside the body of ~{ ~} (or ~@{ ~}) by ~A / ~S starts at the column the
;;; text of the FORMAT call has reached, so the pretty printer lays it out as it does for a ~A
;;; outside an iteration. The expected values are SBCL's.

(defun %fisc (control &rest args)
  (let ((*print-pretty* t) (*print-right-margin* 30))
    (apply #'format nil control args)))

(deftest format-iteration-start-column.plain-a-is-the-reference
  (%fisc "~A~A" (make-string 25 :initial-element #\x) '(alpha beta gamma))
  "xxxxxxxxxxxxxxxxxxxxxxxxx(ALPHA
                          BETA
                          GAMMA)")

(deftest format-iteration-start-column.iteration-agrees
  (list (equal (%fisc "~A~{~A~}" (make-string 25 :initial-element #\x) '((alpha beta gamma)))
               (%fisc "~A~A" (make-string 25 :initial-element #\x) '(alpha beta gamma)))
        (equal (%fisc "~A~@{~A~^,~}" (make-string 25 :initial-element #\x) '(alpha beta gamma))
               (%fisc "~A~A" (make-string 25 :initial-element #\x) '(alpha beta gamma)))
        (equal (%fisc "~A~{~S~}" (make-string 25 :initial-element #\x) '((alpha beta gamma)))
               (%fisc "~A~S" (make-string 25 :initial-element #\x) '(alpha beta gamma))))
  (t t t))

(deftest format-iteration-start-column.later-pass-starts-after-earlier-text
  (let ((x (make-string 5 :initial-element #\x)))
    (%fisc "~A~{~A~}" x '((a b) (alpha beta gamma delta epsilon zeta))))
  "xxxxx(A B)(ALPHA BETA GAMMA
           DELTA EPSILON ZETA)")

(deftest format-iteration-start-column.no-pretty-and-short-unchanged
  (list (let ((*print-pretty* nil)) (format nil "~A~{~A~}" (make-string 25 :initial-element #\x) '((alpha beta gamma))))
        (%fisc "~{~A~^,~}" '(a b c)))
  ("xxxxxxxxxxxxxxxxxxxxxxxxx(ALPHA BETA GAMMA)" "A,B,C"))
