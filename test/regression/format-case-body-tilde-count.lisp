;;; A prefix parameter on a directive inside ~( ... ~) must not hide the closing ~):
;;; ~2~ is two tildes, and the ~) after it still ends the case conversion.

(deftest format-case-body-tilde-count.tildes
  (list (format nil "~(~2~~)")
        (format nil "~:@(~3~~)")
        (format nil "~@(a~2~b~)"))
  ("~~" "~~~" "A~~b"))

(deftest format-case-body-tilde-count.other-params
  (list (format nil "~:@(~5,'0D~)" 42)
        (format nil "~(~10,,,'*A~)" "AB"))
  ("00042" "ab********"))
