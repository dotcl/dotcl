;;; NIL and T are escaped like any other symbol whose name has uppercase letters when
;;; READTABLE-CASE is :DOWNCASE and escaping is on (|NIL|, |T|); the other cases print as before.

(defun %pntrc (case thunk)
  (let ((rt (copy-readtable nil)))
    (setf (readtable-case rt) case)
    (let ((*readtable* rt)) (funcall thunk))))

(deftest print-nil-t-readtable-case.downcase-escapes
  (%pntrc :downcase (lambda () (list (prin1-to-string nil) (prin1-to-string t)
                                     (prin1-to-string '(nil t)) (prin1-to-string 'abc))))
  ("|NIL|" "|T|" "(|NIL| |T|)" "|ABC|"))

(deftest print-nil-t-readtable-case.downcase-princ-and-no-escape
  (%pntrc :downcase (lambda () (list (princ-to-string nil) (princ-to-string t)
                                     (write-to-string nil :escape nil))))
  ("NIL" "T" "NIL"))

(deftest print-nil-t-readtable-case.other-cases-unchanged
  (list (%pntrc :upcase (lambda () (list (prin1-to-string nil) (prin1-to-string t))))
        (%pntrc :preserve (lambda () (list (prin1-to-string nil) (prin1-to-string t))))
        (%pntrc :invert (lambda () (list (prin1-to-string nil) (prin1-to-string t)))))
  (("NIL" "T") ("NIL" "T") ("nil" "t")))

(deftest print-nil-t-readtable-case.reads-back
  (%pntrc :downcase (lambda () (list (eq nil (read-from-string (prin1-to-string nil)))
                                     (eq 'abc (read-from-string (prin1-to-string 'abc))))))
  (t t))
