;;; The documentation string of DEFINE-MODIFY-MACRO is the macro's documentation.

(define-modify-macro %dmmd-add (&optional (d 1)) + "Add D to the place.")
(define-modify-macro %dmmd-plain (&optional (d 1)) +)
(define-modify-macro %dmmd-rest (&rest more) + "Add all of MORE.")

(deftest dmm-docstring.documentation
  (list (documentation '%dmmd-add 'function) (documentation '%dmmd-plain 'function)
        (documentation '%dmmd-rest 'function))
  ("Add D to the place." nil "Add all of MORE."))

(deftest dmm-docstring.macros-still-work
  (let ((a 5) (b 5) (c 5))
    (%dmmd-add a 3) (%dmmd-plain b) (%dmmd-rest c 1 2 3)
    (list a b c))
  (8 6 11))
