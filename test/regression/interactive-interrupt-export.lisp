;;; DOTCL:INTERACTIVE-INTERRUPT is the Ctrl-C condition, by a public name a library can inherit from.

(define-condition ii-export-user-abort (dotcl:interactive-interrupt) ())

(deftest interactive-interrupt-export.same-symbol
  (eq 'dotcl:interactive-interrupt (find-symbol "INTERACTIVE-INTERRUPT" "CL-USER"))
  t)

(deftest interactive-interrupt-export.inherit
  (list (handler-case (signal 'ii-export-user-abort)
          (dotcl:interactive-interrupt () :as-parent))
        (handler-case (error 'ii-export-user-abort) (ii-export-user-abort () :as-itself))
        (subtypep 'dotcl:interactive-interrupt 'serious-condition))
  (:as-parent :as-itself t))
