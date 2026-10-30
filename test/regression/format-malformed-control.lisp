;;; Malformed FORMAT control strings must signal instead of printing.
;;;
;;; CLHS 22.3 defines a directive as a tilde plus a directive character, limits
;;; the parameters of each directive, and restricts ~:^ to ~:{ or ~:@{.  The
;;; expected values are SBCL 2.6.8's.

(defmacro %fmc-try (form)
  `(handler-case (progn ,form :no-error) (error () :error)))

(deftest format-malformed-control.unterminated-tilde
  (list (%fmc-try (format nil "~~~"))
        (%fmc-try (format nil "~"))
        (%fmc-try (format nil "~2,")))
  (:error :error :error))

(deftest format-malformed-control.colon-up-and-out-needs-its-iteration
  (list (%fmc-try (format nil "~a~:^b" 1))
        (%fmc-try (format nil "~@^"))
        (format nil "~:{~a~:^,~}" '((1) (2)))
        (format nil "~:@{~a~:^,~}" '(1) '(2)))
  (:error :error "1,2" "1,2"))

(deftest format-malformed-control.recursive-control-must-be-a-string
  (list (%fmc-try (format nil "~?" 42 '(1)))
        (format nil "~?" "~a" '(1)))
  (:error "1"))

(deftest format-malformed-control.parameter-types
  (list (%fmc-try (format nil "~10,'-A" "abc"))
        (%fmc-try (format nil "~v,'xA" 10 42))
        (%fmc-try (format nil "~10,1,0,1A" "abc"))
        (%fmc-try (format nil "~,,1,'xd" 42))
        (format nil "~10,1,0,'-A" "abc")
        (format nil "~,,,1d" 42))
  (:error :error :error :error "abc-------" "42"))

(deftest format-malformed-control.excess-parameters
  (list (%fmc-try (format nil "~10,,,'.,3:D" 255))
        (%fmc-try (format nil "~2P" 1))
        (%fmc-try (format nil "~2,3,4T")))
  (:error :error :error))
