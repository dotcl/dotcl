;;; A condition made by MAKE-INSTANCE reports from its format control.
;;;
;;; MAKE-CONDITION wraps the instance and keeps the format control beside it;
;;; MAKE-INSTANCE returns the bare instance, whose format control lives only in
;;; the slot. PRINC of it printed #<SIMPLE-ERROR>, also after ERROR had wrapped
;;; it for signaling. The readers already found the slot. The expected strings
;;; are SBCL 2.6.8's.

(define-condition micr-sub (simple-error) ())

(deftest micr-simple-error
  (princ-to-string (make-instance 'simple-error :format-control "foo"))
  "foo")

(deftest micr-format-arguments
  (princ-to-string (make-instance 'simple-error :format-control "x=~a y=~s"
                                                :format-arguments '(1 "q")))
  "x=1 y=\"q\"")

(deftest micr-simple-warning
  (princ-to-string (make-instance 'simple-warning :format-control "w~a"
                                                  :format-arguments '(2)))
  "w2")

(deftest micr-simple-condition-tilde-a
  (format nil "[~a]" (make-instance 'simple-condition :format-control "c"))
  "[c]")

(deftest micr-subclass
  (princ-to-string (make-instance 'micr-sub :format-control "m~a"
                                            :format-arguments '(3)))
  "m3")

(deftest micr-signaled
  (handler-case (error (make-instance 'simple-error :format-control "sig~a"
                                                    :format-arguments '(4)))
    (error (e) (princ-to-string e)))
  "sig4")

(deftest micr-make-condition-unchanged
  (princ-to-string (make-condition 'simple-error :format-control "foo"))
  "foo")

;;; With *print-readably* the report still wins when escape is off, and with
;;; escape on the default method signals PRINT-NOT-READABLE, as in SBCL.
(deftest micr-readably-escape-off
  (write-to-string (make-instance 'simple-error :format-control "foo")
                   :readably t :escape nil)
  "foo")

(deftest micr-readably-escape-on
  (handler-case
      (let ((*print-readably* t))
        (prin1-to-string (make-instance 'simple-error :format-control "foo")))
    (print-not-readable () :signaled))
  :signaled)
