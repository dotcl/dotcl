;;; A list printed directly by the PRINT-OBJECT method (or the condition
;;; report) of an object printed on its own is broken at the right margin
;;; like a list printed on its own, from the column it starts at on the
;;; method's stream. Expected values are SBCL 2.6.8's output.

(define-condition pspo-no-common (error)
  ((a :initarg :a :reader pspo-a)
   (b :initarg :b :reader pspo-b))
  (:report (lambda (c s)
             (format s "No common elements between ~S and ~S namespaces."
                     (pspo-a c) (pspo-b c)))))

(defclass pspo-holder ()
  ((items :initarg :items :reader pspo-items)))

(defmethod print-object ((x pspo-holder) s)
  (format s "Holder with ~S inside" (pspo-items x)))

(defun %pspo-lines (&rest lines)
  (format nil "~{~a~^~%~}" lines))

(defmacro %pspo-with-margin ((margin &optional (pretty t)) &body body)
  `(let ((*print-right-margin* ,margin)
         (*print-pretty* ,pretty)
         (*print-readably* nil)
         (*print-length* nil)
         (*print-level* nil)
         (*print-lines* nil)
         (*print-miser-width* nil))
     ,@body))

(defparameter *pspo-a* '("server" "app" "blog" "post"))
(defparameter *pspo-b* '("completely" "different" "namespace"))

;; The report under ~A breaks the same way as the same FORMAT with nil.
(deftest pprint-standard-dispatch.condition-report-same-as-format
  (%pspo-with-margin (80)
    (string= (format nil "No common elements between ~S and ~S namespaces."
                     *pspo-a* *pspo-b*)
             (format nil "~A" (make-condition 'pspo-no-common
                                              :a *pspo-a* :b *pspo-b*))))
  t)

(deftest pprint-standard-dispatch.condition-report
  (%pspo-with-margin (80)
    (format nil "~A" (make-condition 'pspo-no-common :a *pspo-a* :b *pspo-b*)))
  #.(%pspo-lines
     "No common elements between (\"server\" \"app\" \"blog\" \"post\") and (\"completely\""
     "                                                               \"different\""
     "                                                               \"namespace\") namespaces."))

(deftest pprint-standard-dispatch.condition-report-princ-to-string
  (%pspo-with-margin (40)
    (princ-to-string (make-condition 'pspo-no-common :a '(aaaa bbbb cccc dddd eeee)
                                                     :b '(x))))
  #.(%pspo-lines
     "No common elements between (AAAA BBBB"
     "                            CCCC DDDD"
     "                            EEEE) and (X) namespaces."))

;; The column the report starts at in the enclosing FORMAT counts.
(deftest pprint-standard-dispatch.condition-report-start-column
  (%pspo-with-margin (50)
    (format nil "Error: ~A" (make-condition 'pspo-no-common
                                            :a '(aaaa bbbb cccc dddd eeee) :b '(x))))
  #.(%pspo-lines
     "Error: No common elements between (AAAA BBBB CCCC"
     "                                   DDDD EEEE) and (X) namespaces."))

(deftest pprint-standard-dispatch.print-object-method
  (%pspo-with-margin (40)
    (prin1-to-string (make-instance 'pspo-holder
                                    :items '(aaaa bbbb cccc dddd eeee ffff gggg))))
  #.(%pspo-lines
     "Holder with (AAAA BBBB CCCC DDDD EEEE"
     "             FFFF GGGG) inside"))

(deftest pprint-standard-dispatch.print-object-method-start-column
  (%pspo-with-margin (40)
    (format nil "-> ~S" (make-instance 'pspo-holder
                                       :items '(aaaa bbbb cccc dddd eeee ffff gggg))))
  #.(%pspo-lines
     "-> Holder with (AAAA BBBB CCCC DDDD"
     "                EEEE FFFF GGGG) inside"))

(deftest pprint-standard-dispatch.condition-report-not-pretty
  (%pspo-with-margin (40 nil)
    (princ-to-string (make-condition 'pspo-no-common :a '(aaaa bbbb cccc dddd eeee)
                                                     :b '(x))))
  "No common elements between (AAAA BBBB CCCC DDDD EEEE) and (X) namespaces.")
