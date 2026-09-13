;;; Conditions signalled from Lisp must report what they hold.
;;;
;;; (error 'type-error :datum 7 :expected-type 'list) -- the shape CIL-STDLIB
;;; itself uses to report a bad argument -- printed as "#<TYPE-ERROR>". The
;;; object carried both the datum and the expected type and said neither, so
;;; (butlast 7) and (endp 7) told the user nothing at all.
;;;
;;; Two layers were involved, and both are pinned here:
;;;
;;; 1. The accessors. A condition reaches them in two shapes: the wrapper the
;;;    signalling machinery builds, and the bare instance -- which is what a
;;;    PRINT-OBJECT method receives. CELL-ERROR-NAME and FILE-ERROR-PATHNAME
;;;    knew only the wrapper, so they answered NIL inside the very method whose
;;;    job is to report the condition (while SLOT-VALUE on the same object
;;;    answered correctly).
;;; 2. The reports themselves, as PRINT-OBJECT methods -- the mechanism
;;;    DEFINE-CONDITION's :report already uses, so a user :report overrides them
;;;    by ordinary method specificity, and an explicit format control still wins.

(defun %cr-report (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (error (e) (princ-to-string e))))

(deftest condition-report.type-error
  (%cr-report (lambda () (error 'type-error :datum 7 :expected-type 'list)))
  "The value 7 is not of type LIST")

(deftest condition-report.type-error-from-stdlib
  (list (%cr-report (lambda () (butlast 7)))
        (%cr-report (lambda () (endp 7))))
  ("The value 7 is not of type LIST" "The value 7 is not of type LIST"))

(deftest condition-report.cell-errors
  (list (%cr-report (lambda () (error 'unbound-variable :name 'zzz)))
        (%cr-report (lambda () (error 'undefined-function :name 'zzz)))
        (%cr-report (lambda () (error 'cell-error :name 'zzz))))
  ("The variable ZZZ is unbound."
   "The function ZZZ is undefined."
   "The cell ZZZ is in error."))

(deftest condition-report.file-and-package-errors
  (list (%cr-report (lambda () (error 'file-error :pathname "/x")))
        (%cr-report (lambda () (error 'package-error :package "P"))))
  ("Error on file /x." "Package error on P."))

(defclass %cr-obj () ((s)))
(deftest condition-report.unbound-slot
  (let ((r (%cr-report (lambda () (slot-value (make-instance '%cr-obj) 's)))))
    (list (and (search "The slot S is unbound" r) t)
          (and (search "%CR-OBJ" r) t)))
  (t t))

;;; The accessor must answer inside a report method, not only outside it. This
;;; is the half that was broken: SLOT-VALUE saw the value, the accessor did not.
(defvar *cr-seen* nil)
(defmethod print-object ((c file-error) stream)
  (if *print-escape*
      (call-next-method)
      (progn (setf *cr-seen* (list (file-error-pathname c)
                                   (ignore-errors (slot-value c 'pathname))))
             (format stream "probed"))))
(deftest condition-report.accessor-inside-report-method
  (progn (setf *cr-seen* nil)
         (%cr-report (lambda () (error 'file-error :pathname "/x")))
         *cr-seen*)
  ("/x" "/x"))
(remove-method #'print-object (find-method #'print-object nil
                                           (list (find-class 'file-error)
                                                 (find-class t))))

;;; An explicit format control still wins over the default report. SIMPLE-ERROR
;;; is the plain case; SIMPLE-TYPE-ERROR is the one that would otherwise collide,
;;; since the default TYPE-ERROR report applies to it by inheritance.
(deftest condition-report.format-control-wins
  (list (%cr-report (lambda () (error 'simple-error :format-control "boom ~a"
                                                    :format-arguments '(1))))
        (%cr-report (lambda () (error 'simple-type-error
                                      :datum 7 :expected-type 'list
                                      :format-control "custom ~a"
                                      :format-arguments '(:x)))))
  ("boom 1" "custom X"))

;;; A user :report on a subclass overrides by method specificity.
(define-condition %cr-custom (type-error) ()
  (:report (lambda (c stream) (declare (ignore c)) (write-string "mine" stream))))
(deftest condition-report.user-report-wins
  (%cr-report (lambda () (error '%cr-custom :datum 1 :expected-type 'fixnum)))
  "mine")

;;; Conditions raised inside the runtime keep their own message: they are not
;;; CLOS instances, so these methods never apply to them.
(deftest condition-report.native-conditions-unchanged
  (let ((r (%cr-report (lambda () (car 7)))))
    (and (search "CAR: not a list" r) t))
  t)

;;; PRIN1 (i.e. *PRINT-ESCAPE* true) still prints the object, not the report.
(deftest condition-report.escaped-printing-unchanged
  (handler-case (error 'type-error :datum 7 :expected-type 'list)
    (error (e) (prin1-to-string e)))
  "#<TYPE-ERROR>")

;;; --- a package error that carries a message --------------------------------
;;;
;;; PACKAGE-ERROR has no format control of its own: the slots belong to
;;; SIMPLE-CONDITION, and signalling a PACKAGE-ERROR with :FORMAT-CONTROL puts
;;; the text where nothing can read it back. Every message DEFPACKAGE writes
;;; about a package went that way and reported as "Package error on X."
;;;
;;; SIMPLE-PACKAGE-ERROR has both supers, so the text is where the inherited
;;; report can find it while a PACKAGE-ERROR handler still catches it. It is an
;;; internal name (SBCL keeps its own in SB-INT for the same reason) -- what user
;;; code handles is PACKAGE-ERROR, which is what the tests below do.

(deftest condition-report.simple-package-error-reports-its-message
  (%cr-report (lambda ()
                (error 'dotcl-internal::simple-package-error
                       :package "P"
                       :format-control "no package named ~A here"
                       :format-arguments (list "P"))))
  "no package named P here")

(deftest condition-report.simple-package-error-is-a-package-error
  (handler-case (error 'dotcl-internal::simple-package-error
                       :package "P" :format-control "x" :format-arguments nil)
    (package-error (e) (list :caught (package-error-package e)))
    (error () :wrong-type))
  (:caught "P"))

;;; The messages DEFPACKAGE and IN-PACKAGE write, through the whole path: still
;;; PACKAGE-ERRORs, and now saying what went wrong rather than naming the
;;; package and stopping.
(defpackage #:cr-source (:use) (:export #:present))

(deftest condition-report.defpackage-messages-survive
  (flet ((msg (form)
           (handler-case (progn (eval form) :no-error)
             (package-error (e) (princ-to-string e))
             (error (e) (list :wrong-type (type-of e))))))
    (list (msg '(defpackage #:cr-x1 (:use #:cl) (:import-from #:cr-source #:absent)))
          (msg '(defpackage #:cr-x2 (:use #:cl) (:import-from #:cr-no-such-package #:x)))
          (msg '(in-package #:cr-no-such-package))))
  ("DEFPACKAGE: symbol ABSENT not found in package CR-SOURCE"
   "DEFPACKAGE: :IMPORT-FROM package CR-NO-SUCH-PACKAGE does not exist"
   "No package named \"CR-NO-SUCH-PACKAGE\" exists."))
