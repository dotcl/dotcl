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

;;; The banner the debugger prints must be the report, not the class name.
;;;
;;; The banner read the condition's message field directly. For a condition
;;; built from a DEFINE-CONDITION class that field holds "#<MY-ERROR>": the
;;; report lives in a PRINT-OBJECT method that only fires under PRINC, and
;;; nothing on that path ran the printer. So an unhandled error in a script
;;; said "MY-ERROR: #<MY-ERROR>" while PRINC-TO-STRING of the same condition
;;; said what had gone wrong. It was not only user-defined conditions:
;;; SIMPLE-PACKAGE-ERROR and the SIMPLE-CONDITION + STYLE-WARNING pair that
;;; libraries define are DEFINE-CONDITION classes too, so a build that failed
;;; inside a library printed its condition class and nothing else.
;;;
;;; %CONDITION-REPORT-STRING is the text the banner prints. Asserting on it
;;; rather than on captured process output keeps the test in-process; what it
;;; has to satisfy is that it agrees with PRINC-TO-STRING.

(define-condition cr-banner-lambda (error)
  ((op :initarg :op :reader cr-banner-op))
  (:report (lambda (c s) (format s "The operation ~a is not implemented."
                                 (cr-banner-op c)))))

(define-condition cr-banner-string (error) ()
  (:report "a fixed report string"))

;;; The shape a library gives a condition that carries its own message: a
;;; SIMPLE-CONDITION mixin over the standard type. Both of the ones named in
;;; the report are built this way.
(define-condition cr-banner-package-error (simple-condition package-error) ())
(define-condition cr-banner-style-warning (simple-condition style-warning) ())

(define-condition cr-banner-silent (error) ((x :initarg :x)))

(define-condition cr-banner-explodes (error) ()
  (:report (lambda (c s) (declare (ignore s)) (error "report exploded for ~a" c))))

(defun %cr-banner (c)
  "The two strings that have to agree: what PRINC prints and what the banner
   prints."
  (list (princ-to-string c) (dotcl::%condition-report-string c)))

(deftest condition-report.banner-report-lambda
  (%cr-banner (make-condition 'cr-banner-lambda :op 'set-timeouts))
  ("The operation SET-TIMEOUTS is not implemented."
   "The operation SET-TIMEOUTS is not implemented."))

(deftest condition-report.banner-report-string
  (%cr-banner (make-condition 'cr-banner-string))
  ("a fixed report string" "a fixed report string"))

(deftest condition-report.banner-simple-error
  (%cr-banner (make-condition 'simple-error
                              :format-control "no component named ~a"
                              :format-arguments '(mgl-pax-bootstrap)))
  ("no component named MGL-PAX-BOOTSTRAP" "no component named MGL-PAX-BOOTSTRAP"))

(deftest condition-report.banner-simple-package-error
  (%cr-banner (make-condition 'cr-banner-package-error
                              :package "CR-SOURCE"
                              :format-control "symbol ~a not found in package ~a"
                              :format-arguments '(absent cr-source)))
  ("symbol ABSENT not found in package CR-SOURCE"
   "symbol ABSENT not found in package CR-SOURCE"))

(deftest condition-report.banner-simple-style-warning
  (%cr-banner (make-condition 'cr-banner-style-warning
                              :format-control "~a is deprecated"
                              :format-arguments '(old-fn)))
  ("OLD-FN is deprecated" "OLD-FN is deprecated"))

;;; A condition with no report at all still has to print something, and must
;;; not signal on the way.
(deftest condition-report.banner-no-report
  (%cr-banner (make-condition 'cr-banner-silent :x 1))
  ("#<CR-BANNER-SILENT>" "#<CR-BANNER-SILENT>"))

;;; A report that signals is how one error becomes an endless one: the report
;;; runs while the debugger is being entered, and reaches the debugger again.
;;; The banner drops back to the unreportable form instead, and returns.
(deftest condition-report.banner-report-that-signals
  (dotcl::%condition-report-string (make-condition 'cr-banner-explodes))
  "#<CR-BANNER-EXPLODES>")

;;; The type stays in the banner next to the report, so a reader who knows the
;;; condition class still sees it.
(deftest condition-report.banner-line-has-type-and-report
  (dotcl::%condition-report-line (make-condition 'cr-banner-lambda :op 'set-timeouts))
  "CR-BANNER-LAMBDA: The operation SET-TIMEOUTS is not implemented.")

;;; End to end through the signalling machinery rather than MAKE-CONDITION:
;;; this is the path that produced "#<SIMPLE-PACKAGE-ERROR>" when a build died
;;; inside a library.
(deftest condition-report.banner-matches-princ-when-signalled
  (handler-case (eval '(in-package #:cr-banner-no-such-package))
    (error (e) (list (dotcl::%condition-type-string e)
                     (dotcl::%condition-report-string e)
                     (string= (dotcl::%condition-report-string e) (princ-to-string e)))))
  ("SIMPLE-PACKAGE-ERROR" "No package named \"CR-BANNER-NO-SUCH-PACKAGE\" exists." t))
