;;; LISTEN / CLEAR-INPUT take INPUT-STREAM as an &OPTIONAL argument.
;;;
;;; Both were registered with Startup.RegisterUnary, which turns args.Length != 1
;;; into a PROGRAM-ERROR unconditionally, so a zero-argument call could not get
;;; through. The compiled path supplies *STANDARD-INPUT* at the CALL SITE, so
;;; nothing showed there; it was only reachable when EVAL got into the function
;;; body with no arguments (ansi-test LISTEN.3 / LISTEN.4 / CLEAR-INPUT.2).
;;;
;;; Every case asserts both modes as a pair.

(defun %os (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (princ-to-string e))))))

;;; --- LISTEN with no argument reads *STANDARD-INPUT*

(defparameter %os-listen-empty
  '(with-input-from-string (*standard-input* "") (listen)))

(deftest interp-optional-stream.listen-empty-compile
  (%os :compile %os-listen-empty)
  nil)

(deftest interp-optional-stream.listen-empty-interpret
  (%os :interpret %os-listen-empty)
  nil)

;;; it must answer T when input is available: a fix that always returns NIL
;;; would otherwise pass
(defparameter %os-listen-ready
  '(with-input-from-string (*standard-input* "A") (notnot (listen))))

(deftest interp-optional-stream.listen-ready-compile
  (%os :compile %os-listen-ready)
  t)

(deftest interp-optional-stream.listen-ready-interpret
  (%os :interpret %os-listen-ready)
  t)

;;; --- CLEAR-INPUT with no argument

(deftest interp-optional-stream.clear-input-compile
  (%os :compile '(clear-input))
  nil)

(deftest interp-optional-stream.clear-input-interpret
  (%os :interpret '(clear-input))
  nil)

;;; --- explicit arguments (a stream, or NIL meaning *STANDARD-INPUT*) unchanged

(deftest interp-optional-stream.listen-explicit-interpret
  (%os :interpret '(with-input-from-string (s "A") (notnot (listen s))))
  t)

(deftest interp-optional-stream.listen-nil-interpret
  (%os :interpret '(with-input-from-string (*standard-input* "A") (notnot (listen nil))))
  t)

(deftest interp-optional-stream.clear-input-nil-interpret
  (%os :interpret '(clear-input nil))
  nil)

;;; --- the upper bound survives: this must not become "stop checking arity"

(defun %os-too-many (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (progn (eval form) :no-error)
      (program-error () :program-error)
      (error (e) (list :other (type-of e))))))

(deftest interp-optional-stream.listen-too-many-interpret
  (%os-too-many :interpret '(with-input-from-string (s "A") (listen s nil)))
  :program-error)

(deftest interp-optional-stream.clear-input-too-many-interpret
  (%os-too-many :interpret '(clear-input nil nil))
  :program-error)
