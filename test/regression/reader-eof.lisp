;;; End of file inside a form must be END-OF-FILE, not a raw .NET exception.
;;;
;;; The reader signals mid-object EOF with an END-OF-FILE condition from
;;; MakeEndOfFileError, but the top of the dispatch loop threw a bare
;;; EndOfStreamException instead. That escaped as PROGRAM-ERROR from
;;; READ-FROM-STRING and as STREAM-ERROR from READ, so the REPL/loader idiom
;;; (handler-case (read s) (end-of-file () ...)) did not catch it. The inputs
;;; that reached it are the ones where a reader macro asks for a form that is
;;; not there: "'", "`", ",", and the tail of a dotted list.
;;;
;;; The same throw also made LOAD-time eof-error-p wrong: the raw exception was
;;; caught in READ and turned into the eof value, but CLHS says an end of file
;;; in the middle of an object signals END-OF-FILE whatever eof-error-p says.

(defun %reof-from-string (str)
  "Classify (read-from-string STR): :eof, or (:other <type>)."
  (handler-case (progn (read-from-string str) :no-error)
    (end-of-file (e) (if (stream-error-stream e) :eof (list :eof-no-stream)))
    (error (e) (list :other (type-of e)))))

(defun %reof-from-stream (str)
  (handler-case (with-input-from-string (s str) (read s))
    (end-of-file (e) (if (stream-error-stream e) :eof (list :eof-no-stream)))
    (error (e) (list :other (type-of e)))))

;;; CLHS READ: eof-error-p governs only an end of file encountered before any
;;; object begins. Inside an object it signals regardless.
(defun %reof-eof-value (str)
  (handler-case (with-input-from-string (s str) (read s nil :eof-value))
    (end-of-file () :signaled)
    (error (e) (list :other (type-of e)))))

(defun %reof-cases (fn)
  (mapcar fn '("(1 2" "\"abc" "'" "`" "`(a ,b" "(1 . " "#(1 2" "#|abc")))

(deftest reader-eof.read-from-string-incomplete-forms
  (%reof-cases #'%reof-from-string)
  (:eof :eof :eof :eof :eof :eof :eof :eof))

(deftest reader-eof.read-incomplete-forms
  (%reof-cases #'%reof-from-stream)
  (:eof :eof :eof :eof :eof :eof :eof :eof))

(deftest reader-eof.mid-object-ignores-eof-value
  (%reof-cases #'%reof-eof-value)
  (:signaled :signaled :signaled :signaled :signaled :signaled :signaled :signaled))

;;; Clean end of input is the case eof-error-p does govern: no object had begun,
;;; so the eof value comes back. Trailing whitespace and a trailing comment are
;;; still a clean end of input.
(deftest reader-eof.clean-eof-returns-eof-value
  (mapcar #'%reof-eof-value '("" "   " "; nothing here"))
  (:eof-value :eof-value :eof-value))

(deftest reader-eof.clean-eof-signals-when-asked
  (mapcar #'%reof-from-stream '("" "; nothing here"))
  (:eof :eof))

;;; read-preserving-whitespace shares the code path and must agree.
(defun %reof-preserving (str)
  (handler-case (with-input-from-string (s str) (read-preserving-whitespace s))
    (end-of-file (e) (if (stream-error-stream e) :eof (list :eof-no-stream)))
    (error (e) (list :other (type-of e)))))

(deftest reader-eof.read-preserving-whitespace-incomplete
  (mapcar #'%reof-preserving '("'" "(1 . " "(1 2"))
  (:eof :eof :eof))
