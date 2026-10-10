;;; The input functions signal TYPE-ERROR when the stream argument is not an
;;; input stream designator (a stream, T or NIL).
;;;
;;; A string, number or other object used to fall through to standard input,
;;; so (read-char "abc" nil :eof) quietly returned :EOF when stdin was empty.
;;; SBCL 2.6.8 signals TYPE-ERROR for each of these.

(defun %isd (fn)
  (handler-case (progn (funcall fn) :no-error)
    (type-error (e) (list :type-error (type-error-datum e)))))

(deftest input-stream-designator.read-char
  (%isd (lambda () (read-char "abc" nil :eof)))
  (:type-error "abc"))

(deftest input-stream-designator.peek-char
  (%isd (lambda () (peek-char nil "abc" nil :eof)))
  (:type-error "abc"))

(deftest input-stream-designator.unread-char
  (%isd (lambda () (unread-char #\a "abc")))
  (:type-error "abc"))

(deftest input-stream-designator.read
  (%isd (lambda () (read 42 nil :eof)))
  (:type-error 42))

(deftest input-stream-designator.read-line
  (%isd (lambda () (read-line "abc" nil :eof)))
  (:type-error "abc"))

(deftest input-stream-designator.read-preserving-whitespace
  (%isd (lambda () (read-preserving-whitespace "abc" nil :eof)))
  (:type-error "abc"))

;; Streams, T and NIL are still designators.
(deftest input-stream-designator.streams-still-work
  (list (read-char (make-string-input-stream "x"))
        (with-input-from-string (*standard-input* "y") (read-char nil))
        (with-input-from-string (s "z")
          (let ((*terminal-io* (make-two-way-stream s *standard-output*)))
            (read-char t))))
  (#\x #\y #\z))
