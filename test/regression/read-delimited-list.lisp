;;; READ-DELIMITED-LIST takes part in the enclosing read.
;;;
;;; It built a fresh Reader over the stream and never adopted the stream's
;;; #n= / #n# share tables, so a label defined outside the delimited list was
;;; invisible inside it -- the reason RECURSIVE-P looked unimplementable. The
;;; end-of-file case signalled a bare SIMPLE-ERROR where CLHS asks for
;;; END-OF-FILE.
;;;
;;; The delimiter is #\) throughout: a delimiter only ends a token if it is a
;;; terminating macro character, and #\] is a constituent in the standard
;;; readtable (so "3]" would read as one symbol).

(defun %rdl-read (string)
  (with-input-from-string (s string)
    (read-delimited-list #\) s)))

(deftest read-delimited-list.basic
  (%rdl-read "1 2 3)")
  (1 2 3))

(deftest read-delimited-list.empty
  (%rdl-read ")")
  nil)

(deftest read-delimited-list.nested-forms
  (let ((items (%rdl-read "(a b) #(1 2) \"s\" )")))
    (list (first items) (coerce (second items) (quote list)) (third items)))
  ((a b) (1 2) "s"))

;;; A label defined before the delimited list resolves inside it.

(deftest read-delimited-list.shares-labels-with-enclosing-read
  (with-input-from-string (s "#1=(x) #1# )")
    (let ((items (read-delimited-list #\) s)))
      (list (length items) (eq (first items) (second items)))))
  (2 t))

;;; End of file before the delimiter is END-OF-FILE, and it names the stream.

(deftest read-delimited-list.eof-is-end-of-file
  (handler-case (progn (%rdl-read "1 2 3") :no-error)
    (end-of-file (e) (list :end-of-file (and (stream-error-stream e) t)))
    (error (e) (list :other (type-of e))))
  (:end-of-file t))
