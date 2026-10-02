;;; A reader-macro function that gives a character back with UNREAD-CHAR must
;;; have that character read by the rest of the enclosing READ.
;;;
;;; The character used to stay in the stream's one-char buffer, which the
;;; reader driving the enclosing form did not look at: inside a list the list
;;; reader then missed the ')' the macro had put back and ran to end of input
;;; ("Unexpected end of input in list"). At top level it happened to work,
;;; because the next READ picked the buffered character up. cl-unicode's
;;; #\ reader (read the name, stop at the first non-alphanumeric, unread it)
;;; hit this on every character literal in a list.

(defun %rmu-read-all (text)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\% '%rmu-token)
    (set-macro-character #\! '%rmu-unread-then-read)
    (set-macro-character #\@ '%rmu-peek)
    (set-macro-character #\^ '%rmu-read-then-unread)
    (set-dispatch-macro-character #\# #\! '%rmu-dispatch-token)
    (with-input-from-string (s text)
      (loop for x = (read s nil :eof) collect x until (eq x :eof)))))

;; Read one char, then alphanumerics; unread the char that ended the name.
(defun %rmu-name (stream)
  (with-output-to-string (out)
    (write-char (read-char stream t nil t) out)
    (loop for c = (read-char stream t nil t)
          while (alphanumericp c)
          do (write-char c out)
          finally (unread-char c stream))))

(defun %rmu-token (stream char)
  (declare (ignore char))
  (list :tok (%rmu-name stream)))

(defun %rmu-dispatch-token (stream char arg)
  (declare (ignore char arg))
  (list :dtok (%rmu-name stream)))

;; Unread before a nested READ on the same stream.
(defun %rmu-unread-then-read (stream char)
  (declare (ignore char))
  (let ((c (read-char stream t nil t)))
    (unread-char c stream)
    (list :nested (read stream t nil t))))

(defun %rmu-peek (stream char)
  (declare (ignore char))
  (let ((p (peek-char nil stream t nil t)))
    (list :peek p (read-char stream t nil t))))

;; A nested READ, then READ-CHAR of the delimiter and UNREAD-CHAR of it.
(defun %rmu-read-then-unread (stream char)
  (declare (ignore char))
  (let* ((f (read-preserving-whitespace stream t nil t))
         (c (read-char stream t nil t)))
    (unread-char c stream)
    (list :after f c)))

(deftest reader-macro-unread-char.in-list
  (%rmu-read-all "(a %xy) (b)")
  ((a (:tok "xy")) (b) :eof))

(deftest reader-macro-unread-char.dispatch-in-list
  (%rmu-read-all "(list #!w (list #!W)) (list #!a #!b)")
  ((list (:dtok "w") (list (:dtok "W"))) (list (:dtok "a") (:dtok "b")) :eof))

(deftest reader-macro-unread-char.in-vector
  (let ((forms (%rmu-read-all "#(%ab %cd) z")))
    (list (coerce (first forms) 'list) (rest forms)))
  (((:tok "ab") (:tok "cd")) (z :eof)))

(deftest reader-macro-unread-char.top-level
  (%rmu-read-all "%ab c")
  ((:tok "ab") c :eof))

(deftest reader-macro-unread-char.nested-read
  (%rmu-read-all "(!abc d) (!(a b) e) (!%xy)")
  (((:nested abc) d) ((:nested (a b)) e) ((:nested (:tok "xy"))) :eof))

(deftest reader-macro-unread-char.peek-char
  (%rmu-read-all "(@ab c) d")
  (((:peek #\a #\a) b c) d :eof))

(deftest reader-macro-unread-char.unread-delimiter-after-nested-read
  (%rmu-read-all "(^abc) (^(a) d)")
  (((:after abc #\))) ((:after (a) #\Space) d) :eof))

;; A macro that calls READ and then READ-CHAR: the nested READ ended in a
;; macro that gave its delimiter back with UNREAD-CHAR, and the READ-CHAR after
;; it must see that delimiter. It used to stay in the reader's own lookahead and
;; READ-CHAR read past it (read-as-string, which writes its list reader this
;; way, lost every ')' that followed a quoted token).
(defun %rmu-read-then-char (stream char)
  (declare (ignore char))
  (let* ((f (read stream t nil t))
         (c (read-char stream t nil t)))
    (list :then f c)))

(deftest reader-macro-unread-char.read-char-after-nested-read
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\% '%rmu-token)
    (set-macro-character #\& '%rmu-read-then-char)
    (list (with-input-from-string (s "(&%ab. z)")
            (list (read s nil :eof) (read s nil :eof)))
          (with-input-from-string (s "&%cd)")
            (list (read s nil :eof) (read-char s nil :eof)))))
  ((((:then (:tok "ab") #\.) z) :eof)
   ((:then (:tok "cd") #\)) :eof)))

;; The same through a file stream.
(deftest reader-macro-unread-char.file-stream
  (let ((p (format nil "rmu-test-~a.txt" (get-internal-real-time))))
    (with-open-file (out p :direction :output :if-exists :supersede)
      (write-string "(x #!ab (#!c)) y" out))
    (unwind-protect
         (let ((*readtable* (copy-readtable)))
           (set-dispatch-macro-character #\# #\! '%rmu-dispatch-token)
           (with-open-file (in p)
             (list (read in nil :eof) (read in nil :eof) (read in nil :eof))))
      (delete-file p)))
  ((x (:dtok "ab") ((:dtok "c"))) y :eof))

;; READ-FROM-STRING's second value still counts the unread character as unread.
(deftest reader-macro-unread-char.read-from-string-position
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\% '%rmu-token)
    (multiple-value-list (read-from-string "(%ab) z")))
  (((:tok "ab")) 6))
