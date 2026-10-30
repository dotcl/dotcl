;;; Readtable designators, syntax-driven whitespace, share-label scope, and read-suppress dispatch.

(deftest readtable-get-macro-character-secondary-value
  (let ((rt (copy-readtable nil)))
    (list (multiple-value-list (get-macro-character #\a rt))
          (multiple-value-list (get-macro-character #\Space rt))
          (nth-value 1 (get-macro-character #\# rt))))
  ((nil nil) (nil nil) t))

(deftest readtable-get-macro-character-nil-is-standard
  (let ((rt (copy-readtable nil)))
    (set-macro-character #\! (lambda (s c) (declare (ignore s c)) :bang) nil rt)
    (let ((*readtable* rt))
      (list (multiple-value-list (get-macro-character #\! nil))
            (functionp (get-macro-character #\! rt)))))
  ((nil nil) t))

(deftest readtable-get-dispatch-macro-character-nil-is-standard
  (let ((rt (copy-readtable nil)))
    (make-dispatch-macro-character #\$ nil rt)
    (set-dispatch-macro-character #\$ #\@
                                  (lambda (s c n) (declare (ignore s c n)) :at)
                                  rt)
    (let ((*readtable* rt))
      (list (functionp (get-dispatch-macro-character #\$ #\@ rt))
            (handler-case (get-dispatch-macro-character #\$ #\@ nil)
              (error () :signalled)))))
  (t :signalled))

(deftest readtable-whitespace-syntax-terminates-token
  (let ((rt (copy-readtable nil)))
    (set-syntax-from-char #\! #\Space rt)
    (let ((*readtable* rt))
      (list (multiple-value-list (read-from-string "a!b"))
            (with-input-from-string (s "a!b")
              (list (read s nil :eof) (read-char s nil :eof))))))
  ((a 2) (a #\b)))

(defun %readtable-label-macro (read-function recursive-p)
  (let ((rt (copy-readtable nil)))
    (set-macro-character
     #\! (lambda (s c) (declare (ignore c))
           (list :r (funcall read-function s nil :eof recursive-p)))
     nil rt)
    rt))

(deftest readtable-label-scope-recursive-p-false
  (let ((rt (%readtable-label-macro #'read nil)))
    (let ((*readtable* rt))
      (handler-case
          (progn (read-from-string "(!#1=(a) #1#)") :returned)
        (reader-error () :reader-error))))
  :reader-error)

(deftest readtable-label-scope-recursive-p-true
  (let ((rt (%readtable-label-macro #'read t)))
    (let* ((*readtable* rt)
           (result (read-from-string "(!#1=(a) #1#)")))
      (eq (second result) (second (first result)))))
  t)

(deftest readtable-label-scope-read-preserving-false
  (let ((rt (%readtable-label-macro #'read-preserving-whitespace nil)))
    (let ((*readtable* rt))
      (handler-case
          (progn (read-from-string "(!#1=(a) #1#)") :returned)
        (reader-error () :reader-error))))
  :reader-error)

(deftest readtable-label-scope-read-delimited-list-false
  (let ((rt (copy-readtable nil)))
    (set-macro-character
     #\[ (lambda (s c) (declare (ignore c))
           (cons :brackets (read-delimited-list #\] s nil)))
     nil rt)
    (set-macro-character #\] (get-macro-character #\)) nil rt)
    (let ((*readtable* rt))
      (handler-case
          (progn (read-from-string "(#1=(a) [#1#])") :returned)
        (reader-error () :reader-error))))
  :reader-error)

(deftest readtable-print-readably-invert-uses-standard-case
  (let ((rt (copy-readtable nil)))
    (setf (readtable-case rt) :invert)
    (let ((*readtable* rt) (*print-case* :upcase) (*print-readably* t)
          (*package* (find-package "CL-USER")))
      (list (write-to-string (intern "FOO" "CL-USER"))
            (write-to-string (intern "FOO-BAR" "CL-USER"))
            (let ((*print-readably* nil))
              (write-to-string (intern "FOO" "CL-USER"))))))
  ("FOO" "FOO-BAR" "foo"))

(deftest readtable-share-label-must-label-an-object
  (list (handler-case (progn (read-from-string "#1=#1#") :returned)
          (reader-error () :reader-error))
        (handler-case (progn (read-from-string "#1=#2=#1#") :returned)
          (reader-error () :reader-error)))
  (:reader-error :reader-error))

(deftest readtable-read-suppress-undefined-dispatch-at-eof
  (let ((*read-suppress* t))
    (list (multiple-value-list (read-from-string "#q" nil :eof))
          (multiple-value-list (read-from-string "#q #q" nil :eof))
          (multiple-value-list (read-from-string "#q 1" nil :eof))))
  ((:eof 2) (:eof 5) (nil 4)))
