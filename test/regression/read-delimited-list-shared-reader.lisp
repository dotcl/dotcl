;;; A reader macro that calls READ-DELIMITED-LIST reads on the same reader as
;;; the READ it runs inside, and so do the built-in macro functions a copied
;;; readtable holds (what GET-MACRO-CHARACTER / GET-DISPATCH-MACRO-CHARACTER
;;; return, as named-readtables copies them). With a second reader the two kept
;;; separate look-ahead: the closing delimiter went to the wrong one, and the
;;; form after it was read outside the enclosing #+ / #-. A symbol in a missing
;;; package there signalled. GrammaTech's cl-utils (curry-compose-reader-macros
;;; inside #+sbcl) hit this in COMPILE-FILE.

(defun %rdl-readtable ()
  (let ((rt (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\' (get-dispatch-macro-character #\# #\') rt)
    (set-dispatch-macro-character #\# #\( (get-dispatch-macro-character #\# #\() rt)
    (set-macro-character #\( (get-macro-character #\() nil rt)
    (set-macro-character #\[ (lambda (s c)
                               (declare (ignore c))
                               (cons :brackets (read-delimited-list #\] s t)))
                         nil rt)
    (set-macro-character #\] (get-macro-character #\)) nil rt)
    rt))

(defun %rdl-read-string (string)
  (let ((*readtable* (%rdl-readtable)))
    (handler-case (values (read-from-string string))
      (error (e) (list :error (type-of e))))))

(defun %rdl-read-stream (string)
  (let ((*readtable* (%rdl-readtable)))
    (handler-case (with-input-from-string (s string) (list (read s) (read s)))
      (error (e) (list :error (type-of e))))))

(deftest read-delimited-list-shared-reader.suppressed-function
  (%rdl-read-string "(a #+(or) (r [x #'y] (no-such-package-rdl::v)) b)")
  (a b))

(deftest read-delimited-list-shared-reader.suppressed-vector
  (%rdl-read-string "(a #+(or) (r [(x) #(y)] (no-such-package-rdl::v)) b)")
  (a b))

(deftest read-delimited-list-shared-reader.stream
  (%rdl-read-stream "#+(or) (r [(x) #'y] (no-such-package-rdl::v)) c d")
  (c d))

(deftest read-delimited-list-shared-reader.not-suppressed
  (%rdl-read-string "(a [(x) #'y] (z))")
  (a (:brackets (x) (function y)) (z)))

(deftest read-delimited-list-shared-reader.after-delimiter
  (let ((*readtable* (%rdl-readtable)))
    (with-input-from-string (s "[x #'y]z")
      (list (read s) (read-char s))))
  ((:brackets x (function y)) #\z))
