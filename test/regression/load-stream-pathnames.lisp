;;; LOAD of a stream binds *load-pathname* / *load-truename* to the stream's file, or to NIL
;;; for a stream that is not a file's, instead of leaving the enclosing load's.

(defvar *lsp-result* nil)

(deftest-emitting-only load-stream-pathnames.file-stream
  (let ((path (regression-temp-file "lsp-file.lisp")))
    (with-open-file (o path :direction :output :if-exists :supersede)
      (write-string "(setq cl-user::*lsp-result* (list *load-pathname* *load-truename*))" o))
    (let ((*load-pathname* #p"outer.lisp") (*load-truename* #p"/outer/outer.lisp"))
      (with-open-file (s path) (load s))
      (list (equal (file-namestring (first *lsp-result*)) (file-namestring path))
            (equal (file-namestring (second *lsp-result*)) (file-namestring path))
            (pathname-name *load-pathname*))))
  (t t "outer"))

(deftest-emitting-only load-stream-pathnames.string-stream
  (let ((*load-pathname* #p"outer.lisp") (*load-truename* #p"/outer/outer.lisp"))
    (with-input-from-string (s "(setq cl-user::*lsp-result* (list *load-pathname* *load-truename*))")
      (load s))
    (list *lsp-result* (pathname-name *load-pathname*)))
  ((nil nil) "outer"))
