;;; Install Quicklisp's dist at the version in $QL_DIST, replacing whatever
;;; version is there, and print the dists this home ends up with.
;;; Runs on dotcl (bundled Quicklisp client) and on SBCL under roswell
;;; (Quicklisp already loaded by ros).

#+dotcl (require "quicklisp")
#+dotcl (ql:setup)

(let* ((want (uiop:getenv "QL_DIST"))
       (have (ql-dist:find-dist "quicklisp")))
  (unless (and have (equal (ql-dist:version have) want))
    (ql-dist:install-dist
     (format nil "http://beta.quicklisp.org/dist/quicklisp/~a/distinfo.txt" want)
     :replace t :prompt nil))
  (format t "~&dists:~{ ~a~}~%"
          (mapcar (lambda (d) (format nil "~a=~a" (ql-dist:name d) (ql-dist:version d)))
                  (ql-dist:enabled-dists)))
  (unless (equal (ql-dist:version (ql-dist:find-dist "quicklisp")) want)
    (format t "~&quicklisp dist is not ~a~%" want)
    (uiop:quit 1)))
(uiop:quit 0)
