;;; DIRECTORY reports subdirectories, and reports them as directory pathnames.
;;;
;;; DIRECTORY used to have two implementations. Compiled calls reached one of
;;; them through a name mapping in the assembler; #'DIRECTORY was bound to the
;;; other. The two disagreed: the first omitted subdirectories entirely, the
;;; second returned them as file pathnames — #P".../sub" rather than
;;; #P".../sub/", with NAME "sub" instead of NIL.
;;;
;;; Both halves matter to callers. UIOP:DIRECTORY-FILES enumerates with a wild
;;; pattern and then drops the entries satisfying UIOP:DIRECTORY-PATHNAME-P; a
;;; subdirectory reported as a file pathname survives that filter and is then
;;; read as a file. Lem fails to load on exactly this, walking its frontend
;;; assets. UIOP:SUBDIRECTORIES needs them present at all.
;;;
;;; The values below are what SBCL produces for the same tree.

(defun dirtest-root ()
  "A directory holding one file and one subdirectory, created on first use."
  (let ((root (merge-pathnames "dirtest-tree/" *default-pathname-defaults*)))
    (ensure-directories-exist (merge-pathnames "sub/" root))
    (with-open-file (s (merge-pathnames "a.txt" root)
                       :direction :output :if-exists :supersede)
      (write-string "a" s))
    root))

(defun dirtest-entries ()
  "Entry names directly under the test root. A subdirectory has no NAME and no
TYPE, so it appears here as :DIR."
  (sort (mapcar (lambda (p)
                  (if (and (null (pathname-name p)) (null (pathname-type p)))
                      :dir
                      (file-namestring p)))
                (directory (merge-pathnames "*.*" (dirtest-root))))
        #'string< :key #'string))

;; Listed at all, and as a directory pathname rather than a file named "sub".
(deftest directory-includes-subdirectories-as-directory-pathnames
  (dirtest-entries)
  (:dir "a.txt"))

;; The compiled call and the function object are the same function. They were
;; not, and nothing said so.
(deftest directory-same-through-funcall
  (let ((pat (merge-pathnames "*.*" (dirtest-root))))
    (equal (mapcar #'pathname-name (directory pat))
           (mapcar #'pathname-name (funcall #'directory pat))))
  t)

;; The wildcard is not always in the last segment. Quicklisp finds its dists with
;; "dists/*/distinfo.txt", and only one of the two former implementations could
;; do it — which is how both of them stayed broken: each call path worked for
;; the cases its own callers exercised.
(deftest directory-wild-directory-component
  (let ((root (dirtest-root)))
    (ensure-directories-exist (merge-pathnames "sub/deep/" root))
    (with-open-file (s (merge-pathnames "sub/deep/b.txt" root)
                       :direction :output :if-exists :supersede)
      (write-string "b" s))
    (mapcar #'file-namestring
            (directory (merge-pathnames "*/deep/b.txt" root))))
  ("b.txt"))

;; The same, for a directory-only search with a wild segment above it.
(deftest directory-wild-directory-only-search
  (let ((root (dirtest-root)))
    (ensure-directories-exist (merge-pathnames "sub/deep/" root))
    (mapcar (lambda (p) (car (last (pathname-directory p))))
            (directory (merge-pathnames "*/deep/" root))))
  ("deep"))

;; The subdirectory's own name survives in the directory component, so callers
;; can still recover it — dropping it is the other way this could be got wrong.
(deftest directory-subdirectory-keeps-its-name
  (let* ((entries (directory (merge-pathnames "*.*" (dirtest-root))))
         (dir (find-if (lambda (p) (null (pathname-name p))) entries)))
    (car (last (pathname-directory dir))))
  "sub")
