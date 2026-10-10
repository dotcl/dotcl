;;; The logical host SYS is defined from the start, as in SBCL: portable code
;;; probes for it with (logical-pathname "SYS:SRC;"), which was a TYPE-ERROR
;;; ("logical host not yet defined: SYS"). pathname-utils' tests do exactly
;;; that. In dotcl it names the installation: SYS:CONTRIB; is the contrib tree
;;; REQUIRE loads from, and SYS:SRC; the directory above it.

(deftest sys-logical-host.parses
  (let ((p (logical-pathname "SYS:SRC;")))
    (list (typep p 'logical-pathname) (host-namestring p) (pathname-directory p)))
  (t "SYS" (:absolute "SRC")))

(deftest sys-logical-host.has-translations
  (every (lambda (rule) (typep (pathname (second rule)) 'pathname))
         (logical-pathname-translations "SYS"))
  t)

;; SYS:CONTRIB; reaches the same contrib tree REQUIRE uses.
(deftest sys-logical-host.contrib
  (let ((p (translate-logical-pathname "SYS:CONTRIB;ASDF;ASDF.LISP")))
    (list (typep p 'logical-pathname)
          (last (pathname-directory p) 2)
          (pathname-name p)))
  (nil ("contrib" "asdf") "asdf"))
