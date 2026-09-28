;;; A string pathname designator is parsed the same way everywhere.
;;;
;;; CLHS defines MERGE-PATHNAMES and the PATHNAME-xxx accessors in terms of
;;; PATHNAME, so "HOST:NAME" has to mean the same thing to all of them. It did
;;; not: PATHNAME recognised a defined logical host, while MERGE-PATHNAMES and
;;; the accessors called the physical parser directly. So
;;; (pathname "HOST:NAME") was a logical pathname but
;;; (merge-pathnames "HOST:NAME") was a file literally named "HOST:NAME" in the
;;; current directory.
;;;
;;; What it cost: cl-fad's temporary files merge the template
;;; "TEMPORARY-FILES:TEMP-%" against the defaults, so they were created in the
;;; working directory rather than the temp directory. On Windows a name with a
;;; colon in it is an NTFS alternate data stream, so the write succeeded,
;;; PROBE-FILE said the file was there, and no directory listing showed it.
;;;
;;; The rule, unchanged by this and the same on every platform: the text before
;;; the first colon is a logical host only when that host is already defined.
;;; A single character before the colon is always a drive letter. Everything
;;; else keeps the colon as an ordinary character in the name, which is what a
;;; Unix file name like "foo:bar" needs and what SBCL does here too.

(defun %lpd-target ()
  "A physical directory to translate into, as a namestring ending in a slash."
  (let ((d (concatenate 'string
                        (regression-temp-dir)
                        "/dotcl-lpd/")))
    (ensure-directories-exist d)
    d))

(setf (logical-pathname-translations "DOTCL-LPD-TEST")
      (list (list "*.*.*" (%lpd-target))))

;;; --- A defined logical host is recognised through every designator path ---

;;; The shape that was wrong: the reported reproducer.
(deftest logical-pathname-designator.merge-is-logical
  (typep (merge-pathnames "DOTCL-LPD-TEST:TEMP-ABC") 'logical-pathname)
  t)

(deftest logical-pathname-designator.merge-name-has-no-colon
  (find #\: (pathname-name (merge-pathnames "DOTCL-LPD-TEST:TEMP-ABC")))
  nil)

(deftest logical-pathname-designator.merge-agrees-with-pathname
  (let ((s "DOTCL-LPD-TEST:TEMP-ABC"))
    (list (equal (pathname-host (merge-pathnames s)) (pathname-host (pathname s)))
          (equal (pathname-name (merge-pathnames s)) (pathname-name (pathname s)))))
  (t t))

;;; Merging against the physical *default-pathname-defaults* must not drag the
;;; current drive and directory into the logical pathname: that is what made
;;; the merged pathname unmatchable by its own translations.
(deftest logical-pathname-designator.merge-keeps-logical-components
  (let ((p (merge-pathnames "DOTCL-LPD-TEST:TEMP-ABC")))
    (list (pathname-device p) (pathname-directory p) (pathname-name p)))
  (:unspecific (:absolute) "TEMP-ABC"))

;;; The accessors are specified in terms of PATHNAME, so they must not see a
;;; file called "DOTCL-LPD-TEST:TEMP-ABC".
(deftest logical-pathname-designator.accessors-on-a-string
  (list (pathname-host "DOTCL-LPD-TEST:TEMP-ABC")
        (pathname-name "DOTCL-LPD-TEST:TEMP-ABC")
        (pathname-type "DOTCL-LPD-TEST:TEMP-ABC.LISP"))
  ("DOTCL-LPD-TEST" "TEMP-ABC" "LISP"))

;;; ...and end to end: the merged pathname translates into the target
;;; directory, keeping its name. A translation whose right-hand side is a bare
;;; directory has no name component; CLHS says a NIL piece in the to-wildcard
;;; takes the source's piece, so the file name survives.
(deftest logical-pathname-designator.translate-lands-in-target
  (let ((p (translate-logical-pathname (merge-pathnames "DOTCL-LPD-TEST:TEMP-ABC"))))
    (list (pathname-name p)
          (equal (pathname-directory p) (pathname-directory (pathname (%lpd-target))))))
  ("temp-abc" t))

;;; The symptom itself: writing through the logical pathname creates a file in
;;; the temp directory, not a stream on a file named after the host.
(deftest logical-pathname-designator.write-lands-in-target
  (let ((expected (concatenate 'string (%lpd-target) "lpd-written.txt")))
    ;; An earlier run leaves the file behind, and then this proves nothing.
    (when (probe-file expected) (delete-file expected))
    (with-open-file (s (merge-pathnames "DOTCL-LPD-TEST:LPD-WRITTEN.TXT")
                       :direction :output :if-exists :supersede)
      (write-string "x" s))
    (and (probe-file expected) t))
  t)

;;; --- A drive letter is still a drive letter ---

(deftest logical-pathname-designator.drive-is-a-device
  (let ((p (pathname "C:/foo/bar")))
    (list (typep p 'logical-pathname) (pathname-device p)
          (pathname-directory p) (pathname-name p)))
  (nil "C" (:absolute "foo") "bar"))

(deftest logical-pathname-designator.drive-relative-is-a-device
  (let ((p (pathname "C:foo")))
    (list (typep p 'logical-pathname) (pathname-device p) (pathname-name p)))
  (nil "C" "foo"))

;;; The same through MERGE-PATHNAMES, which is where the confusion would bite.
(deftest logical-pathname-designator.merged-drive-is-a-device
  (pathname-device (merge-pathnames "C:/foo/bar"))
  "C")

;;; --- An undefined host is not a host ---

;;; No logical host by this name exists, so the colon stays in the name. This
;;; is SBCL's answer too; it is deliberately not an error and deliberately not
;;; a logical pathname.
(deftest logical-pathname-designator.undefined-host-stays-physical
  (let ((p (pathname "DOTCL-LPD-NO-SUCH-HOST:FOO.TXT")))
    (list (typep p 'logical-pathname) (pathname-host p)
          (pathname-name p) (pathname-type p)))
  (nil nil "DOTCL-LPD-NO-SUCH-HOST:FOO" "TXT"))

;;; A relative file name that legitimately contains a colon, as on Unix.
(deftest logical-pathname-designator.colon-in-a-file-name
  (let ((p (pathname "foo:bar")))
    (list (typep p 'logical-pathname) (pathname-name p)))
  (nil "foo:bar"))

;;; --- Logical directory syntax ---

;;; CLHS 19.3.1: absolute unless the namestring carries a leading semicolon.
(deftest logical-pathname-designator.absolute-logical-directory
  (pathname-directory (pathname "DOTCL-LPD-TEST:SUB;FOO.TXT"))
  (:absolute "SUB"))

(deftest logical-pathname-designator.relative-logical-directory
  (pathname-directory (pathname "DOTCL-LPD-TEST:;SUB;FOO.TXT"))
  (:relative "SUB"))

;;; --- Round-tripping ---

(deftest logical-pathname-designator.namestring-round-trip
  (let ((failed '()))
    (dolist (s '("C:/foo/bar" "C:foo" "foo:bar" "DOTCL-LPD-NO-SUCH-HOST:FOO.TXT"
                 "DOTCL-LPD-TEST:FOO.TXT" "DOTCL-LPD-TEST:SUB;FOO.TXT"
                 "DOTCL-LPD-TEST:;SUB;FOO.TXT")
             (nreverse failed))
      (let ((back (namestring (parse-namestring s))))
        (unless (string= s back) (push (list s back) failed)))))
  nil)

;;; A version has no place in a namestring without a type: "NAME.NEWEST" would
;;; read back with the type "NEWEST". MERGE-PATHNAMES leaves :newest behind on
;;; a typeless pathname, so this is the round trip that breaks first.
(deftest logical-pathname-designator.merged-namestring-round-trip
  (namestring (merge-pathnames "DOTCL-LPD-TEST:TEMP-ABC"))
  "DOTCL-LPD-TEST:TEMP-ABC")
