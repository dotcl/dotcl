;;; The version MERGE-PATHNAMES leaves behind, and the one a caller asks for.
;;;
;;; CLHS gives MERGE-PATHNAMES the keyword :NEWEST as the default for its third
;;; argument. A symbol merely named NEWEST in some other package is not that
;;; keyword, and the difference is invisible until something compares with EQ.
;;;
;;; The third argument was also being ignored. The two-argument entry point
;;; filled the version slot before the caller's default could be considered, so
;;; the "version is still nil after the merge" test that guarded the default was
;;; already false by the time it ran.

(defun mpv-version (&rest args)
  "PATHNAME-VERSION of a MERGE-PATHNAMES call, whatever its arity."
  (pathname-version (apply #'merge-pathnames args)))

;;; --- the default is the keyword ---

(deftest merge-pathnames-version.default-is-the-keyword
  (eq (mpv-version "mpv-a.txt") :newest)
  t)

;;; Spelled out, so a failure says which symbol arrived rather than just NIL.
(deftest merge-pathnames-version.default-prints-as-the-keyword
  (prin1-to-string (mpv-version "mpv-a.txt"))
  ":NEWEST")

(deftest merge-pathnames-version.default-is-a-keyword-not-a-plain-symbol
  (keywordp (mpv-version "mpv-a.txt"))
  t)

;;; Two arguments reach the same place as one.
(deftest merge-pathnames-version.two-argument-default-is-the-keyword
  (eq (mpv-version "mpv-a.txt" *default-pathname-defaults*) :newest)
  t)

;;; --- the third argument is honoured ---

(deftest merge-pathnames-version.explicit-default-version-wins
  (eq (mpv-version "mpv-a.txt" *default-pathname-defaults* :wild) :wild)
  t)

(deftest merge-pathnames-version.explicit-default-version-can-be-a-number
  (mpv-version "mpv-a.txt" *default-pathname-defaults* 3)
  3)

;;; NIL asked for explicitly is not the same as no argument: it means leave the
;;; version alone rather than supply :NEWEST.
(deftest merge-pathnames-version.explicit-nil-default-version
  (mpv-version "mpv-a.txt" *default-pathname-defaults* nil)
  nil)

;;; --- a version already present is not overridden ---
;;;
;;; The default only applies when the merge left the slot empty, so a pathname
;;; that carries a version keeps it whatever the caller asks for.

(deftest merge-pathnames-version.present-version-survives-the-default
  (let ((p (make-pathname :name "mpv-b" :type "txt" :version 7)))
    (pathname-version (merge-pathnames p *default-pathname-defaults* :wild)))
  7)

(deftest merge-pathnames-version.present-version-survives-the-implicit-default
  (let ((p (make-pathname :name "mpv-b" :type "txt" :version 7)))
    (pathname-version (merge-pathnames p)))
  7)

;;; --- the same holds for a logical pathname ---

(setf (logical-pathname-translations "MPV-TEST")
      (list (list "*.*.*" "/tmp/mpv/")))

(deftest merge-pathnames-version.logical-default-is-the-keyword
  (eq (pathname-version (merge-pathnames "MPV-TEST:MPV-C")) :newest)
  t)

(deftest merge-pathnames-version.logical-explicit-default-version-wins
  (eq (pathname-version
       (merge-pathnames "MPV-TEST:MPV-C" *default-pathname-defaults* :wild))
      :wild)
  t)

;;; MAKE-PATHNAME with neither :VERSION nor :DEFAULTS leaves the version NIL.
;;; The default :DEFAULTS has every component but the host NIL (CLHS
;;; MAKE-PATHNAME), so (make-pathname) and #P"" agree. It used to come back as
;;; :NEWEST, and a component-wise comparison (cl-fad's PATHNAME-EQUAL, used by
;;; its own test suite) then found (make-pathname) and #P"" different.

(deftest make-pathname.version-defaults-to-nil
  (list (pathname-version (make-pathname))
        (pathname-version (make-pathname :name "a"))
        (pathname-version #P""))
  (nil nil nil))

(deftest make-pathname.version-explicit-and-from-defaults
  (list (pathname-version (make-pathname :name "a" :version :newest))
        (pathname-version (make-pathname :defaults (make-pathname :version :newest))))
  (:newest :newest))

;; Merging is where :NEWEST comes from.
(deftest make-pathname.merge-still-fills-newest
  (pathname-version (merge-pathnames (make-pathname :name "a")
                                     (make-pathname :name "b")))
  :newest)

;; A pathname that has its own name takes DEFAULT-VERSION, not the defaults'
;; version (CLHS MERGE-PATHNAMES). This was hidden while (make-pathname) came
;; back with version :NEWEST.
(deftest merge-pathnames.named-takes-default-version
  (let ((p1 (make-pathname :name "foo"))
        (p2 (make-pathname :type "lsp" :version :newest)))
    (list (pathname-version (merge-pathnames p1 p2 nil))
          (pathname-version (merge-pathnames p1 p2 :newest))
          ;; no name of its own: the version comes along with the defaults' name
          (pathname-version (merge-pathnames (make-pathname :type "x")
                                             (make-pathname :name "n" :version :newest)
                                             nil))))
  (nil :newest :newest))
