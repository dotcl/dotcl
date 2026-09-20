;;; PROBE-FILE answers a directory's truename, not the caller's spelling.
;;;
;;; CLHS 21.4 says probe-file returns the truename, and a directory's truename
;;; carries its last component in the directory part -- so the namestring ends in
;;; a separator. TRUENAME did that; PROBE-FILE handed back whatever it was given.
;;; Inside one implementation the two disagreed about the same file.
;;;
;;; What it cost: every "is this a directory?" written the portable way asks
;;; probe-file and then looks at the shape of the answer. uiop:directory-exists-p
;;; said NIL for a directory named without a trailing slash and
;;; uiop:file-exists-p said the same name was a file, so a linter handed a
;;; directory read it as source and reported an unreadable file (found while
;;; running mallet on dotcl: `mallet .` died, `mallet src/` worked).

(defun %pfd-dir ()
  "A directory that certainly exists, as a namestring without a trailing slash."
  (let ((d (concatenate 'string
                        (substitute #\/ (code-char 92)
                                    (or (dotcl:getenv "TMPDIR")
                                        (dotcl:getenv "TEMP") "/tmp"))
                        "/dotcl-pfd-probe")))
    (ensure-directories-exist (concatenate 'string d "/"))
    d))

;;; The shape that was wrong.
(deftest probe-file-directory.trailing-slash
  (let ((p (probe-file (%pfd-dir))))
    (and p (eql #\/ (char (namestring p) (1- (length (namestring p)))))))
  t)

;;; ...and it is the same answer TRUENAME gives, which is the actual rule.
(deftest probe-file-directory.agrees-with-truename
  (let ((d (%pfd-dir)))
    (equal (namestring (probe-file d)) (namestring (truename d))))
  t)

;;; Naming the directory with the slash already on was never broken; it must
;;; stay that way rather than collecting a second one.
(deftest probe-file-directory.slash-not-doubled
  (let* ((d (concatenate 'string (%pfd-dir) "/"))
         (n (namestring (probe-file d))))
    (search "//" n))
  nil)

;;; A file still probes as a file: no separator, and nothing to confuse with a
;;; directory of the same name.
(deftest probe-file-directory.file-unchanged
  (let ((f (concatenate 'string (%pfd-dir) "/pfd-file.txt")))
    (with-open-file (s f :direction :output :if-exists :supersede)
      (write-string "x" s))
    (let ((n (namestring (probe-file f))))
      (eql #\t (char n (1- (length n))))))
  t)

;;; Nothing there is still NIL.
(deftest probe-file-directory.missing-is-nil
  (probe-file (concatenate 'string (%pfd-dir) "/pfd-no-such-thing"))
  nil)
