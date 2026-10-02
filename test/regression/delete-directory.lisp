;;; DOTCL:DELETE-DIRECTORY, with the contract of SBCL's SB-EXT:DELETE-DIRECTORY
;;; (expected values taken from SBCL 2.6.8): an empty directory named in
;;; directory or file form is deleted and its directory-form pathname returned;
;;; FILE-ERROR for a file, for nothing there, and for a non-empty directory
;;; unless :RECURSIVE is true, which deletes the whole tree. (SBCL signals a
;;; SIMPLE-ERROR rather than a FILE-ERROR for a file with :RECURSIVE T; here it
;;; is FILE-ERROR like the other cases.)

(defun %dd-outcome (thunk)
  (handler-case (let ((v (funcall thunk))) (if (pathnamep v) (list :ok (car (last (pathname-directory v)))) (list :ok v)))
    (file-error () :file-error)
    (error () :error)))

(deftest delete-directory.contract
  (let* ((base (pathname (concatenate (quote string) (string-right-trim "/" (namestring (regression-temp-dir))) "/ddir/")))
         (f (merge-pathnames "full/x.txt" base)))
    (dolist (d '("e1/" "e2/" "full/" "tree/a/b/"))
      (ensure-directories-exist (merge-pathnames d base)))
    (with-open-file (o f :direction :output :if-exists :supersede) (write-line "x" o))
    (with-open-file (o (merge-pathnames "tree/a/y.txt" base) :direction :output :if-exists :supersede)
      (write-line "y" o))
    (list (%dd-outcome (lambda () (dotcl:delete-directory (merge-pathnames "e1/" base))))
          (%dd-outcome (lambda () (dotcl:delete-directory (namestring (merge-pathnames "e2" base)))))
          (%dd-outcome (lambda () (dotcl:delete-directory f)))
          (%dd-outcome (lambda () (dotcl:delete-directory (merge-pathnames "nope/" base))))
          (%dd-outcome (lambda () (dotcl:delete-directory (merge-pathnames "full/" base))))
          (%dd-outcome (lambda () (dotcl:delete-directory (merge-pathnames "tree/" base) :recursive t)))
          (and (probe-file (merge-pathnames "tree/" base)) t)
          (and (probe-file f) t)))
  ((:ok "e1") (:ok "e2") :file-error :file-error :file-error (:ok "tree") nil t))
