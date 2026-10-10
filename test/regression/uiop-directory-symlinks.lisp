;;; UIOP's listing functions on dotcl keep the names entries were found by,
;;; as on SBCL: DIRECTORY resolves symbolic links (it returns truenames), and
;;; uiop:directory* asks it not to with :RESOLVE-SYMLINKS NIL, so
;;; uiop:directory-files of a directory reached through a link answers paths
;;; under the link. Before, it answered the link target (on macOS, /tmp/x/a
;;; came back as /private/tmp/x/a).
;;;
;;; uiop:print-backtrace on dotcl prints the Lisp frames with their arguments,
;;; up to COUNT. It used to print nothing.
;;;
;;; The expected answers are what SBCL 2.6.8 gives for the same tree.

(require "asdf")

#-windows
(progn
  (defun %uds-setup ()
    (let* ((dir (namestring (truename (regression-temp-dir))))
           (root (format nil "~a/uds/" (string-right-trim "/" dir))))
      (ensure-directories-exist (format nil "~areal/sub/" root))
      (with-open-file (s (format nil "~areal/a.txt" root) :direction :output
                                                         :if-exists :supersede)
        (write-line "x" s))
      (ignore-errors (dotnet:static "System.IO.File" "Delete" (format nil "~adl" root)))
      (dotnet:static "System.IO.Directory" "CreateSymbolicLink"
                     (format nil "~adl" root) (format nil "~areal" root))
      root))

  (defun %uds-names (list root)
    (mapcar (lambda (p)
              (let ((n (namestring p)))
                (if (and (>= (length n) (length root)) (string= root n :end2 (length root)))
                    (subseq n (length root))
                    n)))
            list))

  (deftest uiop-directory-symlinks.directory-files
    (let ((root (%uds-setup)))
      (%uds-names (uiop:directory-files (format nil "~adl/" root)) root))
    ("dl/a.txt"))

  (deftest uiop-directory-symlinks.subdirectories
    (let ((root (%uds-setup)))
      (list (%uds-names (uiop:subdirectories root) root)
            (%uds-names (uiop:subdirectories (format nil "~adl/" root)) root)))
    (("dl/" "real/") ("dl/sub/"))))

(defun %upb-inner (x)
  (let ((s (with-output-to-string (out) (uiop:print-backtrace :stream out :count 3))))
    (list s x)))

(defun %upb-outer (y)
  (values (%upb-inner (1+ y))))

(deftest uiop-print-backtrace.frames
  (let ((s (first (%upb-outer 41))))
    (list (and (search "(%UPB-INNER 42)" s) t)
          (count #\Newline s)
          (string= "0: " s :end2 3)))
  (t 3 t))
