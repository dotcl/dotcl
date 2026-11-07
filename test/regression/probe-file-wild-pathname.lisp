;;; PROBE-FILE, TRUENAME and ENSURE-DIRECTORIES-EXIST signal a FILE-ERROR for a wild pathname,
;;; also when it is given as a string (it was taken literally, or answered NIL). A quoted \* names a
;;; file called *, and ordinary names are unchanged. (Unix: \ quotes; Windows has no * in names.)

(defun %pfwp-dir ()
  (let ((d (format nil "~A/pfwp-test/" (regression-temp-dir))))
    (ensure-directories-exist d)
    (dolist (n '("a.txt" #-windows "star\\*name.txt"))   ; Windows cannot hold a * in a file name
      (with-open-file (s (format nil "~A~A" d n) :direction :output :if-exists :supersede)
        (write-string "x" s)))
    d))

(defmacro %pfwp-file-error-p (form)
  `(handler-case (progn ,form nil) (file-error () t)))

(deftest probe-file-wild-pathname.strings
  (let ((d (%pfwp-dir)))
    (list (%pfwp-file-error-p (probe-file (format nil "~A*.txt" d)))
          (%pfwp-file-error-p (probe-file (format nil "~Aa?.txt" d)))
          (%pfwp-file-error-p (probe-file (format nil "~Astar*name.txt" d)))
          (%pfwp-file-error-p (truename (format nil "~A*.txt" d)))
          (%pfwp-file-error-p (ensure-directories-exist (format nil "~Awild*/child/" d)))
          (%pfwp-file-error-p (probe-file (merge-pathnames "*.txt" d)))
          (and (probe-file (format nil "~Aa.txt" d)) t)
          (and (probe-file d) t)
          (null (probe-file (format nil "~Anosuch.txt" d)))))
  (t t t t t t t t t))

#-windows
(deftest probe-file-wild-pathname.quoted-star
  (let ((d (%pfwp-dir)))
    (list (and (probe-file (format nil "~Astar\\*name.txt" d)) t)
          (and (truename (format nil "~Astar\\*name.txt" d)) t)
          (and (probe-file (make-pathname :directory (pathname-directory d) :name "star*name" :type "txt")) t)
          (%pfwp-file-error-p (probe-file (make-pathname :directory (pathname-directory d) :name :wild :type "txt")))))
  (t t t t))
