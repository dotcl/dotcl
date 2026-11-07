;;; A one-letter logical host that has been defined is a logical host ("H:A;B.LISP"), not a drive;
;;; and "a:b" is a name containing a colon, not drive A and name b. "C:/x" and "C:" are still a
;;; drive.

(deftest pathname-single-letter-host-and-device.logical-host
  (progn
    (setf (logical-pathname-translations "Q") '(("**;*.*.*" "/tmp/qtest/**/*.*")))
    (let ((p (pathname "Q:A;B.LISP")))
      (list (typep p 'logical-pathname)
            (namestring (translate-logical-pathname p)))))
  (t "/tmp/qtest/a/b.lisp"))

(deftest pathname-single-letter-host-and-device.colon-name
  (let ((p (parse-namestring "a:b")))
    (if (find :windows *features*)
        (list (pathname-device p) (pathname-name p))
        (list (pathname-device p) (pathname-name p) (namestring p))))
  #-windows (nil "a:b" "a:b")
  #+windows ("A" "b"))

(deftest pathname-single-letter-host-and-device.drive-kept
  (list (pathname-device (parse-namestring "c:/x/y.lisp"))
        (pathname-name (parse-namestring "c:/x/y.lisp"))
        (pathname-device (parse-namestring "C:"))
        (typep (pathname "C:/foo") 'logical-pathname))
  ("C" "y" "C" nil))
