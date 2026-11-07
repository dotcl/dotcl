;;; An :OUTPUT-FILE with a name and no type gets the .fasl type, in COMPILE-FILE-PATHNAME and in
;;; COMPILE-FILE (which then writes NAME.fasl). A name with a type is left alone.

(deftest cf-output-file-type.pathname
  (list (namestring (compile-file-pathname "x.lisp" :output-file "out"))
        (namestring (compile-file-pathname "x.lisp" :output-file "out.fasl"))
        (namestring (compile-file-pathname "x.lisp" :output-file "out.bin"))
        (namestring (compile-file-pathname "x.lisp"))
        (namestring (compile-file-pathname "x.lisp" :output-file "dir/sub/out")))
  ("out.fasl" "out.fasl" "out.bin" "x.fasl" "dir/sub/out.fasl"))

(deftest-emitting-only cf-output-file-type.compile-file-writes-fasl
  (let* ((dir (regression-temp-dir)) (src (format nil "~A/cfoft.lisp" dir))
         (out (format nil "~A/cfoft-out" dir)) (result nil))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string "(defun cfoft-f () 1)" s))
    (unwind-protect
         (progn (setq result (namestring (compile-file src :output-file out)))
                (list (and (probe-file (format nil "~A.fasl" out)) t)
                      (null (probe-file out))
                      (and (search "cfoft-out.fasl" result) t)))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file (format nil "~A.fasl" out)))
      (ignore-errors (delete-file out))))
  (t t t))
