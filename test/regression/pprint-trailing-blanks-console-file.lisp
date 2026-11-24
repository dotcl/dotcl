;;; A pretty-printed line break takes back the spaces before it (SBCL, and dotcl on a string
;;; stream, did so; the console and file streams had already written them out), so
;;; lisp-unit2's "Expected 1 " line ended in a blank and its output differed from SBCL's.

(defun %ptb-block (stream)
  (let ((*print-pretty* t))
    (pprint-logical-block (stream nil :per-line-prefix "| ")
      (write-string "ab  " stream)
      (pprint-newline :mandatory stream)
      (write-string "x " stream)
      (pprint-newline :mandatory stream)
      (write-string "y" stream))))

(deftest pprint-trailing-blanks.file-stream
  (let ((path (merge-pathnames "ptb-file.txt" (regression-temp-dir/))))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (%ptb-block s))
    (with-open-file (s path)
      (let ((str (make-string 100)))
        (subseq str 0 (read-sequence str s)))))
  #.(format nil "| ab~%| x~%| y"))

(deftest pprint-trailing-blanks.string-stream
  (with-output-to-string (s) (%ptb-block s))
  #.(format nil "| ab~%| x~%| y"))

(deftest pprint-trailing-blanks.console-and-later-blanks-kept
  (let ((r (dotcl:run-process
            (dotnet:static "System.Environment" "ProcessPath")
            (list "--core" (regression-child-core) "--eval"
                  "(progn (let ((*print-pretty* t)) (pprint-logical-block (*standard-output* nil :per-line-prefix \"| \") (write-string \"ab  \" *standard-output*) (pprint-newline :mandatory *standard-output*) (write-string \"x\" *standard-output*))) (write-string \"end  \") (finish-output))"))))
    (second r))
  #.(format nil "| ab~%| xend  "))
