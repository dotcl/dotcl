;;; A logical block opened on a file or console stream is laid out as it is on a string stream:
;;; the output does not depend on the kind of stream. Expected strings are SBCL's.

(defun %pbf-emit (s margin kind)
  (let ((*print-pretty* t) (*print-right-margin* margin) (*print-miser-width* nil))
    (ecase kind
      (:fill
       (pprint-logical-block (s '("aaaa" "bbbb") :prefix "(" :suffix ")")
         (write (pprint-pop) :stream s) (write-char #\Space s)
         (pprint-newline :fill s)
         (write (pprint-pop) :stream s)))
      (:linear
       (pprint-logical-block (s '(1 2 3) :prefix "(" :suffix ")")
         (write (pprint-pop) :stream s) (write-char #\Space s)
         (pprint-newline :linear s)
         (write (pprint-pop) :stream s) (write-char #\Space s)
         (pprint-newline :linear s)
         (write (pprint-pop) :stream s)))
      (:nested
       (pprint-logical-block (s nil :per-line-prefix "; ")
         (write-string "head" s) (write-char #\Space s) (pprint-newline :fill s)
         (pprint-logical-block (s '("x" "y") :prefix "[" :suffix "]")
           (write (pprint-pop) :stream s) (write-char #\Space s) (pprint-newline :mandatory s)
           (write (pprint-pop) :stream s)))))))

(defun %pbf-string (margin kind)
  (with-output-to-string (s)
    (write-string "xx" s)
    (%pbf-emit s margin kind)
    (write-string "yy" s)))

(defun %pbf-file (margin kind)
  (let ((path (regression-temp-file (format nil "pbf-~D.txt" (random 1000000)))))
    (unwind-protect
         (progn
           (with-open-file (f path :direction :output :if-exists :supersede)
             (write-string "xx" f)
             (%pbf-emit f margin kind)
             (write-string "yy" f))
           (with-open-file (f path :direction :input)
             (with-output-to-string (o)
               ;; (A line end that was written as CR LF reads back with its CR on Windows.)
               (loop for c = (read-char f nil nil) while c
                     unless (char= c #\Return) do (write-char c o)))))
      (ignore-errors (delete-file path)))))

(deftest pprint-block-on-file-stream.same-as-string-stream
  (loop for (margin kind) in '((10 :fill) (40 :fill) (6 :linear) (40 :linear) (5 :nested) (40 :nested))
        collect (equal (%pbf-string margin kind) (%pbf-file margin kind)))
  (t t t t t t))

(deftest pprint-block-on-file-stream.no-blank-before-a-break
  (%pbf-file 10 :fill)
  "xx(\"aaaa\"
   \"bbbb\")yy")

(deftest pprint-block-on-file-stream.output-before-and-after-kept
  (list (%pbf-file 40 :fill) (%pbf-file 6 :linear))
  ("xx(\"aaaa\" \"bbbb\")yy"
   "xx(1
   2
   3)yy"))
