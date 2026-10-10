;;; The line DOTCL:FUNCTION-SOURCE-LOCATION records for a top-level definition
;;; must be the line the form starts on, whatever reader macros came before it.
;;;
;;; Two ways it went wrong:
;;;   - A newline consumed by a reader macro written in Lisp (it reads through the
;;;     stream, not through the reader) was not counted. A readtable whose #\"
;;;     is such a macro (named-readtables users, e.g. mgl-pax) lost one line per
;;;     line of docstring, so every later definition in the file was reported
;;;     too early: 29 lines in one mgl-pax file.
;;;   - A reader macro that READs a subform (the comma inside a backquote) moved
;;;     the recorded line to the subform's line.

(defvar *slr-readtable*
  (let ((rt (copy-readtable nil)))
    (set-macro-character
     #\" (lambda (stream char)
           (declare (ignore char))
           (with-output-to-string (out)
             (loop for c = (read-char stream t nil t)
                   until (char= c #\")
                   do (write-char (if (char= c #\\) (read-char stream t nil t) c) out))))
     nil rt)
    rt))

(defun %slr-fixture-path (name)
  (merge-pathnames name (regression-temp-dir)))

(defun %slr-write-fixture (path)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-line "(defun slr-a ()" s)                   ; 1
    (write-line "  \"Doc" s)                           ; 2
    (write-line "  spanning" s)                        ; 3
    (write-line "  lines.\"" s)                        ; 4
    (write-line "  1)" s)                              ; 5
    (write-line "" s)                                  ; 6
    (write-line "(defun slr-b () `(a" s)               ; 7
    (write-line ",(car nil)" s)                        ; 8
    (write-line ",@nil))" s)                           ; 9
    (write-line "(defun slr-c () \"x" s)               ; 10
    (write-line "y\")" s)                              ; 11
    (write-line "; comment" s)                         ; 12
    (write-line "(defun slr-d () 4)" s)))              ; 13

(defun %slr-lines ()
  (mapcar (lambda (name) (getf (dotcl:function-source-location name) :line))
          '(slr-a slr-b slr-c slr-d)))

(deftest source-location-line.load-lisp-string-macro
  (let ((path (%slr-fixture-path "slr-load.lisp")))
    (%slr-write-fixture path)
    (let ((*readtable* *slr-readtable*))
      (load path))
    (%slr-lines))
  (1 7 10 13))

;; An emit-free build has no COMPILE-FILE.
(deftest-emitting-only source-location-line.compile-file-lisp-string-macro
  (let ((path (%slr-fixture-path "slr-cf.lisp")))
    (%slr-write-fixture path)
    (let ((*readtable* *slr-readtable*))
      (compile-file path))
    (%slr-lines))
  (1 7 10 13))

(deftest source-location-line.load-standard-readtable
  (let ((path (%slr-fixture-path "slr-std.lisp")))
    (%slr-write-fixture path)
    (load path)
    (%slr-lines))
  (1 7 10 13))
