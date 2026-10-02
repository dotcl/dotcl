;;; DOTCL:NATIVE-NAMESTRING and DOTCL:PARSE-NATIVE-NAMESTRING: the string a
;;; pathname is handed to the operating system as, and back.
;;;
;;; They are what UIOP's functions of the same names call on dotcl, the way
;;; UIOP calls SB-EXT's on SBCL. On Windows the native spelling uses \
;;; (C:\dir\file.txt, \\server\share\... for UNC); elsewhere it is the Unix one.
;;; Writing gives what UIOP's UNIX-NAMESTRING gives; reading gives what SBCL's
;;; SB-EXT:PARSE-NATIVE-NAMESTRING gives (a name with no dot has type NIL, "."
;;; components stay, ".." is :UP), so code written against SBCL's answers works.
;;;
;;; The workers take the OS as an argument, so both spellings are checked here
;;; on whatever host runs the suite. The Windows-only file
;;; uiop-native-namestring-windows.lisp checks the real thing through UIOP.

(require "asdf")

(defun dnn-native (x windows-p)
  (funcall (find-symbol "%NATIVE-NAMESTRING" "DOTCL.CIL-COMPILER") x windows-p))

(defun dnn-parse (s windows-p)
  (funcall (find-symbol "%PARSE-NATIVE-NAMESTRING" "DOTCL.CIL-COMPILER") s windows-p))

(deftest dotcl-native-namestring.exported-and-documented
  (list (eq (nth-value 1 (find-symbol "NATIVE-NAMESTRING" "DOTCL")) :external)
        (eq (nth-value 1 (find-symbol "PARSE-NATIVE-NAMESTRING" "DOTCL")) :external)
        (stringp (documentation 'dotcl:native-namestring 'function))
        (stringp (documentation 'dotcl:parse-native-namestring 'function)))
  (t t t t))

(deftest dotcl-native-namestring.nil
  (list (dotcl:native-namestring nil) (dotcl:parse-native-namestring nil))
  (nil nil))

;;; --- Windows ---------------------------------------------------------------

(deftest dotcl-native-namestring.windows-file
  (dnn-native #p"C:/tmp/a.txt" t)
  "C:\\tmp\\a.txt")

(deftest dotcl-native-namestring.windows-directory
  (dnn-native "C:/Users/me/data/" t)
  "C:\\Users\\me\\data\\")

(deftest dotcl-native-namestring.windows-relative
  (list (dnn-native #p"a/b/c.txt" t) (dnn-native "rel dir/" t))
  ("a\\b\\c.txt" "rel dir\\"))

(deftest dotcl-native-namestring.windows-unc
  (dnn-native "\\\\srv\\share\\x\\y.txt" t)
  "\\\\srv\\share\\x\\y.txt")

;;; Reading the native spelling back gives the same pathname.
(deftest dotcl-native-namestring.windows-round-trip
  (loop for x in '("C:/tmp/a.txt" "C:/Users/me/data/" "a/b/c.txt" "rel dir/"
                   "\\\\srv\\share\\x\\y.txt")
        for p = (pathname x)
        unless (equal (dnn-parse (dnn-native p t) t) p)
          collect x)
  nil)

;;; --- Unix: the same answers UIOP gave ---------------------------------------

(defparameter *dnn-unix-corpus*
  '("/tmp/a.txt" "/tmp/dir/" "a/b/c.txt" "rel dir/" "" "a" ".bashrc" "foo."
    "a.b.c" "/a/./b/../c" "//x//y/" "/" "./" "../x" "*.lisp" "/tmp/[x]?.txt"))

;;; SB-EXT:PARSE-NATIVE-NAMESTRING's answers on SBCL 2.6.8 (directory, name,
;;; type) for the same corpus. No wildcard syntax: "*" and "?" are characters
;;; of a name.
(defparameter *dnn-sbcl-native-answers*
  '(((:absolute "tmp") "a" "txt") ((:absolute "tmp" "dir") nil nil)
    ((:relative "a" "b") "c" "txt") ((:relative "rel dir") nil nil)
    (nil nil nil) (nil "a" nil) (nil ".bashrc" nil) (nil "foo" "")
    (nil "a.b" "c") ((:absolute "a" "." "b" :up) "c" nil)
    ((:absolute "x" "y") nil nil) ((:absolute) nil nil)
    ((:relative ".") nil nil) ((:relative :up) "x" nil)
    (nil "*" "lisp") ((:absolute "tmp") "[x]?" "txt")))

(deftest dotcl-native-namestring.unix-parse-matches-sbcl-native
  (loop for s in *dnn-unix-corpus*
        for want in *dnn-sbcl-native-answers*
        for p = (dnn-parse s nil)
        unless (equal (list (pathname-directory p) (pathname-name p) (pathname-type p))
                      want)
          collect s)
  nil)

(deftest dotcl-native-namestring.unix-namestring-matches-uiop
  (loop for s in *dnn-unix-corpus*
        for p = (uiop:parse-unix-namestring s)
        unless (equal (dnn-native p nil) (uiop:unix-namestring p))
          collect s)
  nil)

(deftest dotcl-native-namestring.unix-examples
  (list (dnn-native "/tmp/a b/c.txt" nil)
        (pathname-directory (dnn-parse "/a/./b/../c" nil))
        (pathname-name (dnn-parse "/tmp/*.lisp" nil)))
  ("/tmp/a b/c.txt" (:absolute "a" "." "b" :up) "*"))
