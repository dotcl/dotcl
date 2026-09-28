;;; On Windows, uiop:native-namestring answers the Windows spelling of a path.
;;;
;;; A native namestring is the string handed to the operating system. uiop has
;;; no native hook for dotcl and falls back to NAMESTRING, which uses `/` on
;;; every OS, so `C:/Users/me/data` went to `cmd /c dir` and cmd read `/Users`
;;; as a switch ("Invalid switch"). SBCL answers `C:\Users\me\data`; so does
;;; dotcl now. Lisp namestrings are unchanged: NAMESTRING and PROBE-FILE still
;;; use `/`.
;;;
;;; Windows only: elsewhere the native separator is `/` already.

#+windows
(require "asdf")

#+windows
(progn

(defun %unn (x) (uiop:native-namestring x))

(defun %unn-jp (sep)
  "A path with a space and Japanese names (Nihongo folder / file.txt),
built from code points so this file stays ASCII, joined with SEP."
  (flet ((s (&rest codes) (map 'string #'code-char codes)))
    (format nil "C:~aProgram Files~a~a ~a~a~a.txt"
            sep sep
            (s #x65E5 #x672C #x8A9E)            ; nihongo
            (s #x30D5 #x30A9 #x30EB #x30C0)     ; foruda
            sep
            (s #x30D5 #x30A1 #x30A4 #x30EB))))  ; fairu

(deftest uiop-native-namestring.file
  (%unn #p"C:/tmp/a.txt")
  "C:\\tmp\\a.txt")

(deftest uiop-native-namestring.directory
  (%unn "C:/Users/me/data/")
  "C:\\Users\\me\\data\\")

(deftest uiop-native-namestring.relative
  (list (%unn #p"a/b/c.txt") (%unn "rel dir/"))
  ("a\\b\\c.txt" "rel dir\\"))

(deftest uiop-native-namestring.unc
  (%unn "\\\\srv\\share\\x\\y.txt")
  "\\\\srv\\share\\x\\y.txt")

(deftest uiop-native-namestring.spaces-and-japanese
  (equal (%unn (%unn-jp "/")) (%unn-jp "\\"))
  t)

;;; The Lisp namestring keeps its `/`.
(deftest uiop-native-namestring.lisp-namestring-unchanged
  (namestring #p"C:/tmp/a.txt")
  "C:/tmp/a.txt")

;;; parse-native-namestring reads it back to the same pathname.
(deftest uiop-native-namestring.round-trip
  (loop for x in (list "C:/tmp/a.txt" "C:/Users/me/data/" "a/b/c.txt"
                       "rel dir/" "\\\\srv\\share\\x\\y.txt"
                       (%unn-jp "/"))
        for p = (pathname x)
        unless (equal (uiop:parse-native-namestring (%unn p)) p)
          collect x)
  nil)

;;; What the report hit: cmd's own DIR given the native spelling of a
;;; directory with a space in its name.
(deftest uiop-native-namestring.cmd-dir
  (let* ((dir (concatenate 'string (regression-temp-dir)
                           "/dotcl-unn-test/sub dir/"))
         (file (concatenate 'string dir "hello.txt")))
    (ensure-directories-exist dir)
    (with-open-file (s file :direction :output :if-exists :supersede)
      (write-line "x" s))
    (string-trim '(#\Return #\Newline #\Space)
                 (uiop:run-program (list "cmd" "/c" "dir" "/b"
                                         (uiop:native-namestring dir))
                                   :output :string)))
  "hello.txt")

;;; uiop's own callers that build a path from a native namestring and parse it
;;; back (temporary files) still work.
(deftest uiop-native-namestring.temporary-file
  (let ((seen nil))
    (uiop:with-temporary-file (:pathname p :keep nil)
      (setf seen (list (and (probe-file p) t)
                       (find #\\ (namestring p)))))
    seen)
  (t nil))

)
