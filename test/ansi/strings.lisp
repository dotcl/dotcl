;;; strings.lisp — minimal tests for ANSI strings category

;;; --- nstring-upcase ---

(deftest nstring-upcase.basic
  (let* ((s (copy-seq "a"))
         (s2 (nstring-upcase s)))
    (values (eq s s2) s))
  t "A")

(deftest nstring-upcase.start
  (nstring-upcase (copy-seq "abcdef") :start 2)
  "abCDEF")

;;; --- nstring-downcase ---

(deftest nstring-downcase.basic
  (let* ((s (copy-seq "A"))
         (s2 (nstring-downcase s)))
    (values (eq s s2) s))
  t "a")

;;; --- nstring-capitalize ---

(deftest nstring-capitalize.basic
  (nstring-capitalize (copy-seq "hello world"))
  "Hello World")

;;; --- stringp for character arrays ---

(deftest stringp-char-array
  (notnot (stringp (make-array 4 :element-type 'character
                               :initial-contents '(#\a #\b #\c #\d))))
  t)

;;; --- string-upcase on character array ---

(deftest string-upcase-char-array
  (string-upcase (make-array 3 :element-type 'character
                             :initial-contents '(#\a #\b #\c)))
  "ABC")

;;; --- string= with :start1/:end1/:start2/:end2 ---

(deftest string=-keywords.basic
  (string= "abc" "abd" :end1 2 :end2 2)
  t)
