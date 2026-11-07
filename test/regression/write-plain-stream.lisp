;;; WRITE-CHAR, WRITE-STRING and WRITE-LINE on a file or string output stream (outside a
;;; pretty-printer block) go straight to the stream's writer. The text, the line-start state
;;; that FRESH-LINE reads, the column ~T reads, and the errors are as through the long path.

(defun %wps-path () (regression-temp-file (format nil "wps-~a" (random 1000000))))

(defun %wps-file-text (fn)
  (let ((path (%wps-path)))
    (unwind-protect
         (progn (with-open-file (o path :direction :output :if-exists :supersede) (funcall fn o))
                (with-open-file (i path) (let ((s (make-string (file-length i)))) (subseq s 0 (read-sequence s i)))))
      (ignore-errors (delete-file path)))))

(deftest write-plain.string-stream-text
  (with-output-to-string (o)
    (write-char #\a o) (write-string "bc" o) (write-line "de" o) (write-string "" o) (write-char #\Newline o)
    (write-string "x" o :start 0 :end 1) (write-string "hello" o :start 1 :end 3))
  "abcde

xel")

(deftest write-plain.file-text
  (%wps-file-text (lambda (o) (write-char #\a o) (write-string "bc" o) (write-line "de" o) (terpri o)
                    (write-string (format nil "tab~aend" #\Tab) o)))
  "abcde

tab	end")

(deftest write-plain.fresh-line-after-each
  (with-output-to-string (o)
    (fresh-line o)
    (write-char #\a o) (fresh-line o)
    (write-string "b" o) (fresh-line o) (fresh-line o)
    (write-line "c" o) (fresh-line o)
    (write-string (format nil "d~%") o) (fresh-line o)
    (write-char #\Newline o) (fresh-line o)
    (write-string "" o) (fresh-line o)
    (write-char #\e o))
  "a
b
c
d

e")

(deftest write-plain.fresh-line-on-file
  (%wps-file-text (lambda (o) (write-string "x" o) (fresh-line o) (write-line "y" o) (fresh-line o) (write-char #\z o)))
  "x
y
z")

(deftest write-plain.column-for-tabulation
  (list (with-output-to-string (o) (write-string "ab" o) (format o "~10T|") (write-char #\c o) (format o "~3,1T|"))
        (%wps-file-text (lambda (o) (write-char #\a o) (write-string "bc" o) (format o "~6T|"))))
  ("ab        |c |" "abc   |"))

(deftest write-plain.string-argument-kinds
  (with-output-to-string (o)
    (write-string (make-string 3 :initial-element #\z) o)
    (write-string (coerce "abc" '(vector character)) o)
    (write-line (make-array 2 :element-type 'character :initial-element #\q :adjustable t :fill-pointer 1) o)
    (write-string "tail" o))
  "zzzabcq
tail")

(deftest write-plain.return-values
  (let ((s (make-string-output-stream)) (str "val"))
    (list (eq str (write-string str s)) (eq str (write-line str s)) (write-char #\k s)))
  (t t #\k))

(deftest write-plain.errors
  (let ((closed (make-string-output-stream)))
    (close closed)
    (list (handler-case (write-char #\a closed) (error () :error))
          (handler-case (write-string "a" closed) (error () :error))
          (handler-case (write-line "a" closed) (error () :error))
          (handler-case (write-char 65 (make-string-output-stream)) (error () :error))))
  (:error :error :error :error))

(deftest write-plain.binary-file-rejects-characters
  (let ((path (%wps-path)))
    (unwind-protect
         (with-open-file (o path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
           (list (handler-case (write-char #\a o) (error () :error))
                 (progn (write-byte 65 o) :byte-ok)))
      (ignore-errors (delete-file path))))
  (:error :byte-ok))

(deftest write-plain.composite-and-pretty-streams
  (let* ((a (make-string-output-stream)) (b (make-string-output-stream))
         (bc (make-broadcast-stream a b))
         (tw (make-two-way-stream (make-string-input-stream "") (make-string-output-stream))))
    (write-char #\x bc) (write-string "yz" bc) (write-line "w" bc)
    (write-string "q" tw)
    (list (get-output-stream-string a) (get-output-stream-string b)
          (get-output-stream-string (two-way-stream-output-stream tw))
          (with-output-to-string (o)
            (pprint-logical-block (o '(1 2) :prefix "(" :suffix ")")
              (write-string "a" o) (write-char #\b o) (pprint-newline :mandatory o) (write-line "c" o)))
          (let ((*standard-output* (make-string-output-stream)))
            (write-string "s") (write-char #\t) (write-line "u")
            (get-output-stream-string *standard-output*))))
  ("xyzw
" "xyzw
" "q" "(ab
 c
)" "stu
"))

(deftest write-plain.io-file-stream
  (let ((path (%wps-path)))
    (unwind-protect
         (with-open-file (s path :direction :io :if-exists :supersede)
           (write-string "hello" s) (write-char #\! s) (file-position s 0)
           (list (read-line s) (file-position s)))
      (ignore-errors (delete-file path))))
  ("hello!" 6))
