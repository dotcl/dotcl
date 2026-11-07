;;; (TERPRI [stream]) and (FRESH-LINE [stream]) with zero or one argument are direct
;;; calls with no argument array, and FRESH-LINE on a string stream looks at the last
;;; character instead of copying the whole text. Output and return values are unchanged.

(defun %tfd-path () (regression-temp-file (format nil "tfd-~a" (random 1000000))))

(deftest terpri-fresh-line.string-stream
  (let ((o (make-string-output-stream)))
    (list (fresh-line o) (progn (write-char #\a o) (fresh-line o)) (fresh-line o)
          (terpri o) (fresh-line o) (progn (write-string "bc" o) (terpri o)) (get-output-stream-string o)))
  (nil t nil nil nil nil "a

bc
"))

(deftest terpri-fresh-line.after-get-output-stream-string
  (let ((o (make-string-output-stream)))
    (write-string "xyz" o)
    (get-output-stream-string o)
    (list (fresh-line o) (progn (write-string "q" o) (fresh-line o)) (get-output-stream-string o)))
  (nil t "q
"))

(deftest terpri-fresh-line.designators
  (list (with-output-to-string (*standard-output*) (write-char #\a) (fresh-line) (terpri) (fresh-line nil) (terpri nil))
        (with-output-to-string (*terminal-io*) (write-char #\a *terminal-io*) (fresh-line t) (terpri t))
        (let ((o (make-string-output-stream))) (list (eq nil (terpri o)) (terpri o))))
  ("a


" "a

" (t nil)))

(deftest terpri-fresh-line.file
  (let ((path (%tfd-path)))
    (unwind-protect
         (progn (with-open-file (o path :direction :output :if-exists :supersede)
                  (write-string "a" o) (fresh-line o) (fresh-line o) (terpri o) (write-char #\b o) (terpri o) (fresh-line o))
                (with-open-file (i path) (list (read-line i) (read-line i) (read-line i) (read-line i nil :eof))))
      (ignore-errors (delete-file path))))
  ("a" "" "b" :eof))

(deftest terpri-fresh-line.composite-streams
  (let* ((a (make-string-output-stream)) (b (make-string-output-stream))
         (bc (make-broadcast-stream a b))
         (tw (make-two-way-stream (make-string-input-stream "") a))
         (syn-target (make-string-output-stream)))
    (write-char #\x bc) (fresh-line bc) (terpri bc)
    (write-char #\y tw) (fresh-line tw)
    (let ((*standard-output* syn-target)) (write-char #\z) (fresh-line *standard-output*) (fresh-line))
    (list (get-output-stream-string a) (get-output-stream-string b) (get-output-stream-string syn-target)))
  ("x

y
" "x

" "z
"))

(deftest terpri-fresh-line.pretty-block
  (with-output-to-string (o)
    (pprint-logical-block (o '(1 2) :per-line-prefix ";")
      (write-string "a" o) (terpri o) (fresh-line o) (write-string "b" o) (fresh-line o)))
  ";a
;
;b
;")

(deftest terpri-fresh-line.errors
  (list (handler-case (eval '(terpri nil nil)) (error () :error))
        (handler-case (eval '(fresh-line nil nil)) (error () :error))
        (let ((o (make-string-output-stream))) (close o) (handler-case (terpri o) (error () :error))))
  (:error :error :error))

(deftest terpri-fresh-line.long-buffer
  ;; A long text before FRESH-LINE: the answer comes from the last character.
  (let ((o (make-string-output-stream)))
    (dotimes (i 5000) (write-string "line" o) (terpri o))
    (write-string "tail" o)
    (list (fresh-line o) (fresh-line o) (length (get-output-stream-string o))))
  (t nil 25005))

;;; FINISH-OUTPUT / FORCE-OUTPUT with 0 or 1 argument.

(deftest finish-force-output.designators
  (let ((o (make-string-output-stream)))
    (list (finish-output) (force-output) (finish-output nil) (force-output t)
          (finish-output o) (force-output o)
          (with-output-to-string (*standard-output*) (write-char #\a) (finish-output) (force-output))
          (progn (write-string "ab" o) (finish-output o) (get-output-stream-string o))))
  (nil nil nil nil nil nil "a" "ab"))

(deftest finish-force-output.file
  (let ((path (%tfd-path)))
    (unwind-protect
         (with-open-file (o path :direction :output :if-exists :supersede)
           (write-string "hello" o) (finish-output o) (force-output o)
           (with-open-file (i path) (values (read-line i))))
      (ignore-errors (delete-file path))))
  "hello")

(deftest finish-force-output.errors
  (list (handler-case (eval '(finish-output nil nil)) (error () :error))
        (handler-case (eval '(force-output nil nil)) (error () :error))
        (handler-case (finish-output 5) (error () :error))
        (handler-case (force-output "s") (error () :error)))
  (:error :error :error :error))

(require "dotcl-gray")

(defvar *%ffo-calls* nil)
(defclass %ffo-gray (dotcl-gray:fundamental-character-output-stream) ())
(defmethod dotcl-gray:stream-finish-output ((s %ffo-gray)) (push :finish *%ffo-calls*) nil)
(defmethod dotcl-gray:stream-force-output ((s %ffo-gray)) (push :force *%ffo-calls*) nil)

(deftest finish-force-output.gray
  (let ((g (make-instance '%ffo-gray)))
    (setf *%ffo-calls* nil)
    (finish-output g) (force-output g)
    (let ((*standard-output* g)) (finish-output) (force-output))
    (reverse *%ffo-calls*))
  (:finish :force :finish :force))
