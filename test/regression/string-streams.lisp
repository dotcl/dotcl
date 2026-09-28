;;; Regression tests for string-input-stream file-position.
;;; (setf file-position) on string-input-streams was a no-op (returned NIL
;;; without repositioning). Also the getter must return the correct position.

;; string-input-stream repositioning via (setf file-position)
(deftest string-stream-file-position-setf
  (let ((s (make-string-input-stream "hello")))
    (read-char s)
    (file-position s 0)       ; returns new position
    (read-char s))            ; reads from start
  #\h)

(deftest string-stream-file-position-setf-return
  (let ((s (make-string-input-stream "hello")))
    (read-char s)
    (file-position s 0))
  0)

(deftest string-stream-file-position-getter
  (let ((s (make-string-input-stream "hello")))
    (read-char s)
    (read-char s)
    (file-position s))
  2)

;; Seeking to the end (position == length) is valid: leaves the stream at EOF.
;; Previously SeekToPosition rejected `>= _endOffset`, so this returned NIL.
(deftest string-stream-file-position-to-eof-return
  (file-position (make-string-input-stream "hi") 2)  ; length 2, seek to EOF
  2)

(deftest string-stream-file-position-to-eof-reads-eof
  (let ((s (make-string-input-stream "hello")))
    (file-position s 5)                 ; seek to EOF (length)
    (read-char s nil :done))            ; nothing left -> :done
  :done)

;; Past the end is still rejected (returns NIL, stream unmoved).
(deftest string-stream-file-position-past-end
  (let ((s (make-string-input-stream "hi")))
    (list (file-position s 3)           ; past end -> NIL
          (read-char s)))               ; stream unmoved -> #\h
  (nil #\h))

;; READ after FILE-POSITION moves a string stream back must read from the new
;; position, even when an earlier READ on the same stream left a reader behind
;; and the stream was then read to its end.
(deftest string-stream-read-after-file-position-back
  (with-input-from-string (s "(a)42")
    (let ((first (read s)))
      (read-line s nil nil)             ; consume "42" up to end of input
      (file-position s 3)
      (list first (read s nil :eof) (file-position s))))
  ((a) 42 5))

(deftest string-stream-read-preserving-whitespace-after-reposition
  (with-input-from-string (s "(+ 1 2)42")
    (read-line s nil nil)
    (file-position s 0)
    (let ((form (read-preserving-whitespace s nil :eof nil)))
      (read-line s nil nil)
      (file-position s 7)
      (list form (read-preserving-whitespace s nil :eof nil))))
  ((+ 1 2) 42))
