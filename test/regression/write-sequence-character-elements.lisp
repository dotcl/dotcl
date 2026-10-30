;;; WRITE-SEQUENCE to a character stream requires character elements.

(defun ws-character-stream-error (seq &rest keys)
  (handler-case
      (let ((s (make-string-output-stream)))
        (apply #'write-sequence seq s keys)
        :returned)
    (type-error () :type-error)))

(deftest write-sequence.character-stream-rejects-non-characters
  (list (ws-character-stream-error '(65 66))
        (ws-character-stream-error #(65 66))
        (ws-character-stream-error (list #\a 98))
        (ws-character-stream-error (list 'a)))
  (:type-error :type-error :type-error :type-error))

(deftest write-sequence.character-stream-accepts-characters
  (list (let ((s (make-string-output-stream)))
          (write-sequence '(65 #\a) s :start 1)
          (get-output-stream-string s))
        (with-output-to-string (s)
          (write-sequence (list #\a #\b) s))
        (with-output-to-string (s)
          (write-sequence #(#\c #\d) s)))
  ("a" "ab" "cd"))

(deftest write-sequence.character-file-rejects-non-characters
  (let ((path (regression-temp-file "write-sequence-character-elements.txt")))
    (unwind-protect
        (with-open-file (s path :direction :output :if-exists :supersede)
          (handler-case (progn (write-sequence '(65 66) s) :returned)
            (type-error () :type-error)))
      (ignore-errors (delete-file path))))
  :type-error)
