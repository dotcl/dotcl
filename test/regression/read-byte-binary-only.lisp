;;; READ-BYTE takes a binary input stream (CLHS): a file stream opened for characters
;;; and a string input stream are a type-error. A binary stream, and a file opened
;;; with :ELEMENT-TYPE :DEFAULT (bytes and characters), still read bytes.

(defun %rbo-file (name)
  (let ((path (merge-pathnames name (regression-temp-dir))))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "AB" s))
    path))

(deftest read-byte-binary-only.character-file
  (handler-case (with-open-file (s (%rbo-file "rbo1.txt")) (read-byte s))
    (type-error () :type-error))
  :type-error)

(deftest read-byte-binary-only.explicit-character-file
  (handler-case (with-open-file (s (%rbo-file "rbo2.txt") :element-type 'character) (read-byte s))
    (type-error () :type-error))
  :type-error)

(deftest read-byte-binary-only.string-stream
  (handler-case (with-input-from-string (s "AB") (read-byte s))
    (type-error () :type-error))
  :type-error)

(deftest read-byte-binary-only.binary-file-reads
  (with-open-file (s (%rbo-file "rbo3.txt") :element-type '(unsigned-byte 8))
    (list (read-byte s) (read-byte s) (read-byte s nil :eof)))
  (65 66 :eof))

(deftest read-byte-binary-only.default-element-type-reads
  (with-open-file (s (%rbo-file "rbo4.txt") :element-type :default)
    (list (read-byte s) (read-byte s)))
  (65 66))

(deftest read-byte-binary-only.character-reads-unchanged
  (with-open-file (s (%rbo-file "rbo5.txt"))
    (list (read-char s) (read-char s)))
  (#\A #\B))
