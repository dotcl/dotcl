;;; FILE-POSITION on a character :IO file stream after writing.
;;;
;;; An :IO file stream reads through a position-tracking reader and writes
;;; through a writer on the same underlying file. Writes moved the file but not
;;; the reader, so FILE-POSITION kept answering the reader's count: 0 after any
;;; amount of output. Code that remembers a position while writing and seeks
;;; back to it to read (osicat's temporary-file test, for one) read from the
;;; start of the file instead.
;;;
;;; Expected values are SBCL's.

(defparameter *iofp-path*
  (concatenate 'string (regression-temp-dir) "/dotcl-iofp.txt"))

(defun iofp-open ()
  (open *iofp-path* :direction :io :if-exists :supersede
                    :if-does-not-exist :create))

(deftest io-stream-file-position.after-writes
  (with-open-stream (s (iofp-open))
    (list (file-position s)
          (progn (write-string "abc" s) (file-position s))
          (progn (write-string "defgh" s) (file-position s))))
  (0 3 8))

;;; The osicat test: remember a position between writes, seek back, read.

(deftest io-stream-file-position.seek-back-and-read
  (with-open-stream (s (iofp-open))
    (print 'foo s)
    (let ((pos (file-position s)))
      (print 'bar s)
      (print 'baz s)
      (file-position s pos)
      (list pos (read s) (read s) (read s nil :eof))))
  (5 bar baz :eof))

;;; Reading after a write continues from where the write ended.

(deftest io-stream-file-position.read-after-write
  (with-open-stream (s (iofp-open))
    (write-string "hello world" s)
    (file-position s 6)
    (list (read-char s)
          (file-position s)
          (progn (file-position s :start) (read-line s))))
  (#\w 7 "hello world"))
