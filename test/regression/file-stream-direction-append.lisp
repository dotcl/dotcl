;;; Operations that use a file stream in the wrong direction signal an error, and an abortive
;;; CLOSE of an append stream cuts the file back to the length it had when it was opened.

(defvar *fsda-path* (regression-temp-file "file-stream-direction.txt"))

(defun %fsda-write (text)
  (with-open-file (o *fsda-path* :direction :output :if-exists :supersede)
    (write-string text o)))

(defun %fsda-error-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(deftest file-stream-direction-append.wrong-direction
  (unwind-protect
       (progn
         (%fsda-write "abc")
         (list (%fsda-error-p (lambda () (with-open-file (s *fsda-path*) (fresh-line s))))
               (%fsda-error-p (lambda () (with-open-file (s *fsda-path* :direction :output :if-exists :supersede)
                                           (listen s))))
               (%fsda-error-p (lambda () (%fsda-write "abc")
                                (with-open-file (s *fsda-path* :element-type '(unsigned-byte 8))
                                  (write-sequence (vector 1 2 3) s))))))
    (ignore-errors (delete-file *fsda-path*)))
  (t t t))

(deftest file-stream-direction-append.valid-uses-unchanged
  (unwind-protect
       (progn
         (%fsda-write "abc")
         (list (with-open-file (s *fsda-path*) (listen s))
               (with-open-file (s *fsda-path* :direction :output :if-exists :supersede)
                 (write-char #\a s) (list (fresh-line s) (fresh-line s)))
               (with-open-file (s *fsda-path* :direction :io :if-exists :overwrite)
                 (write-string "xy" s) (file-position s))))
    (ignore-errors (delete-file *fsda-path*)))
  (t (t nil) 2))

(deftest file-stream-direction-append.append-abort-restores-length
  (unwind-protect
       (progn
         (%fsda-write "0123456789")
         (let ((s (open *fsda-path* :direction :output :if-exists :append)))
           (write-string "abcde" s)
           (close s :abort t))
         (let ((after-abort (with-open-file (s *fsda-path*) (file-length s))))
           (let ((s (open *fsda-path* :direction :output :if-exists :append)))
             (write-string "abcde" s)
             (close s))
           (list after-abort (with-open-file (s *fsda-path*) (file-length s)))))
    (ignore-errors (delete-file *fsda-path*)))
  (10 15))
