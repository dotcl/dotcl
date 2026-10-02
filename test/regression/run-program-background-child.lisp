;;; RUN-PROGRAM with a file or null output returns when the child exits, even
;;; if the child left a background process that still holds that output.
;;;
;;; A string command goes through the shell with :output to a temporary file.
;;; On Unix the file used to be fed from a pipe by a copy thread, and waiting
;;; meant waiting for EOF on that pipe, which `sleep 5 &` kept open for five
;;; seconds (and a background writer blocked on a FIFO, forever). SBCL gives the
;;; file to the child directly and returns at once; so does dotcl now.

(require "asdf")

(defun rpbg-elapsed (thunk)
  (let ((t0 (get-internal-real-time)))
    (funcall thunk)
    (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))

#-windows
(deftest run-program-background-child-output-string
  (let (out)
    (list (< (rpbg-elapsed (lambda ()
                             (setq out (uiop:run-program "echo a; sleep 5 &" :output :string))))
             3)
          out))
  (t "a
"))

#-windows
(deftest run-program-background-child-output-nil
  (< (rpbg-elapsed (lambda () (uiop:run-program "sleep 5 &"))) 3)
  t)

#-windows
(deftest run-program-background-child-holding-stderr
  (< (rpbg-elapsed (lambda () (uiop:run-program "sleep 5 >/dev/null &" :output :string))) 3)
  t)

;;; The program is still found, and fails to start, exactly as before.
#-windows
(deftest run-program-direct-output-missing-program
  (handler-case (progn (uiop:run-program '("dotcl-no-such-program-zz") :output nil) :ran)
    (error (e)
      (and (search "An error occurred trying to start process 'dotcl-no-such-program-zz'"
                   (princ-to-string e))
           t)))
  t)

;;; Arguments reach the program unparsed.
#-windows
(deftest run-program-direct-output-arguments
  (let ((file (concatenate 'string (regression-temp-dir) "/dotcl-rpbg-args.txt")))
    (uiop:run-program (list "printf" "[%s]" "a b" "$HOME" "'q\"" "-x" "") :output file)
    (prog1 (uiop:read-file-string file) (delete-file file)))
  "[a b][$HOME]['q\"][-x][]")
