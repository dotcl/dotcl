;;; On Windows, dotcl leaves the console's input and output code pages as it
;;; found them when it exits.
;;;
;;; dotcl switches the console to UTF-8 (65001) at startup so that non-ASCII
;;; text reads and prints correctly. The code page belongs to the console, not
;;; to the process, so without a restore it stayed 65001 after dotcl was gone,
;;; and every later program in the same window that assumed the old code page
;;; (932 on Japanese Windows, 437 on US English) printed garbage.
;;;
;;; Each case runs a batch file that sets a code page with chcp, runs another
;;; dotcl, and prints the code page again. RUN-PROCESS starts it with no window,
;;; in a console of its own, so chcp here never touches the console (if any)
;;; the suite itself runs in.
;;;
;;; Windows only: elsewhere there is no console code page.

#+windows
(progn

(defvar *ccp-dir*
  (let ((dir (concatenate 'string (regression-temp-dir)
                          "/dotcl-console-code-page-test/")))
    (ensure-directories-exist dir)
    dir))

(defvar *ccp-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *ccp-core*
  (or (ignore-errors (namestring (truename "compiler/cil-out.sil")))
      "compiler/cil-out.sil"))

(defun ccp-file (name contents)
  (let ((path (concatenate 'string *ccp-dir* name)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string contents out))
    (substitute #\\ #\/ path)))

(defvar *ccp-normal* (ccp-file "normal.lisp" "(princ \"done\")"))
(defvar *ccp-error* (ccp-file "error.lisp" "(error \"on purpose\")"))

(defun ccp-trailing-number (line)
  "The digits at the end of LINE (chcp prints its number last in every
language), or NIL."
  (let* ((end (length (string-right-trim '(#\Space #\Return) line)))
         (start (or (position-if-not #'digit-char-p line :end end :from-end t) -1)))
    (and (< (1+ start) end)
         (parse-integer line :start (1+ start) :end end))))

(defun ccp-run (code-page args &key (stdin "nul"))
  "Set CODE-PAGE, run dotcl with ARGS, and return (exit-code code-page-after)."
  (let* ((bat (ccp-file
               "run.cmd"
               (with-output-to-string (out)
                 (flet ((line (fmt &rest xs)
                          (apply #'format out fmt xs)
                          (format out "~C~%" #\Return)))
                   (line "@echo off")
                   (line "chcp ~D >nul" code-page)
                   (line "\"~A\" --core \"~A\"~{ ~A~} >nul 2>&1 <~A"
                         *ccp-exe* *ccp-core* args stdin)
                   (line "echo EXIT=%errorlevel%")
                   (line "chcp")))))
         (result (dotcl:run-process bat nil))
         (lines (with-input-from-string (in (second result))
                  (loop for line = (read-line in nil)
                        while line
                        for trimmed = (string-right-trim '(#\Return) line)
                        unless (equal trimmed "") collect trimmed)))
         (exit-line (find "EXIT=" lines :test (lambda (p l) (eql 0 (search p l))))))
    (list (and exit-line (parse-integer exit-line :start 5 :junk-allowed t))
          (and lines (ccp-trailing-number (car (last lines)))))))

(deftest console-code-page.script-932
  (ccp-run 932 (list *ccp-normal*))
  (0 932))

(deftest console-code-page.script-437
  (ccp-run 437 (list *ccp-normal*))
  (0 437))

(deftest console-code-page.quit-with-code
  (ccp-run 932 (list "--eval" "\"(dotcl:quit 3)\""))
  (3 932))

(deftest console-code-page.script-error
  (ccp-run 932 (list *ccp-error*))
  (1 932))

;; The REPL reaching end of input: what Ctrl+D (Ctrl+Z on Windows) does.
(deftest console-code-page.repl-end-of-input
  (ccp-run 932 nil)
  (0 932))

)
