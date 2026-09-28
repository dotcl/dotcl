;;; Ctrl+Z at the start of a line on standard input.
;;;
;;; A Windows console ends input on a line that starts with Ctrl+Z, and without
;;; the line editor that is the only way to leave the REPL from the keyboard.
;;; Standard input is opened below the console's own reader, so the key used to
;;; arrive as a character (U+001A) and the reader reported it as an invalid
;;; character. On a console the line is now the end of input; on a pipe or a
;;; file the byte is data, as it is to .NET and Python.
;;;
;;; Whether standard input is a console is decided once, when it is opened, so
;;; the console side is checked on the reader itself, given a string and told
;;; it is reading a console. The pipe side is checked on a child process.

(defun scz-reader (text console)
  (dotnet:new "DotCL.StdinReader" (dotnet:new "System.IO.StringReader" text)
              console))

(defun scz-lines (reader n)
  "The next N results of ReadLine, NIL where it answered the end of input."
  (loop repeat n collect (dotnet:invoke reader "ReadLine")))

(defvar *scz-z* (string (code-char 26)))
(defvar *scz-crlf* (coerce (list #\Return #\Newline) 'string))

(defun scz-text (&rest parts)
  (apply #'concatenate 'string parts))

;;; A console: the Ctrl+Z line reads as the end of input, and reading can go
;;; on after it, as it can on the console.
(deftest scz-console-line-start-ends-input
  (scz-lines (scz-reader (scz-text *scz-z* *scz-crlf* "(+ 1 2)" *scz-crlf*) t) 3)
  (nil "(+ 1 2)" nil))

;;; What follows Ctrl+Z on its line is dropped with it.
(deftest scz-console-rest-of-line-dropped
  (scz-lines (scz-reader (scz-text *scz-z* "abc" *scz-crlf* "x" *scz-crlf*) t) 2)
  (nil "x"))

;;; After a line of text, the next line starting with Ctrl+Z ends the input.
(deftest scz-console-after-a-line
  (scz-lines (scz-reader (scz-text "(+ 1 2)" *scz-crlf* *scz-z* *scz-crlf*) t) 2)
  ("(+ 1 2)" nil))

;;; Anywhere but the start of a line it is an ordinary character.
(deftest scz-console-mid-line-is-data
  (map 'list #'char-code
       (first (scz-lines (scz-reader (scz-text "a" *scz-z* *scz-crlf*) t) 1)))
  (97 26))

;;; Read and Peek see the same end of input as ReadLine.
(deftest scz-console-read-char
  (let ((r (scz-reader (scz-text *scz-z* *scz-crlf* "a") t)))
    (list (dotnet:invoke r "Peek") (dotnet:invoke r "Read") (dotnet:invoke r "Read")))
  (-1 -1 97))

;;; Not a console: the character is data.
(deftest scz-pipe-is-data
  (let ((line (first (scz-lines (scz-reader (scz-text *scz-z* *scz-crlf* "x") nil) 1))))
    (map 'list #'char-code line))
  (26))

;;; The same through a child REPL reading a pipe: Ctrl+Z is reported as an
;;; invalid character, and the REPL goes on to the next form.

(defvar *scz-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defun scz-image ()
  "The --core this process was started on, if any, for the child to use too."
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defun scz-run (args text)
  "Run dotcl with ARGS and the characters of TEXT, all below 256, on standard
input as one byte each. Returns standard output and standard error."
  (let ((psi (dotnet:new "System.Diagnostics.ProcessStartInfo" *scz-exe*))
        (buf (dotnet:invoke (dotnet:static "System.Text.Encoding" "Latin1")
                            "GetBytes" text)))
    (dolist (a (append (scz-image) (list "--no-init") args))
      (dotnet:invoke (dotnet:invoke psi "ArgumentList") "Add" a))
    (setf (dotnet:invoke psi "UseShellExecute") nil
          (dotnet:invoke psi "RedirectStandardInput") t
          (dotnet:invoke psi "RedirectStandardOutput") t
          (dotnet:invoke psi "RedirectStandardError") t)
    (let* ((p (dotnet:static "System.Diagnostics.Process" "Start" psi))
           (in (dotnet:invoke p "StandardInput")))
      (dotnet:invoke (dotnet:invoke in "BaseStream") "Write" buf 0
                     (dotnet:invoke buf "Length"))
      (dotnet:invoke in "Close")
      (let* ((err-task (dotnet:invoke (dotnet:invoke p "StandardError")
                                      "ReadToEndAsync"))
             (out (dotnet:invoke (dotnet:invoke p "StandardOutput") "ReadToEnd")))
        (dotnet:invoke p "WaitForExit")
        (values out (dotnet:invoke err-task "Result"))))))

(defvar *scz-out*)
(defvar *scz-err*)
(multiple-value-setq (*scz-out* *scz-err*)
  (scz-run (list "--no-readline" "repl")
           (scz-text *scz-z* *scz-crlf* "(+ 1 2)" *scz-crlf*)))

(deftest scz-pipe-repl-reports-the-character
  (and (search "code 26" *scz-err*) t)
  t)

(deftest scz-pipe-repl-goes-on
  (and (search "CL-USER> 3" *scz-out*) t)
  t)
