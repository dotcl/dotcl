;;; On Windows, arguments given to a .bat / .cmd file through RUN-PROGRAM /
;;; LAUNCH-PROCESS / RUN-PROCESS reach it as text: cmd.exe does not run a
;;; second command out of "a&b", expand "%PATH%", or eat a caret.
;;;
;;; A batch file is run by cmd.exe, which parses the whole command line with its
;;; own rules. The quoting used for ordinary executables left & | < > ^ % open to
;;; it, so an argument could run another command. Each argument that needs it is
;;; now quoted for cmd (the batch file sees it in double quotes in %1, and %~1
;;; drops them); one that cmd cannot receive at all (a line break) is refused.
;;;
;;; Windows only: elsewhere a .bat file is not special, so nothing is defined.

(require "asdf")

#+windows
(progn

(defvar *rpba-dir*
  (let ((dir (concatenate 'string (regression-temp-dir)
                          "/dotcl-run-program-batch-args-test/")))
    (ensure-directories-exist dir)
    dir))

;; Prints %1 .. %9, one per line. The space in the name is on purpose.
(defvar *rpba-echo*
  (let ((path (concatenate 'string *rpba-dir* "echo args.bat")))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "@echo off~C~%" #\Return)
      (loop for i from 1 to 9
            do (format out "echo ~D=[%~D]~C~%" i i #\Return)))
    path))

(defun rpba-lines (text)
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for trimmed = (string-right-trim '(#\Return) line)
        unless (equal trimmed "") collect trimmed))

(defun rpba-echo (args)
  "What the batch file sees in %1 .. %9, and anything else printed on the way."
  (rpba-lines (uiop:run-program (cons *rpba-echo* args)
                                :output :string :error-output :output
                                :ignore-error-status t)))

;; The program a forwarding batch file hands %* to: another dotcl that prints
;; the arguments it got.
(defvar *rpba-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defun rpba-image ()
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defvar *rpba-forward*
  (let ((script (concatenate 'string *rpba-dir* "print-args.lisp"))
        (bat (concatenate 'string *rpba-dir* "forward.cmd")))
    (with-open-file (out script :direction :output :if-exists :supersede)
      (write-line "(format t \"ARGS~S~%\" (dotcl:script-arguments))" out))
    (with-open-file (out bat :direction :output :if-exists :supersede)
      (format out "@echo off~C~%" #\Return)
      (format out "\"~A\"~{ \"~A\"~} \"~A\" %*~C~%"
              (substitute #\\ #\/ *rpba-exe*) (rpba-image)
              (substitute #\\ #\/ script) #\Return))
    bat))

(defun rpba-forward (args)
  "The arguments a program receives when a batch file passes %* on to it."
  (let* ((out (uiop:run-program (cons *rpba-forward* args)
                                :output :string :ignore-error-status t))
         (start (search "ARGS(" out)))
    (and start (read-from-string out t nil :start (+ start 4)))))

;;; Characters cmd.exe gives a meaning to reach the batch file as text: no
;;; second command runs, no variable is expanded, the caret stays.
(deftest run-program-batch-args-cmd-metacharacters
  (rpba-echo '("a&echo INJECTED" "%OS%" "^caret" "x&ver" "<i|n>" "(p)" "!OS!" "100%" "plain"))
  ("1=[\"a&echo INJECTED\"]" "2=[\"%OS%\"]" "3=[\"^caret\"]" "4=[\"x&ver\"]"
   "5=[\"<i|n>\"]" "6=[\"(p)\"]" "7=[\"!OS!\"]" "8=[\"100%\"]" "9=[plain]"))

;;; A batch file that forwards %* to a program (the usual .cmd shim) hands it
;;; the original arguments, embedded quotes and backslashes included.
(deftest run-program-batch-args-forwarded-literally
  (let ((args '("a b" "%PATH%" "a&b" "^" "q\"q" "a\\" "a\\\"b" "" "%%cd:~,%" "x y\\")))
    (equal (rpba-forward args) args))
  t)

;;; LAUNCH-PROCESS and RUN-PROCESS take the same path.
(deftest run-program-batch-args-launch-and-run-process
  (list (let ((p (dotcl:launch-process *rpba-echo* '("l&echo INJECTED" "%OS%"))))
          (prog1 (subseq (rpba-lines
                          (with-output-to-string (s)
                            (loop for line = (read-line (dotcl:process-output p) nil)
                                  while line do (write-line line s))))
                         0 2)
            (dotcl:process-wait p)))
        (subseq (rpba-lines (second (dotcl:run-process *rpba-echo* '("r&echo INJECTED" "%OS%"))))
                0 2))
  (("1=[\"l&echo INJECTED\"]" "2=[\"%OS%\"]") ("1=[\"r&echo INJECTED\"]" "2=[\"%OS%\"]")))

;;; cmd.exe ends a command at a line break, so an argument holding one cannot be
;;; passed to a batch file: it is an error, not a cut-short command line.
(deftest run-program-batch-args-line-break-refused
  (list (handler-case (progn (uiop:run-program
                              (list *rpba-echo* (format nil "a~%echo INJECTED")))
                             :ran)
          (error () :refused))
        (handler-case (progn (dotcl:run-process *rpba-echo*
                                                (list (format nil "a~Cb" #\Return)))
                             :ran)
          (error () :refused)))
  (:refused :refused))

) ; #+windows progn
