;;; Comma commands in the bundled REPL.
;;;
;;; A line starting with a comma is an instruction to the REPL rather than a
;;; form. The terminal half of that cannot be exercised headlessly, but the
;;; deciding half can: splitting a line into a name and an argument, finding the
;;; command a name or alias belongs to, and containing everything that can go
;;; wrong so that a mistyped command cannot end a session.
;;;
;;; The tests that assert "this line reaches this command with this argument"
;;; put a recorder in place of the command's body rather than reading its
;;; output. What matters there is the routing, and a recorder says so without
;;; also running a disassembler or fetching a system from the network.

(require "dotcl-repl")

(defun rcmd-dispatch-capturing (line)
  "DISPATCH LINE with its output captured.
Returns (values RESULT STANDARD-OUTPUT ERROR-OUTPUT)."
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((result (let ((*standard-output* out)
                        (*error-output* err))
                    (dotcl-repl:dispatch line))))
      (values result
              (get-output-stream-string out)
              (get-output-stream-string err)))))

(defun rcmd-run (line)
  "What DISPATCH answers for LINE, with the command's output swallowed."
  (values (rcmd-dispatch-capturing line)))

(defun rcmd-output (line)
  "What the command LINE names printed on *STANDARD-OUTPUT*."
  (nth-value 1 (rcmd-dispatch-capturing line)))

(defun rcmd-error-output (line)
  "What running LINE printed on *ERROR-OUTPUT*."
  (nth-value 2 (rcmd-dispatch-capturing line)))

(defun rcmd-observe (line)
  "Return (NAME ARGUMENT) as the command LINE reaches saw them, or NIL.
The command's body is swapped for a recorder and put back afterwards, so this
says which handler ran and on what without running the real one."
  (let* ((name (values (dotcl-repl:split-command line)))
         (command (and name (gethash name dotcl-repl::*commands*))))
    (when command
      (let ((original (dotcl-repl::command-handler command))
            (seen nil))
        (unwind-protect
             (progn
               (setf (dotcl-repl::command-handler command)
                     (lambda (argument)
                       (setf seen (list (dotcl-repl::command-name command)
                                        argument))))
               (rcmd-run line))
          (setf (dotcl-repl::command-handler command) original))
        seen))))

(defun rcmd-lines (text)
  "TEXT split on newlines."
  (let ((lines '())
        (start 0))
    (loop for i below (length text)
          when (char= (char text i) #\Newline)
            do (push (subseq text start i) lines)
               (setf start (1+ i)))
    (push (subseq text start) lines)
    (nreverse lines)))

(defun rcmd-listing-lines (text)
  "The command lines of a ,help listing: the indented ones that open with a
comma and a name. The line under Modes for the comma has a blank after it."
  (remove-if-not (lambda (line)
                   (and (> (length line) 3)
                        (string= "  ," (subseq line 0 3))
                        (char/= (char line 3) #\Space)))
                 (rcmd-lines text)))

;;; Commands this file defines to test the machinery are named rcmd-something,
;;; so that counting what the contrib itself ships stays possible.
(defun rcmd-test-command-p (command)
  (let ((name (dotcl-repl::command-name command)))
    (and (>= (length name) 5) (string= "rcmd-" (subseq name 0 5)))))

(defun rcmd-builtin-command-count ()
  (count-if-not #'rcmd-test-command-p dotcl-repl::*command-list*))

;;; -- Splitting ---------------------------------------------------------------

(deftest rcmd-split-name-only
  (multiple-value-list (dotcl-repl:split-command ",help"))
  ("help" ""))

(deftest rcmd-split-ordinary-form-is-not-a-command
  (dotcl-repl:split-command "(+ 1 2)")
  nil)

(deftest rcmd-split-empty-line-is-not-a-command
  (dotcl-repl:split-command "")
  nil)

;;; A comma one column in is an unquote, not a command.
(deftest rcmd-split-comma-must-be-first
  (dotcl-repl:split-command " ,help")
  nil)

(deftest rcmd-split-is-case-insensitive
  (values (dotcl-repl:split-command ",HeLp"))
  "help")

(deftest rcmd-split-bare-comma-gives-empty-name
  (multiple-value-list (dotcl-repl:split-command ","))
  ("" ""))

;;; The argument is never divided, so a form keeps its spaces and its brackets.
(deftest rcmd-split-keeps-a-form-whole
  (multiple-value-list
   (dotcl-repl:split-command ",time (loop for i below 3 sum i)"))
  ("time" "(loop for i below 3 sum i)"))

;;; And so does a quoted file name with a space in it.
(deftest rcmd-split-keeps-a-quoted-string-whole
  (multiple-value-list (dotcl-repl:split-command ",load \"a b.lisp\""))
  ("load" "\"a b.lisp\""))

(deftest rcmd-split-trims-around-the-argument
  (multiple-value-list (dotcl-repl:split-command ",cd    test   "))
  ("cd" "test"))

;;; -- Routing -----------------------------------------------------------------
;;;
;;; One per command: a name or an alias, and an argument that arrives unchanged.

(deftest rcmd-routes-help    (rcmd-observe ",h cd")        ("help" "cd"))
(deftest rcmd-routes-quit    (rcmd-observe ",q")           ("quit" ""))
(deftest rcmd-routes-clear   (rcmd-observe ",clear")       ("clear" ""))
(deftest rcmd-routes-history (rcmd-observe ",history")     ("history" ""))
(deftest rcmd-routes-pkg     (rcmd-observe ",pkg cl-user") ("in-package" "cl-user"))
(deftest rcmd-routes-pwd     (rcmd-observe ",pwd")         ("pwd" ""))
(deftest rcmd-routes-cd      (rcmd-observe ",cd test")     ("cd" "test"))
(deftest rcmd-routes-doc     (rcmd-observe ",d car")       ("doc" "car"))
(deftest rcmd-routes-desc    (rcmd-observe ",desc car")    ("describe" "car"))
(deftest rcmd-routes-apropos (rcmd-observe ",ap sort")     ("apropos" "sort"))
(deftest rcmd-routes-args    (rcmd-observe ",args car")    ("args" "car"))
(deftest rcmd-routes-mx      (rcmd-observe ",mx (push 1 x)")  ("mx" "(push 1 x)"))
(deftest rcmd-routes-mxa     (rcmd-observe ",mxa (push 1 x)") ("mxa" "(push 1 x)"))
(deftest rcmd-routes-time    (rcmd-observe ",time (loop for i below 3 sum i)")
  ("time" "(loop for i below 3 sum i)"))
(deftest rcmd-routes-load    (rcmd-observe ",ld \"a b.lisp\"") ("load" "\"a b.lisp\""))
(deftest rcmd-routes-ql      (rcmd-observe ",ql alexandria")   ("ql" "alexandria"))
(deftest rcmd-routes-trace   (rcmd-observe ",trace car")   ("trace" "car"))
(deftest rcmd-routes-untrace (rcmd-observe ",untrace car") ("untrace" "car"))
(deftest rcmd-routes-dis     (rcmd-observe ",dis car")     ("dis" "car"))

;;; ,describe takes a bare symbol as itself and evaluates anything else, so the
;;; quoted and unquoted spellings describe the same symbol, and #'car describes
;;; the function object rather than the list (FUNCTION CAR).
(deftest rcmd-describe-bare-symbol
  (let ((out (rcmd-output ",describe car")))
    (and (search "COMMON-LISP:CAR" out) (search "[symbol]" out) t))
  t)

(deftest rcmd-describe-quoted-symbol
  (let ((out (rcmd-output ",describe 'car")))
    (and (search "COMMON-LISP:CAR" out) (search "[symbol]" out) t))
  t)

(deftest rcmd-describe-function-form
  (let ((out (rcmd-output ",describe #'car")))
    (and (search "[compiled-function]" out) (search "Lambda-list:" out) t))
  t)

;;; Every alias of ,help lands on the same command object, which is what makes
;;; one listing entry cover all three spellings.
(deftest rcmd-aliases-share-one-command
  (let ((help (gethash "help" dotcl-repl::*commands*)))
    (and (eq help (gethash "h" dotcl-repl::*commands*))
         (eq help (gethash "?" dotcl-repl::*commands*))
         t))
  t)

;;; -- ,help -------------------------------------------------------------------
;;;
;;; Asserted before this file defines any command of its own, so the count is
;;; the contrib's.

;;; The bar is 18. The count is asserted rather than the text, so that adding a
;;; command does not have to touch this file.
(deftest rcmd-help-lists-at-least-eighteen
  (>= (length (rcmd-listing-lines (rcmd-output ",help"))) 18)
  t)

(deftest rcmd-contrib-defines-at-least-eighteen
  (>= (rcmd-builtin-command-count) 18)
  t)

;;; The listing is generated from the table, so it has exactly as many lines as
;;; there are commands. A hand-written list would drift from it.
(deftest rcmd-help-lists-every-command-once
  (= (length (rcmd-listing-lines (rcmd-output ",help")))
     (length dotcl-repl::*command-list*))
  t)

(deftest rcmd-help-on-one-command-names-its-aliases
  (let ((out (rcmd-output ",help help")))
    (and (search ",h " out) (search ",?" out) t))
  t)

(deftest rcmd-help-accepts-the-comma
  (and (search ",pwd" (rcmd-output ",help ,pwd")) t)
  t)

(deftest rcmd-help-on-an-unknown-command-is-handled
  (rcmd-run ",help nosuchcommandanywhere")
  :handled)

;;; -- Nothing may kill the REPL -----------------------------------------------

(deftest rcmd-ordinary-line-is-passed-through
  (dotcl-repl:dispatch "(+ 1 2)")
  :not-a-command)

(deftest rcmd-unknown-command-is-handled
  (rcmd-run ",nosuchcommandanywhere")
  :handled)

(deftest rcmd-unknown-command-says-so
  (and (search "nosuchcommandanywhere" (rcmd-error-output ",nosuchcommandanywhere")) t)
  t)

(deftest rcmd-bare-comma-is-handled
  (rcmd-run ",")
  :handled)

(deftest rcmd-bare-comma-points-at-help
  (and (search ",help" (rcmd-error-output ",")) t)
  t)

;;; A command whose argument will not read reports and returns, the same as one
;;; whose body signals for any other reason.
(deftest rcmd-unreadable-argument-is-handled
  (rcmd-run ",cd \"")
  :handled)

(dotcl-repl:define-command ("rcmd-explode") (argument "<anything>")
  "Signal, so that containment can be tested.
Defined by the test suite rather than by the contrib."
  (error "this command always fails"))

(deftest rcmd-error-in-a-body-is-handled
  (rcmd-run ",rcmd-explode now")
  :handled)

(deftest rcmd-error-in-a-body-is-reported
  (and (search "always fails" (rcmd-error-output ",rcmd-explode now")) t)
  t)

;;; -- define-command outside the contrib --------------------------------------
;;;
;;; A user init file is read before this contrib loads unless it requires the
;;; contrib itself, so what is this contrib's to get right is the rest: a
;;; command defined from another package is indistinguishable from one defined
;;; here, aliases and help entry included.

(defvar *rcmd-user-command-saw* nil)

(dotcl-repl:define-command ("rcmd-user" "rcmd-u") (argument "<text>")
  "Record its argument, the way a command in an init file would do work."
  (setf *rcmd-user-command-saw* argument))

(deftest rcmd-user-command-runs
  (progn (setf *rcmd-user-command-saw* nil)
         (rcmd-run ",rcmd-user hello there")
         *rcmd-user-command-saw*)
  "hello there")

(deftest rcmd-user-command-alias-runs
  (progn (setf *rcmd-user-command-saw* nil)
         (rcmd-run ",rcmd-u again")
         *rcmd-user-command-saw*)
  "again")

(deftest rcmd-user-command-is-listed
  (and (search ",rcmd-user <text>" (rcmd-output ",help")) t)
  t)

;;; -- ,quit -------------------------------------------------------------------
;;;
;;; :QUIT is what makes READLINE answer NIL, which the read loop already treats
;;; as end of input. Every spelling has to reach it.

(deftest rcmd-quit-asks-to-stop  (rcmd-run ",quit") :quit)
(deftest rcmd-q-asks-to-stop     (rcmd-run ",q")    :quit)
(deftest rcmd-exit-asks-to-stop  (rcmd-run ",exit") :quit)

;;; And no other command does.
(deftest rcmd-pwd-does-not-ask-to-stop
  (rcmd-run ",pwd")
  :handled)

;;; -- ,in-package -------------------------------------------------------------

(deftest rcmd-in-package-changes-the-current-package
  (let ((before *package*))
    (unwind-protect
         (progn (rcmd-run ",in-package cl-user")
                (package-name *package*))
      (setf *package* before)))
  "COMMON-LISP-USER")

(deftest rcmd-in-package-on-an-unknown-package-is-handled
  (let ((before *package*))
    (unwind-protect
         (rcmd-run ",in-package no-such-package-here")
      (setf *package* before)))
  :handled)

;;; -- ,cd ---------------------------------------------------------------------
;;;
;;; The suite runs from the top of the tree, so regression/run.lisp is not
;;; reachable from here and is reachable from test/. That is the whole
;;; assertion: after ,cd a relative name resolves against the new directory.

(defun rcmd-with-cwd-restored (thunk)
  "Call THUNK with the working directory and *DEFAULT-PATHNAME-DEFAULTS* put
back afterwards, whichever way it leaves."
  (let ((before (dotcl:getcwd))
        (defaults *default-pathname-defaults*))
    (unwind-protect (funcall thunk)
      (dotcl:chdir before)
      (setf *default-pathname-defaults* defaults))))

(deftest rcmd-relative-name-does-not-resolve-before-cd
  (probe-file "regression/run.lisp")
  nil)

(deftest rcmd-cd-moves-relative-resolution
  (rcmd-with-cwd-restored
   (lambda ()
     (rcmd-run ",cd test")
     (and (probe-file "regression/run.lisp") t)))
  t)

(deftest rcmd-cd-moves-default-pathname-defaults
  (rcmd-with-cwd-restored
   (lambda ()
     (rcmd-run ",cd test")
     (and (search "test" (namestring *default-pathname-defaults*)) t)))
  t)

;;; The directory the tests found on the way in is the one they leave behind.
(deftest rcmd-cd-leaves-no-trace
  (let ((before (namestring (dotcl:getcwd))))
    (rcmd-with-cwd-restored (lambda () (rcmd-run ",cd test")))
    (string= before (namestring (dotcl:getcwd))))
  t)

(deftest rcmd-cd-to-a-missing-directory-is-handled
  (rcmd-with-cwd-restored
   (lambda () (rcmd-run ",cd no-such-directory-anywhere")))
  :handled)

;;; -- Reading the argument ----------------------------------------------------

(deftest rcmd-quoted-file-name-is-read-as-a-string
  (dotcl-repl::argument-namestring ",load" "\"a b.lisp\"")
  "a b.lisp")

;;; An unquoted argument is taken literally, the way a shell takes it, so a
;;; path does not have to be quoted to be usable.
(deftest rcmd-unquoted-file-name-is-taken-literally
  (dotcl-repl::argument-namestring ",load" "a.lisp")
  "a.lisp")

(deftest rcmd-directory-namestring-gets-a-separator
  (dotcl-repl::as-directory-namestring "test")
  "test/")

(deftest rcmd-directory-namestring-keeps-one-it-has
  (dotcl-repl::as-directory-namestring "test/")
  "test/")

;;; -- Where a command is recognised -------------------------------------------
;;;
;;; While a form is unfinished the read loop prompts with spaces, and on such a
;;; line a leading comma is an unquote inside a backquoted form. READLINE checks
;;; the prompt to tell the two apart.

(deftest rcmd-a-blank-prompt-is-a-continuation
  (dotcl-repl::continuation-prompt-p "          ")
  t)

(deftest rcmd-a-real-prompt-is-not-a-continuation
  (dotcl-repl::continuation-prompt-p "CL-USER> ")
  nil)

;;; -- Completion --------------------------------------------------------------

(deftest rcmd-command-names-complete
  (let ((result (dotcl-repl::complete-line ",hist" 5)))
    (list (getf result :start)
          (getf result :end)
          (getf (first (getf result :items)) :label)))
  (1 5 "history"))

;;; A prefix shared by several commands offers all of them.
(deftest rcmd-ambiguous-command-name-offers-every-match
  (> (length (getf (dotcl-repl::complete-line ",d" 2) :items)) 1)
  t)

;;; Past the name the line is arguments, and command completion has nothing to
;;; say about those.
(deftest rcmd-no-command-completion-past-the-name
  (dotcl-repl::command-name-completions ",cd test" 8)
  nil)

;;; An ordinary line still reaches the completer that was installed.
(deftest rcmd-ordinary-lines-still-complete
  (let ((result (dotcl-repl::complete-line "(make-insta" 11)))
    (and (getf result :items) t))
  t)

(deftest rcmd-tab-on-a-command-name-inserts-it
  (let ((r (dotcl-repl::complete (coerce ",hist" 'list) 5)))
    (coerce (first r) 'string))
  ",history")
