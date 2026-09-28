;;; The cmd> prompt of the bundled REPL.
;;;
;;; A comma typed at the start of an empty line switches the prompt to cmd> for
;;; one line, with the command names offered in a menu under it. Typed straight
;;; on, the line runs as the comma command it spells, as it did before.
;;;
;;; The keystrokes need a console. What they decide is checked here: when the
;;; comma switches, which commands are offered for what is typed, what taking
;;; one does, what the menu writes, and what the line runs.

(require "dotcl-repl")

(defun rcm-mode (ch &key (empty t) pasting (commands t) (menus t))
  (let ((mode (dotcl-repl::line-mode-for-key ch empty pasting commands menus)))
    (and mode (dotcl-repl::line-mode-name mode))))

(defvar *rcm-mode* (find :command dotcl-repl::*line-modes*
                         :key #'dotcl-repl::line-mode-name))

(defun rcm-labels (text)
  (mapcar (lambda (item) (getf item :label))
          (dotcl-repl::command-menu-items text)))

(defun rcm-item (text label)
  (find label (dotcl-repl::command-menu-items text)
        :key (lambda (item) (getf item :label)) :test #'string=))

(defun rcm-choose (text label how)
  (multiple-value-list
   (dotcl-repl::command-line-choose text (rcm-item text label) how)))

(defun rcm-capture (fn)
  "Call FN with both outputs captured. Returns its value and the two outputs."
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((value (let ((*standard-output* out) (*error-output* err))
                   (funcall fn))))
      (list value (get-output-stream-string out) (get-output-stream-string err)))))

;;; -- Entering the prompt ------------------------------------------------------

(deftest rcm-comma-on-empty-line
  (rcm-mode #\,)
  :command)

;;; Where a comma is not the first thing typed at the primary prompt, or arrives
;;; in a paste, it goes into the line.
(deftest rcm-comma-not-taken
  (list (rcm-mode #\, :empty nil)
        (rcm-mode #\, :pasting t)
        (rcm-mode #\, :commands nil))
  (nil nil nil))

;;; Where no menu can be drawn (not a terminal, TERM=dumb) the comma is not
;;; taken and ,doc car goes as typed. The shell mode needs no menu and stays.
(deftest rcm-no-menu-no-mode
  (list (rcm-mode #\, :menus nil) (rcm-mode #\; :menus nil))
  (nil :shell))

(deftest rcm-prompt
  (let ((prompt (dotcl-repl::mode-prompt-string *rcm-mode*)))
    (list (dotcl-repl::strip-control-sequences prompt)
          (dotcl-repl::prompt-display-width prompt)))
  ("cmd> " 5))

;;; -- What the menu offers -----------------------------------------------------

;;; On the empty line, every command once, in the order ,help lists them.
(deftest rcm-all-commands
  (let ((labels (rcm-labels "")))
    (list (= (length labels) (length dotcl-repl:*command-list*))
          (first labels)))
  (t "help"))

;;; Typing narrows it. A name matched through an alias is shown by its own name,
;;; and a command named exactly by what is typed comes first.
(deftest rcm-narrowing
  (list (rcm-labels "d") (rcm-labels "de") (rcm-labels "q") (rcm-labels "h")
        (rcm-labels "untr") (rcm-labels "?") (rcm-labels "DO"))
  (("doc" "describe" "dis") ("describe") ("quit" "ql") ("help" "history")
   ("untrace") ("?") ("doc")))

;;; Nothing matches, or the name is finished and the argument begun: no menu.
(deftest rcm-no-items
  (list (rcm-labels "zz") (rcm-labels "doc car") (rcm-labels "doc "))
  (nil nil nil))

(deftest rcm-detail
  (let ((item (rcm-item "pw" "pwd")))
    (and (search "package" (getf item :detail)) t))
  t)

;;; -- Taking a choice ----------------------------------------------------------

;;; Enter on a name typed out in full runs it as typed, whatever is marked:
;;; ,d still means ,doc, and ,cd with nothing after it still goes home.
(deftest rcm-enter-whole-name
  (list (rcm-choose "d" "doc" :enter) (rcm-choose "cd" "cd" :enter)
        (rcm-choose "Q" "quit" :enter))
  ((:submit "d") (:submit "cd") (:submit "Q")))

;;; Enter on a prefix runs the marked command when it needs no argument, and
;;; puts the name and a blank in the line when it does.
(deftest rcm-enter-prefix
  (list (rcm-choose "pw" "pwd" :enter) (rcm-choose "he" "help" :enter)
        (rcm-choose "do" "doc" :enter) (rcm-choose "untr" "untrace" :enter))
  ((:submit "pwd") (:submit "help") (:edit "doc ") (:submit "untrace")))

;;; TAB only fills in the name, with a blank when there is an argument to type.
(deftest rcm-tab
  (list (rcm-choose "do" "doc" :tab) (rcm-choose "pw" "pwd" :tab)
        (rcm-choose "he" "help" :tab) (rcm-choose "d" "describe" :tab))
  ((:edit "doc ") (:edit "pwd") (:edit "help ") (:edit "describe ")))

(deftest rcm-required-argument
  (mapcar (lambda (name)
            (dotcl-repl::command-argument-required-p
             (gethash name dotcl-repl:*commands*)))
          '("doc" "help" "cd" "untrace" "pwd" "load"))
  (t nil nil nil nil t))

;;; -- The mark -----------------------------------------------------------------

;;; No mark to start with on the empty line: down marks the first row and up
;;; the last. The mark stops at the ends.
(deftest rcm-select
  (list (dotcl-repl::mode-menu-select :down nil 5)
        (dotcl-repl::mode-menu-select :up nil 5)
        (dotcl-repl::mode-menu-select :down 4 5)
        (dotcl-repl::mode-menu-select :up 0 5)
        (dotcl-repl::mode-menu-select :down 1 5)
        (dotcl-repl::mode-menu-select :down nil 0))
  (0 4 4 0 2 nil))

;;; -- What the menu writes -----------------------------------------------------

;;; Anchored at the end of the line (column 7), the rows below it with no row
;;; marked, and the cursor back at the point (column 6) on the input row.
(deftest rcm-menu-string
  (multiple-value-bind (out top)
      (dotcl-repl::mode-menu-string '("aa" "bb") nil 0 40 24 7 6)
    (list top
          (string= out
                   (format nil "~C[G~C[7C~C[J~C~C  aa~C~C  bb~C[2A~C[G~C[7C~C[G~C[6C"
                           #\Escape #\Escape #\Escape #\Return #\Newline
                           #\Return #\Newline #\Escape #\Escape #\Escape
                           #\Escape #\Escape))))
  (0 t))

(deftest rcm-menu-string-marked
  (let ((out (dotcl-repl::mode-menu-string '("aa" "bb") 1 0 40 24 7 7)))
    (list (and (search "  aa" out) t) (and (search "> bb" out) t)))
  (t t))

;;; A long list shows ten rows and follows the mark.
(deftest rcm-menu-scrolls
  (let ((labels (loop for i below 30 collect (format nil "c~D" i))))
    (multiple-value-bind (out top)
        (dotcl-repl::mode-menu-string labels 25 0 40 24 3 3)
      (list top (and (search "> c25" out) t) (search "c15" out)
            (and (search (format nil "~C[10A" #\Escape) out) t))))
  (16 t nil t))

;;; -- Running the line ---------------------------------------------------------

;;; cmd> doc car runs what ,doc car runs, with the same output.
(deftest rcm-same-as-comma-line
  (let ((typed (rcm-capture (lambda () (dotcl-repl::answer-line "pwd" *rcm-mode* nil))))
        (comma (rcm-capture (lambda () (dotcl-repl:dispatch ",pwd")))))
    (list (first typed) (string= (second typed) (second comma))
          (and (search "package:" (second typed)) t)))
  ("" t t))

;;; An error in a command is reported the way ,-lines report it.
(deftest rcm-unknown-command
  (let ((result (rcm-capture (lambda () (dotcl-repl::answer-line "nosuch" *rcm-mode* nil)))))
    (list (first result) (and (search "no command named ,nosuch" (third result)) t)))
  ("" t))

;;; ,quit ends the REPL from cmd> too; a blank line runs nothing.
(deftest rcm-quit-and-blank
  (list (first (rcm-capture (lambda () (dotcl-repl::answer-line "quit" *rcm-mode* nil))))
        (rcm-capture (lambda () (dotcl-repl::answer-line "" *rcm-mode* nil)))
        (rcm-capture (lambda () (dotcl-repl::answer-line "   " *rcm-mode* nil))))
  (nil ("" "" "") ("" "" "")))

;;; The history keeps it as the comma line, which runs at the Lisp prompt too.
(deftest rcm-history-entry
  (list (dotcl-repl::command-history-entry "doc car")
        (dotcl-repl::command-history-entry "  "))
  (",doc car" nil))

;;; -- ,help ---------------------------------------------------------------------

(deftest rcm-help-explains
  (let ((out (with-output-to-string (*standard-output*)
               (dotcl-repl:dispatch ",help"))))
    (list (and (search "cmd>" out) t)
          (and (search "  ,  cmd>  Run a comma command" out) t)
          (and (search ",cd [directory]" out) t)))
  (t t t))
