;;; The inline menu of the bundled REPL, and the debugger's use of it.
;;;
;;; A menu is drawn on the rows under the line being typed, moved with the
;;; arrows, taken with Enter, and put away with Escape or Ctrl+C. The debugger
;;; offers its restarts this way when the line editor is on; digits still
;;; choose by number, and without a terminal the restarts are listed and read
;;; by number as they always were.
;;;
;;; What each key does, which rows are shown and what is written are functions
;;; of their arguments, and RUN-MENU reads keys and writes through functions it
;;; is given, so all of it is checked here without a terminal.

(require "dotcl-repl")

(defun rmn-esc (s)
  "S with ESC spelled as <E>, CR as <R> and LF as <N>, for readable expectations."
  (with-output-to-string (out)
    (loop for ch across s
          do (case ch
               (#\Escape (write-string "<E>" out))
               (#\Return (write-string "<R>" out))
               (#\Newline (write-string "<N>" out))
               (t (write-char ch out))))))

(defun rmn-count (needle s)
  (loop with start = 0
        for at = (search needle s :start2 start)
        while at
        count t
        do (setf start (1+ at))))

(defun rmn-step (key selected typed count)
  (multiple-value-list
   (dotcl-repl::menu-step key selected typed count :numbered t)))

(defun rmn-run (labels keys &rest options)
  "Run a menu over LABELS on the keys KEYS. Returns the two values and what
was written, escapes spelled out."
  (let ((out (make-string-output-stream)))
    (multiple-value-bind (action value)
        (apply #'dotcl-repl::run-menu labels
               :read-key (lambda ()
                           (let ((k (pop keys)))
                             (if (functionp k) (funcall k) (or k :eof))))
               :write (lambda (s) (write-string s out))
               :painter (lambda (s) (concatenate 'string "[" s "]"))
               options)
      (list action value (rmn-esc (get-output-stream-string out))))))

(defvar *rmn-three* '("0: [ABORT] Return to top level."
                      "1: [RETRY] Retry."
                      "2: [USE-VALUE] Use a value."))

;;; -- Cutting to the width -------------------------------------------------------

(deftest rmn-truncate
  (list (dotcl-repl::truncate-to-width "abcdef" 10)
        (dotcl-repl::truncate-to-width "abcdef" 6)
        (dotcl-repl::truncate-to-width "abcdef" 5)
        (dotcl-repl::truncate-to-width "abcdef" 2))
  ("abcdef" "abcdef" "ab..." ".."))

;;; A wide character is two columns and is never split.
(deftest rmn-truncate-wide
  (let ((s (dotcl-repl::truncate-to-width
            (coerce (list (code-char #x3042) (code-char #x3044) (code-char #x3046)) 'string)
            6)))
    (list (length s) (dotcl-repl::string-display-width s)))
  (3 6))

(deftest rmn-truncate-wide-cut
  (let ((s (dotcl-repl::truncate-to-width
            (coerce (list (code-char #x3042) (code-char #x3044) (code-char #x3046)) 'string)
            5)))
    (list (subseq s 1) (dotcl-repl::string-display-width s)))
  ("..." 5))

;;; A report that runs over several lines is one row in a menu.
(deftest rmn-label-one-row
  (dotcl-repl::menu-label-text (format nil "Use~%a value.~C" #\Tab))
  "Use a value. ")

(deftest rmn-row-text
  (list (dotcl-repl::menu-row-text "abc" t 10)
        (dotcl-repl::menu-row-text "abc" nil 10)
        (dotcl-repl::menu-row-text "abcdefghij" nil 10))
  ("> abc" "  abc" "  abcd..."))

;;; -- Keys -----------------------------------------------------------------------

(deftest rmn-step-arrows
  (list (rmn-step :down 0 "" 3)
        (rmn-step :down 2 "" 3)
        (rmn-step :up 1 "" 3)
        (rmn-step :up 0 "" 3)
        (rmn-step :end 0 "" 3)
        (rmn-step :home 2 "" 3))
  ((:redraw 1 "") (:none 2 "") (:redraw 0 "") (:none 0 "") (:redraw 2 "")
   (:redraw 0 "")))

(deftest rmn-step-enter-cancel-eof
  (list (rmn-step :enter 1 "" 3)
        (rmn-step :cancel 1 "" 3)
        (rmn-step :eof 0 "" 3)
        (rmn-step :eof 1 "1" 3)
        (rmn-step :ignore 1 "" 3))
  ((:choose 1 "") (:cancel 1 "") (:eof 0 "") (:none 1 "1") (:none 1 "")))

;;; Digits mark the choice they name and collect on the prompt row.
(deftest rmn-step-digits
  (list (rmn-step #\2 0 "" 3)
        (rmn-step #\7 0 "" 3)
        (rmn-step #\1 0 "1" 12)
        (rmn-step #\5 1 "1" 12)
        (rmn-step #\0 0 "" 3))
  ((:redraw 2 "2") (:none 0 "") (:redraw 11 "11") (:redraw 5 "5")
   (:redraw 0 "0")))

;;; An arrow after digits drops them: the mark is what Enter takes.
(deftest rmn-step-arrow-after-digits
  (rmn-step :down 1 "1" 3)
  (:redraw 2 ""))

(deftest rmn-step-backspace
  (list (rmn-step :backspace 11 "11" 12)
        (rmn-step :backspace 2 "2" 3)
        (rmn-step :backspace 2 "" 3))
  ((:redraw 1 "1") (:redraw 2 "") (:none 2 "")))

;;; Any other key is the caller's, and so is a digit when digits are not
;;; choices.
(deftest rmn-step-other
  (list (rmn-step #\: 0 "" 3)
        (multiple-value-list (dotcl-repl::menu-step #\1 0 "" 3)))
  ((:other 0 "") (:other 0 "")))

;;; -- Which rows are shown -------------------------------------------------------

(deftest rmn-visible-rows
  (list (dotcl-repl::menu-visible-rows 3 24)
        (dotcl-repl::menu-visible-rows 30 24)
        (dotcl-repl::menu-visible-rows 30 5)
        (dotcl-repl::menu-visible-rows 30 2))
  (3 10 3 1))

(deftest rmn-window-top
  (list (dotcl-repl::menu-window-top 0 0 10 30)
        (dotcl-repl::menu-window-top 9 0 10 30)
        (dotcl-repl::menu-window-top 10 0 10 30)
        (dotcl-repl::menu-window-top 29 0 10 30)
        (dotcl-repl::menu-window-top 15 20 10 30)
        (dotcl-repl::menu-window-top 25 20 10 30))
  (0 0 1 20 15 20))

;;; -- What is written ------------------------------------------------------------

;;; The anchor row is gone back to by column, the rows go below it one line feed
;;; each, and the cursor goes back up by as many rows as were written: right at
;;; the bottom of the window too, where the line feeds scroll.
(deftest rmn-render
  (rmn-esc (dotcl-repl::render-menu *rmn-three* 1 0 3 40 3 ""
                                    (lambda (s) (concatenate 'string "[" s "]"))))
  "<E>[G<E>[3C<E>[J<R><N>  0: [ABORT] Return to top level.<R><N>[> 1: [RETRY] Retry.]<R><N>  2: [USE-VALUE] Use a value.<E>[3A<E>[G<E>[3C")

;;; Digits typed are written after the prompt, and the cursor rests after them.
(deftest rmn-render-typed
  (rmn-esc (dotcl-repl::render-menu *rmn-three* 2 0 3 40 3 "2"))
  "<E>[G<E>[3C2<E>[J<R><N>  0: [ABORT] Return to top level.<R><N>  1: [RETRY] Retry.<R><N>> 2: [USE-VALUE] Use a value.<E>[3A<E>[G<E>[4C")

;;; A row longer than the terminal is cut one column short of it, so no row
;;; wraps and the count of rows to go back up stays exact.
(deftest rmn-render-narrow
  (rmn-esc (dotcl-repl::render-menu *rmn-three* 0 0 3 16 3 ""))
  "<E>[G<E>[3C<E>[J<R><N>> 0: [ABORT]...<R><N>  1: [RETRY]...<R><N>  2: [USE-VA...<E>[3A<E>[G<E>[3C")

;;; Only the rows of the window are written.
(deftest rmn-render-window
  (let* ((labels (loop for i below 30 collect (format nil "~D" i)))
         (s (dotcl-repl::render-menu labels 25 20 10 40 3 "")))
    (list (count #\Newline s)
          (and (search "  20" s) t)
          (and (search "> 25" s) t)
          (search "  19" s)
          (search "  30" s)
          (and (search (format nil "~C[10A" #\Escape) s) t)))
  (10 t t nil nil t))

(deftest rmn-erase
  (rmn-esc (dotcl-repl::erase-menu-string 3))
  "<E>[G<E>[3C<E>[J")

;;; The marked row is painted as the REPL paints, which is not at all unless
;;; colour is on.
(deftest rmn-paint-selected
  (list (rmn-esc (dotcl::%repl-paint :selected "> x" t))
        (dotcl::%repl-paint :selected "> x" nil))
  ("<E>[7m> x<E>[0m" "> x"))

;;; -- Running it -----------------------------------------------------------------

;;; The menu's default size comes from the window. Without a console behind
;;; standard output (a detached or redirected process on Windows) the size
;;; query throws; the size must then fall back to a default, not signal.
(deftest rmn-terminal-size-always-answers
  (list (typep (dotcl-repl::terminal-width) '(integer 1))
        (typep (dotcl-repl::terminal-height) '(integer 1)))
  (t t))

(deftest rmn-run-down-enter
  (let ((r (rmn-run *rmn-three* (list :down :enter) :anchor-col 3 :width 40 :height 24)))
    (list (first r) (second r)
          ;; Drawn twice, then taken away: three erases.
          (rmn-count "<E>[J" (third r))
          (let ((s (third r)))
            (subseq s (- (length s) (length "<E>[G<E>[3C<E>[J"))))))
  (:choose 1 3 "<E>[G<E>[3C<E>[J"))

(deftest rmn-run-digit-enter
  (subseq (rmn-run *rmn-three* (list #\2 :enter) :anchor-col 3 :numbered t) 0 2)
  (:choose 2))

(deftest rmn-run-digit-shown
  (let ((s (third (rmn-run *rmn-three* (list #\2 :enter) :anchor-col 3 :numbered t))))
    (and (search "<E>[G<E>[3C2<E>[J" s) t))
  t)

(deftest rmn-run-cancel
  (let ((r (rmn-run *rmn-three* (list :down :cancel) :anchor-col 3)))
    (list (first r) (second r)
          (let ((s (third r)))
            (subseq s (- (length s) (length "<E>[G<E>[3C<E>[J"))))))
  (:cancel nil "<E>[G<E>[3C<E>[J"))

(deftest rmn-run-eof
  (subseq (rmn-run *rmn-three* (list :eof) :anchor-col 3) 0 2)
  (:eof nil))

;;; Another key ends the menu and goes to the caller, with any digits before it.
(deftest rmn-run-other
  (list (subseq (rmn-run *rmn-three* (list :down #\:) :numbered t) 0 2)
        (subseq (rmn-run *rmn-three* (list #\1 #\+) :numbered t) 0 2))
  ((:other ":") (:other "1+")))

;;; Ctrl+C arriving as an interrupt rather than as a key cancels too, and the
;;; menu still comes off the screen.
(deftest rmn-run-interrupt
  (let ((r (rmn-run *rmn-three*
                    (list :down
                          (lambda ()
                            (signal (make-condition
                                     (find-symbol "INTERACTIVE-INTERRUPT"
                                                  "DOTCL-INTERNAL")))
                            :enter))
                    :anchor-col 3)))
    (list (first r)
          (let ((s (third r)))
            (subseq s (- (length s) (length "<E>[G<E>[3C<E>[J"))))))
  (:cancel "<E>[G<E>[3C<E>[J"))

;;; A long menu scrolls to keep the mark in view.
(deftest rmn-run-scrolls
  (let* ((labels (loop for i below 15 collect (format nil "item ~D" i)))
         (r (rmn-run labels (list :end :enter) :height 24)))
    (list (first r) (second r)
          (and (search "> item 14" (third r)) t)))
  (:choose 14 t))

(deftest rmn-run-empty
  (subseq (rmn-run '() (list :enter)) 0 2)
  (:cancel nil))

;;; -- Whether there is a menu at all ---------------------------------------------

(deftest rmn-usable
  (list (dotcl-repl::menu-usable-p :term "xterm-256color" :output-redirected nil
                                   :input-redirected nil)
        (dotcl-repl::menu-usable-p :term nil :output-redirected nil
                                   :input-redirected nil)
        (dotcl-repl::menu-usable-p :term "dumb" :output-redirected nil
                                   :input-redirected nil)
        (dotcl-repl::menu-usable-p :term "xterm" :output-redirected t
                                   :input-redirected nil)
        (dotcl-repl::menu-usable-p :term "xterm" :output-redirected nil
                                   :input-redirected t))
  (t t nil nil nil))

;;; -- The debugger without a terminal ----------------------------------------------
;;;
;;; A REPL on a pipe, the line editor forced on: the debugger has nobody to ask,
;;; lists the restarts as it always did, and draws no menu.

(defvar *rmn-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *rmn-dir*
  (let ((dir (concatenate 'string (regression-temp-dir) "/dotcl-repl-menu-test/")))
    (ensure-directories-exist dir)
    dir))

(defun rmn-image ()
  (let* ((args (dotnet:static "System.Environment" "GetCommandLineArgs"))
         (count (dotnet:invoke args "Length")))
    (loop for i below (1- count)
          when (equal (dotnet:invoke args "GetValue" i) "--core")
            return (list "--core" (dotnet:invoke args "GetValue" (1+ i))))))

(defun rmn-piped (lines &rest flags)
  (let ((input (concatenate 'string *rmn-dir* "input.lisp")))
    (with-open-file (out input :direction :output :if-exists :supersede)
      (dolist (line lines)
        (write-string line out)
        (terpri out)))
    (multiple-value-bind (out err)
        (ignore-errors
         (uiop:run-program (append (list *rmn-exe*) (rmn-image)
                                   (list "--no-init") flags (list "repl"))
                           :input (pathname input)
                           :output :string
                           :error-output :string
                           :ignore-error-status t))
      (concatenate 'string (or out "") (or err "")))))

(defvar *rmn-piped*
  (rmn-piped '("(with-simple-restart (abort \"give up\") (error \"boom\"))")
             "--readline" "--color=always"))

(deftest rmn-piped-lists-restarts
  (and (search "; Available restarts:" *rmn-piped*)
       (search "0: [ABORT]" *rmn-piped*)
       t)
  t)

(deftest rmn-piped-no-menu
  (list (search (format nil "~C[7m" #\Escape) *rmn-piped*)
        (search "> 0: [ABORT]" *rmn-piped*)
        (search (format nil "~C[J" #\Escape) *rmn-piped*))
  (nil nil nil))
