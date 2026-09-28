;;; dotcl-repl.lisp: Terminal readline for dotcl
;;;
;;; Usage: (require "dotcl-repl")
;;;        (dotcl-repl:readline "CL-USER> ")
;;;
;;; Features:
;;;   - Character-by-character raw terminal input via System.Console
;;;   - Left/right/home/end cursor movement
;;;   - Backspace, Delete
;;;   - Up/down arrow history, kept between sessions in a file
;;;   - Ctrl+R reverse incremental search of that history
;;;   - Word-wise motion and deletion (Alt+B, Alt+F, Alt+D, Ctrl+W)
;;;   - Tab completion hook (*completer*)
;;;   - CJK-aware display width (wide chars counted as 2 columns)
;;;   - Comma commands (,help lists them), and a cmd> prompt that offers
;;;     their names in a menu when a comma is typed on an empty line
;;;   - Multi-line input: Enter decides by parenthesis balance, and a
;;;     continuation line is indented for you
;;;   - The bracket a closing bracket closes is shown painted, and strings,
;;;     comments and keywords are coloured (*syntax-highlight*)
;;;   - Bracketed paste, so a pasted form arrives as one form
;;;   - A ; on an empty line switches to a one-line shell prompt (sh>)
;;;   - An inline menu drawn under the line (RUN-MENU); the debugger offers
;;;     its restarts with it

(defpackage :dotcl-repl
  (:use :cl)
  (:export #:readline
           #:*history*
           #:*history-max*
           #:*history-file*
           #:*completer*
           #:*syntax-highlight*
           #:enable
           #:disable
           #:define-command
           #:dispatch
           #:split-command
           #:quit-repl
           #:*commands*
           #:*command-list*))

(in-package :dotcl-repl)

;;; -- Public state ------------------------------------------------------------

(defvar *history* '())
(defvar *history-max* 500)

;;; Called with (text offset) -- the whole input line and the cursor position --
;;; and returns either NIL or a plist
;;;
;;;   (:start N :end M :items ((:label "Append" :detail "(String) => ...") ...))
;;;
;;; where START and END delimit the text to replace. This is the shape
;;; dotcl-lsp-api:completions returns, and the same shape an editor needs, so a
;;; completer written for one serves the other. It replaces an older contract
;;; that took a prefix string: a prefix cannot say where a candidate starts,
;;; which breaks as soon as the token is a string literal (a .NET member name).
(defvar *completer* nil)

;;; -- East Asian Width --------------------------------------------------------

(defun char-display-width (ch)
  "Return 1 for narrow, 2 for wide (CJK) characters."
  (let ((cp (char-code ch)))
    (if (or (<= #x1100 cp #x115F)   ; Hangul Jamo
            (<= #x2E80 cp #x303E)   ; CJK Radicals / Kangxi / Punct
            (<= #x3041 cp #x33BF)   ; Hiragana / Katakana / CJK compat
            (<= #x33FF cp #x33FF)
            (<= #x3400 cp #x4DBF)   ; CJK Ext-A
            (<= #x4E00 cp #x9FFF)   ; CJK Unified
            (<= #xA000 cp #xA4CF)   ; Yi
            (<= #xA960 cp #xA97F)   ; Hangul Jamo Ext-A
            (<= #xAC00 cp #xD7FF)   ; Hangul Syllables + Jamo Ext-B
            (<= #xF900 cp #xFAFF)   ; CJK Compat Ideographs
            (<= #xFE10 cp #xFE1F)   ; Vertical Forms
            (<= #xFE30 cp #xFE6F)   ; CJK Compat Forms
            (<= #xFF01 cp #xFF60)   ; Fullwidth
            (<= #xFFE0 cp #xFFE6)   ; Fullwidth Signs
            (<= #x1B000 cp #x1B0FF) ; Kana Supplement
            (<= #x1F004 cp #x1F0CF)
            (<= #x1F200 cp #x1FFFF) ; Enclosed CJK + Emoji
            (<= #x20000 cp #x2FFFD) ; CJK Ext-B..F
            (<= #x30000 cp #x3FFFD))
        2
        1)))

(defun string-display-width (str &optional (end (length str)))
  (loop for i below end sum (char-display-width (char str i))))

;;; A prompt can arrive painted: the read loop colours the package name when
;;; colour is on. The escape sequences that do it take no columns, and counting
;;; them would put every redraw that many columns to the right.
(defun strip-control-sequences (string)
  "STRING without the ESC [ ... control sequences in it."
  (with-output-to-string (out)
    (let ((i 0)
          (n (length string)))
      (loop while (< i n)
            do (let ((ch (char string i)))
                 (cond ((and (char= ch #\Escape) (< (1+ i) n)
                             (char= (char string (1+ i)) #\[))
                        ;; Parameter and intermediate bytes, then one final
                        ;; byte in the range @ to ~.
                        (incf i 2)
                        (loop while (and (< i n)
                                         (<= #x20 (char-code (char string i)) #x3F))
                              do (incf i))
                        (when (< i n) (incf i)))
                       (t (write-char ch out)
                          (incf i))))))))

(defun prompt-display-width (prompt)
  "Columns PROMPT takes on the screen, its colour included at no cost."
  (string-display-width (strip-control-sequences prompt)))

;;; -- Colour --------------------------------------------------------------------
;;;
;;; Whether to paint, and with what, is decided once by the runtime from --color,
;;; NO_COLOR and TERM, so the prompts and messages written here agree with the
;;; ones the read loop writes. An image without the runtime function paints
;;; nothing.

(defun paint (role text &optional (target :output))
  "TEXT painted as ROLE (:prompt :shell :error ...) for TARGET, which is :output,
:error, a stream, or T / NIL to paint or not regardless."
  (let ((fn (find-symbol "%REPL-PAINT" "DOTCL")))
    (if (and fn (fboundp fn))
        (funcall fn role text target)
        text)))

;;; -- System.Console wrappers -------------------------------------------------

(defun console-read-key ()
  "Read a ConsoleKeyInfo without echo. Returns the .NET object."
  (dotnet:static "System.Console" "ReadKey" t))

(defun console-read-key-interruptable ()
  "Read a ConsoleKeyInfo without echo, with a busy wait so the
   thread can be interrupted. Returns the .NET object, or nil
   if the thread was interrupted."
  (handler-case
      ;; Busy-wait loop: Check Console.KeyAvailable periodically,
      ;; sleeping for 50ms at a time if no key is available.
      ;; This allows System.Threading.ThreadInterruptedException to be
      ;; thrown during Thread.Sleep when the REPL thread is interrupted,
      ;; which we can then catch and handle.
      (loop
        (if (dotnet:static "System.Console" "get_KeyAvailable")
            ;; A key is ready; read it without echoing it to the console.
            (return (dotnet:static "System.Console" "ReadKey" t))
            ;; No key is available; sleep for 50ms to yield execution time.
            (dotnet:static "System.Threading.Thread" "Sleep" 50)))
    ;; Catch any .NET System.Exception (which gets wrapped by the runtime
    ;; as a Lisp LispProgramError/error). In case of a ThreadInterruptedException,
    ;; we return nil to notify the caller that reading was interrupted.
    (error () nil)))

;;; On a Windows console, Ctrl+C is normally not a key at all: the console
;;; raises CTRL_C_EVENT for every process attached to it. The REPL's own
;;; process turns that into an interrupt, but a batch file that launched it
;;; (the .cmd shim of a global tool) gets the event too and asks "Terminate
;;; batch job (Y/N)?" once the REPL exits. While the editor waits for a key,
;;; Ctrl+C only ever means "drop this form", so it is read as a key there and
;;; no event is raised. The setting is put back before anything is evaluated,
;;; so Ctrl+C during evaluation still interrupts.
;;;
;;; Only on Windows: on Unix Ctrl+C arrives as SIGINT, the REPL already turns
;;; that into "drop this form" while it waits for input, and nothing else
;;; sees the signal.

(defun ctrl-c-as-input (on)
  "Set Console.TreatControlCAsInput on a Windows console; return the old value.
Does nothing, and returns NIL, anywhere else."
  (when (member :windows *features*)
    (ignore-errors
     (prog1 (dotnet:static "System.Console" "TreatControlCAsInput")
       (setf (dotnet:static "System.Console" "TreatControlCAsInput") on)))))

(defun key-char (ki)
  (dotnet:invoke ki "KeyChar"))

(defun key-key (ki)
  (dotnet:invoke ki "Key"))

(defun key-modifiers (ki)
  (dotnet:invoke ki "Modifiers"))

(defun key-name (ki)
  "What .NET calls this key: \"A\", \"Enter\", \"LeftArrow\" and so on."
  (dotnet:invoke (key-key ki) "ToString"))

(defun console-key= (ki name)
  (string= (key-name ki) name))

(defun key-ctrl-p (ki)
  (let ((mods (dotnet:invoke (key-modifiers ki) "ToString")))
    (search "Control" mods)))

(defun key-alt-p (ki)
  (let ((mods (dotnet:invoke (key-modifiers ki) "ToString")))
    (search "Alt" mods)))

(defun key-shift-p (ki)
  (let ((mods (dotnet:invoke (key-modifiers ki) "ToString")))
    (search "Shift" mods)))

(defun key-waiting-p ()
  "True when a key can be read without waiting for one to be pressed."
  (and (dotnet:static "System.Console" "get_KeyAvailable") t))

(defun write-str (s)
  (dotnet:static "System.Console" "Write" s))

(defun write-ch (ch)
  (dotnet:static "System.Console" "Write" ch))

;;; -- Display helpers ---------------------------------------------------------

;;; The size of the window, or a stand-in when it cannot be had. With no
;;; console behind standard output (a redirected or detached process, a
;;; service, a test harness) Windows .NET throws "The handle is invalid" from
;;; the size query instead of answering, and some hosts answer zero. Either way
;;; the answer is then COLUMNS / LINES from the environment when set, else
;;; 80 x 24, so that code which only lays text out does not fail for want of a
;;; window. Whether there is a console to edit on is ENSURE-CONSOLE's question.
(defun terminal-dimension (getter variable default)
  (let ((n (ignore-errors (dotnet:static "System.Console" getter))))
    (if (and (integerp n) (> n 0))
        n
        (let* ((env (ignore-errors
                     (dotnet:static "System.Environment"
                                    "GetEnvironmentVariable" variable)))
               (v (and (stringp env)
                       (ignore-errors (parse-integer env)))))
          (if (and (integerp v) (> v 0)) v default)))))

(defun terminal-width ()
  (terminal-dimension "get_WindowWidth" "COLUMNS" 80))

;;; Only the redraw of a multi-line input needs this, and only to find out that
;;; it cannot be done. A host that answers zero is treated as one that does not
;;; know, because zero would mean every input is already too tall.
(defun terminal-height ()
  (terminal-dimension "get_WindowHeight" "LINES" 24))

;;; Everything below draws with RELATIVE cursor movement only.
;;;
;;; The obvious way to redraw an edited line is to ask the terminal where the
;;; cursor is and then move it to an absolute position. On Unix .NET answers
;;; those two questions by writing a Device Status Report to the terminal and
;;; reading the reply back off standard input. That reply is ordinary input:
;;; when anything else is reading the same stream -- rlwrap, an ssh session, a
;;; paste that arrives while the report is in flight -- the reply lands in the
;;; program's input instead, and the line fills with stray digits. Relative
;;; movement asks no question, so no answer can reach the wrong reader. The
;;; window width stays a query because it is an ioctl, not a round trip through
;;; the terminal.

(defun ansi-up (n)
  "Escape sequence moving the cursor up N rows."
  (if (> n 0) (format nil "~C[~DA" #\Escape n) ""))

(defun ansi-right (n)
  "Escape sequence moving the cursor right N columns."
  (if (> n 0) (format nil "~C[~DC" #\Escape n) ""))

;;; Move to the first column of the current row.
(defparameter *ansi-column-1* (format nil "~C[G" #\Escape))

;;; Erase from the cursor to the end of the screen.
(defparameter *ansi-erase-below* (format nil "~C[J" #\Escape))

;;; A double-width character is never split across the right margin: with a
;;; single cell left the terminal leaves it blank and starts the character on
;;; the next row. The arithmetic has to make the same choice, or the screen and
;;; the model disagree by a row for every such character.
(defun advance-column (col ch width)
  "Column reached after writing CH at column COL of a WIDTH-column terminal."
  (let ((w (char-display-width ch)))
    (if (and (= w 2) (= (mod col width) (1- width)))
        (+ col 3)
        (+ col w))))

;;; A newline in the content ends the row it is on and opens the next one INDENT
;;; columns in. Those columns are not in the content: they are written by the
;;; redraw so that every line of the input starts under the first one, in the
;;; same way the prompt puts the first line where it is. The indentation a
;;; continuation line really carries is ordinary characters in the buffer, and
;;; is counted here like any other text.
(defun newline-column (col width indent)
  "Column reached by a newline written at column COL.
The row a newline ends is the one COL is on, and text that filled its last cell
has already put COL on the next row, where the newline leaves it: a line filling
the screen exactly is followed by the next line, not by a blank row."
  (+ (* width (1+ (floor (max 0 (1- col)) width))) indent))

(defun layout-column (start content width &optional (end (length content))
                                                    (indent 0))
  "Column reached after writing the first END characters of CONTENT from START."
  (let ((col start))
    (dotimes (i end col)
      (let ((ch (char content i)))
        (setf col (if (char= ch #\Newline)
                      (newline-column col width indent)
                      (advance-column col ch width)))))))

;;; The single place that decides what a redraw looks like. It reads nothing and
;;; writes nothing, so wrapping, wide characters and the resting place of the
;;; cursor can all be checked without a terminal.
(defparameter *ansi-reset* (format nil "~C[0m" #\Escape))

(defun render (prompt-width content point width cursor-row &optional spans)
  "Return three values: the string to write, the row the cursor ends on, and
   the number of rows the input area spans. Rows are counted from the row the
   prompt starts on, and CURSOR-ROW says which of those rows the cursor sits on
   before the redraw. Text ending exactly on the right margin spans one row
   more than it has characters on, because the next character goes on the row
   below and the cursor is already waiting there.

   CONTENT may have newlines in it. A newline starts the next row and the row
   is then padded out to the width of the prompt, so the lines of one input
   line up under each other. That padding is on the screen only; POINT indexes
   CONTENT, which holds the newline and nothing else.

   SPANS paints parts of CONTENT: a list of (START END ON), sorted and not
   overlapping, where ON is the escape sequence that starts the paint and the
   characters from START below END are painted. A reset ends every painted
   run, and a newline is never painted, so the padding of the next row is not
   either. Escape sequences take no columns, so the layout is the same with
   SPANS as without."
  (let* ((width (max 2 width))
         ;; A prompt as wide as the terminal is degenerate, and a rightward move
         ;; stops at the margin anyway, so the model stops there too.
         (prompt-width (min prompt-width (1- width)))
         (indent (make-string prompt-width :initial-element #\Space))
         (col prompt-width)
         ;; True when the character just written filled the last cell of a row.
         ;; A terminal holds that wrap pending until something else arrives,
         ;; leaving the cursor on the old row, so every move from there would be
         ;; off by one.
         (pending nil)
         ;; The escape sequence in force, or NIL when nothing is painted.
         (painted nil)
         (out (make-string-output-stream)))
    ;; Land on the first cell after the prompt, on the prompt's own row. The
    ;; prompt is then to the left of the cursor, so erasing from here down
    ;; cannot touch it and it never has to be written again.
    (write-string (ansi-up cursor-row) out)
    (write-string *ansi-column-1* out)
    (write-string (ansi-right prompt-width) out)
    (write-string *ansi-erase-below* out)
    (dotimes (i (length content))
      (let* ((ch (char content i))
             (on (and (char/= ch #\Newline)
                      (progn
                        (loop while (and spans (<= (second (first spans)) i))
                              do (pop spans))
                        (let ((span (first spans)))
                          (and span (<= (first span) i) (third span)))))))
        (unless (equal on painted)
          (when painted (write-string *ansi-reset* out))
          (when on (write-string on out))
          (setf painted on))
        (cond ((char= ch #\Newline)
               ;; The carriage return is what makes this work over a pending
               ;; wrap: it clears the wrap and puts the cursor at the start of
               ;; the row the text filled, and the line feed then steps to the
               ;; row the arithmetic already counted.
               (write-char #\Return out)
               (write-char #\Newline out)
               (write-string indent out)
               (setf col (newline-column col width prompt-width)
                     pending nil))
              (t
               (write-char ch out)
               (setf col (advance-column col ch width)
                     pending (zerop (mod col width)))))))
    (when painted (write-string *ansi-reset* out))
    ;; Force a wrap left pending by the last character, and the screen agrees
    ;; with the arithmetic again.
    (when pending
      (write-char #\Return out)
      (write-char #\Newline out))
    (let* ((point-col (layout-column prompt-width content width point
                                     prompt-width))
           (end-row (floor col width))
           (point-row (floor point-col width))
           (point-x (mod point-col width)))
      (write-string (ansi-up (- end-row point-row)) out)
      (write-string *ansi-column-1* out)
      (write-string (ansi-right point-x) out)
      (values (get-output-stream-string out) point-row (1+ end-row)))))

;;; -- History -----------------------------------------------------------------

(defun history-push (line)
  "Remember LINE, unless it is empty or is the line before it.
True when it was remembered, which is what decides whether it is also worth
writing to the file."
  (when (and (> (length line) 0)
             (not (equal line (car *history*))))
    (push line *history*)
    (when (> (length *history*) *history-max*)
      (setf *history* (subseq *history* 0 *history-max*)))
    t))

;;; -- History between sessions ------------------------------------------------
;;;
;;; The file sits next to the init file, in the directory dotcl already keeps a
;;; user's own things in. It is deliberately not in the cache tree: everything
;;; under there is by definition safe to throw away, and `dotcl clean` does
;;; throw it away. A history a maintenance command is allowed to eat is not a
;;; history. The pair a reader carries to a new machine is the init file and
;;; what they have typed, and they are now in one directory.
;;;
;;; A line is written the moment it is accepted rather than at exit. Two REPLs
;;; open at once then both keep everything they typed, in the order it was
;;; typed, without either reading the other's file or winning a race at exit,
;;; and a session that is killed or crashes keeps its history as well. The
;;; price is that the file only grows, so a read that finds it too long writes
;;; it back trimmed.
;;;
;;; Nothing here reports a failure. A history is a convenience, and a read-only
;;; home directory or a file belonging to someone else is not a reason to
;;; interrupt a session.

(defvar *history-file* :default
  "Where the history is kept between sessions.
:DEFAULT works it out from the init file's directory, and NIL turns the whole
thing off, which is also what DOTCL_NO_HISTORY=1 does.")

(defvar *history-file-max* 1000
  "Lines the file may hold before a read rewrites it with the newest
*HISTORY-MAX* entries. Twice what is kept in memory, so the rewrite -- the one
operation here that is not an append -- is rare.")

(defvar *history-loaded* nil
  "True once the file has been read, so that enabling the editor twice does not
read it twice.")

(defun default-history-file ()
  "The history file, or NIL when the environment has turned it off."
  (unless (equal (dotcl:getenv "DOTCL_NO_HISTORY") "1")
    (make-pathname :name "history" :type nil
                   :defaults (dotcl:user-init-file))))

(defun history-file ()
  (if (eq *history-file* :default)
      (default-history-file)
      *history-file*))

;;; A form spans lines, and one entry per line is the only shape that survives
;;; being appended to by two processes and cut short by a crash. So a newline
;;; inside an entry is written as a backslash and an n, and a backslash as two
;;; of them, which is all it takes to make the encoding reversible.

(defun encode-history-entry (text)
  "TEXT as one line, with the characters that would end it spelled out."
  (with-output-to-string (out)
    (loop for ch across text
          do (case ch
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (t (write-char ch out))))))

(defun decode-history-entry (line)
  "The entry LINE stands for."
  (with-output-to-string (out)
    (let ((i 0)
          (n (length line)))
      (loop while (< i n)
            do (let ((ch (char line i)))
                 (cond ((and (char= ch #\\) (< (1+ i) n))
                        (write-char (case (char line (1+ i))
                                      (#\n #\Newline)
                                      (#\r #\Return)
                                      (t (char line (1+ i))))
                                    out)
                        (incf i 2))
                       (t
                        (write-char ch out)
                        (incf i))))))))

(defun history-line-usable-p (line)
  "True for a line that could have been written by ENCODE-HISTORY-ENTRY.
No control character survives encoding, so a line holding one is part of a
damaged file rather than an entry."
  (and (plusp (length line))
       (notany (lambda (ch) (< (char-code ch) 32)) line)))

(defun parse-history-file (text)
  "The entries TEXT holds, oldest first.
An entry and its newline are written in one call, so anything after the last
newline is a write that did not finish and is dropped. So is a line with a
control character in it, which is what a file that was cut short in the middle
of a block tends to leave behind."
  (let ((entries '())
        (start 0))
    (loop for i = (position #\Newline text :start start)
          while i
          do (let ((line (string-right-trim '(#\Return) (subseq text start i))))
               (when (history-line-usable-p line)
                 (push (decode-history-entry line) entries))
               (setf start (1+ i))))
    (nreverse entries)))

(defun history-from-file-text (text max)
  "The *HISTORY* list TEXT stands for: newest first, at most MAX entries."
  (reverse (last (parse-history-file text) max)))

(defun read-history-text (path)
  "The bytes of PATH as text, or NIL when there are none to be had.
One value, not the two IGNORE-ERRORS gives: a caller asking whether there is a
history wants an answer and not a condition to look at."
  (values
   (ignore-errors
    (dotnet:static "System.IO.File" "ReadAllText" (namestring path)))))

(defun history-append (line)
  "Add LINE to the history file, if there is one and it can be written."
  (let ((path (history-file)))
    (when path
      (ignore-errors
       (ensure-directories-exist path)
       (dotnet:static "System.IO.File" "AppendAllText"
                      (namestring path)
                      (concatenate 'string (encode-history-entry line)
                                   (string #\Newline)))))))

(defun history-record (line)
  "Remember LINE for this session and for the next one."
  (when (history-push line)
    (history-append line)))

(defun trim-history-file (path entries)
  "Write ENTRIES, oldest first, over PATH.
Through a temporary file and a rename, so that another session reading it sees
one whole version of the file and never half of two."
  (let ((temp (make-pathname :type "tmp" :defaults path)))
    (ignore-errors
     (dotnet:static "System.IO.File" "WriteAllText" (namestring temp)
                    (with-output-to-string (out)
                      (dolist (entry entries)
                        (write-string (encode-history-entry entry) out)
                        (write-char #\Newline out))))
     (dotnet:static "System.IO.File" "Move" (namestring temp) (namestring path) t))))

(defun load-history ()
  "Read the history file into *HISTORY*, and trim it if it has grown long.
Does nothing the second time it is called.

The count that decides on a trim is of lines rather than of entries, because
the entries are already parsed by then and a line that was dropped as damaged
is still a line taking up room."
  (let ((path (and (not *history-loaded*) (history-file))))
    (when path
      (setf *history-loaded* t)
      (let ((text (read-history-text path)))
        (when text
          (setf *history* (history-from-file-text text *history-max*))
          (when (> (count #\Newline text) *history-file-max*)
            (trim-history-file path (reverse *history*))))))))

;;; -- Searching the history ---------------------------------------------------
;;;
;;; A history that outlives the session is too long to walk with the up arrow,
;;; so Ctrl+R searches it backwards as you type, the way it does everywhere
;;; else. Everything but the keystrokes is arithmetic over a list of strings
;;; and is checked without a terminal.
;;;
;;; The search prompt grows a character at a time, and a prompt that changes
;;; width is exactly what the redraw cannot have: it moves the cursor past the
;;; prompt before erasing, and so never writes it a second time. The way out is
;;; that the search prompt is not a prompt. It goes at the front of the
;;; content, which is erased and written again on every keystroke anyway. The
;;; line's own prompt stays where it is, to the left of it, and RENDER is
;;; untouched -- still four relative movements and no question to the terminal.

(defstruct (isearch (:constructor %make-isearch))
  (query "")
  ;; Index into *HISTORY* of the entry on show, or NIL before anything has
  ;; matched, when the line being edited is on show instead.
  (index nil)
  ;; Where the query sits in that entry. Kept when a search fails, so that a
  ;; keystroke that matches nothing does not move the cursor.
  (match 0)
  (failed nil)
  ;; The state before the last keystroke. Backspace is an undo, which is what
  ;; makes it walk back through the entries a repeated Ctrl+R walked through.
  (previous nil))

(defun isearch-start ()
  "A search with nothing typed into it yet."
  (%make-isearch))

(defun history-search (query start history)
  "Index at or after START of the first entry of HISTORY holding QUERY."
  (loop for entry in (nthcdr start history)
        for i from start
        when (search query entry)
          return i))

(defun isearch-advance (state query start history)
  "STATE after searching HISTORY for QUERY from START.
A search that finds nothing keeps the entry that is on show and says so, which
is why a query can be typed past the point where it matches and then rubbed out
again."
  (let ((found (history-search query start history)))
    (if found
        (%make-isearch :query query :index found
                       :match (search query (nth found history))
                       :failed nil :previous state)
        (%make-isearch :query query :index (isearch-index state)
                       :match (isearch-match state)
                       :failed t :previous state))))

(defun isearch-type (state char history)
  "Another character of the query.
The search starts at the entry already found, so that a longer query keeps the
match where it is for as long as it can."
  (isearch-advance state
                   (concatenate 'string (isearch-query state) (string char))
                   (or (isearch-index state) 0)
                   history))

(defun isearch-again (state history)
  "Ctrl+R once more: the next match further back."
  (isearch-advance state (isearch-query state)
                   (if (isearch-index state) (1+ (isearch-index state)) 0)
                   history))

(defun isearch-undo (state)
  "Backspace: one keystroke back, whichever kind of keystroke it was."
  (or (isearch-previous state) state))

(defun isearch-prefix (state)
  "What stands in front of the entry while the search is on."
  (format nil "~A`~A': "
          (if (isearch-failed state)
              "(failed reverse-i-search)"
              "(reverse-i-search)")
          (isearch-query state)))

(defun isearch-line (state history fallback)
  "The entry the search is showing, or FALLBACK before anything has matched."
  (let ((index (isearch-index state)))
    (if index (nth index history) fallback)))

(defun isearch-point (state history fallback)
  "Where in that entry the cursor belongs."
  (if (isearch-index state)
      (isearch-match state)
      (length (isearch-line state history fallback))))

(defun isearch-content (state history fallback)
  "What RENDER draws while the search is on, and where the cursor goes in it."
  (let ((prefix (isearch-prefix state)))
    (values (concatenate 'string prefix
                         (isearch-line state history fallback))
            (+ (length prefix) (isearch-point state history fallback)))))

(defun isearch-accept (state history fallback)
  "The line the search leaves behind, and the cursor position in it."
  (values (isearch-line state history fallback)
          (isearch-point state history fallback)))

;;; -- Completion --------------------------------------------------------------

(defun common-prefix (strings)
  "Longest string that starts every one of STRINGS."
  (if (null strings)
      ""
      (let ((result (first strings)))
        (dolist (s (rest strings) result)
          (let ((n (min (length result) (length s))))
            (setf result
                  (subseq result 0 (or (mismatch result s :end1 n :end2 n) n))))))))

(defun complete (buf point)
  "Return (new-buf new-point items-to-show span) after tab completion, or NIL.

The completer sees the whole line and the cursor, and says which span its
candidates replace, so completing inside a string literal works the same as
completing a symbol. A unique candidate is inserted; several are extended as far
as they agree and then offered, the way a shell does it. SPAN is (start . end)
in the new buffer: the text a candidate chosen from ITEMS replaces."
  (when *completer*
    (let* ((text (coerce buf 'string))
           (result (funcall *completer* text point)))
      (when result
        (let* ((start (getf result :start))
               (end (getf result :end))
               (items (getf result :items))
               (labels* (mapcar (lambda (i) (getf i :label)) items))
               (typed (subseq text start end))
               (common (common-prefix labels*)))
          (cond
            ((null items) nil)
            ;; Something to insert: one candidate, or a shared prefix longer
            ;; than what is already there.
            ((> (length common) (length typed))
             (let ((new-text (concatenate 'string
                                          (subseq text 0 start)
                                          common
                                          (subseq text end))))
               (list (coerce new-text 'list)
                     (+ start (length common))
                     (when (rest items) items)
                     (cons start (+ start (length common))))))
            ;; Nothing more to insert, but the reader deserves to see what the
            ;; choices are -- with .NET members the signature is the point.
            ((rest items) (list buf point items (cons start end)))
            (t nil)))))))

(defun choose-completion (buf span item)
  "BUF with SPAN, (start . end), replaced by the label of ITEM, and the point
after it."
  (let ((label (coerce (getf item :label) 'list)))
    (values (append (subseq buf 0 (car span)) label (subseq buf (cdr span)))
            (+ (car span) (length label)))))

(defun completion-menu-labels (items)
  "The rows of a completion menu: each label, then its detail, the details
lined up in one column as far as the labels allow."
  (let ((column (min 24 (reduce #'max items
                                :key (lambda (i) (length (getf i :label)))
                                :initial-value 0))))
    (mapcar (lambda (item)
              (let ((label (getf item :label))
                    (detail (getf item :detail)))
                (if detail
                    (format nil "~vA  ~A" column label detail)
                    label)))
            items)))

(defun completion-menu-key (menu-key key-name shift-p)
  "What a key means to the completion menu, given what it means to any menu
(MENU-KEY, from CONSOLE-MENU-KEY) and the name .NET gives it.

TAB takes the marked candidate, as Enter does, and Shift+TAB moves up. A key a
menu has no use for is :PASS: the menu goes away and the key goes on to the
line, so the arrows left and right, Backspace and the rest keep editing without
a key to close the menu first. Ctrl+D closes it rather than ending the input."
  (cond ((equal key-name "Tab") (if shift-p :up :enter))
        ((eq menu-key :backspace) :pass)
        ((eq menu-key :eof) :cancel)
        ;; An Escape with a sequence behind it has been read whole already, so
        ;; it cannot go on to the line; the menu just stays.
        ((and (eq menu-key :ignore) (not (equal key-name "Escape"))) :pass)
        (t menu-key)))

(defparameter *completion-display-limit* 20)

(defun show-completions (items)
  "Print candidates one per line, label then detail."
  (let ((width (max 20 (terminal-width)))
        (shown (min (length items) *completion-display-limit*)))
    (write-str (format nil "~%"))
    (dolist (item (subseq items 0 shown))
      (let* ((label (getf item :label))
             (detail (getf item :detail))
             (line (if detail
                       (format nil "  ~vA  ~A" (min 24 (max 8 (length label)))
                               label detail)
                       (format nil "  ~A" label))))
        (write-str (format nil "~A~%"
                           (if (> (length line) (1- width))
                               (subseq line 0 (1- width))
                               line)))))
    (when (> (length items) shown)
      (write-str (format nil "  ... ~A more~%" (- (length items) shown))))))

;;; -- Multi-line input --------------------------------------------------------
;;;
;;; A form is not a line. Sending every line to the reader as it is typed leaves
;;; the reader holding an unfinished form and the prompt replaced by spaces,
;;; which is a poor place to be: the line above can no longer be edited, and a
;;; paste of twenty lines arrives as twenty separate reads. So the editor keeps
;;; the whole form and decides for itself when it is finished.
;;;
;;; Everything that decides is a function of a string, so all of it can be
;;; checked without a terminal. What the keys do with the answers is the loop's
;;; business and only that.

(defun command-line-p (text)
  "True when TEXT is a comma command rather than a form to read."
  (and (plusp (length text)) (char= (char text 0) #\,)))

(defun scan-brackets (text start end fn)
  "Call FN with the character and the index of every bracket from START to END.

The scan steps over what a reader would not read as a bracket: the inside of a
string literal, the rest of a line after a semicolon, a block comment and the
block comments nested inside it, and the character after a #\\ so that #\\( and
#\\) count as characters and not as brackets.

It is a scan and not the reader, and some spellings are deliberately counted
wrong. A parenthesis inside |a vertical bar symbol| is counted, and so is one
after a single escape outside a string. A string or a block comment still open
at END does not make the text unfinished on its own, because only parentheses
are counted. Each of those needs the reader to tell apart, and the reader
cannot be asked: the question here is whether to call it at all."
  (let ((i start))
    (loop while (< i end)
          do (let ((ch (char text i)))
               (cond
                 ;; A string literal, up to its closing quote. A backslash
                 ;; inside one takes the next character with it, and that is
                 ;; what keeps "\"" from ending where it does not.
                 ((char= ch #\")
                  (incf i)
                  (loop while (< i end)
                        do (let ((c (char text i)))
                             (cond ((char= c #\\) (incf i 2))
                                   ((char= c #\") (incf i) (return))
                                   (t (incf i))))))
                 ;; A semicolon comment, up to the end of its line.
                 ((char= ch #\;)
                  (setf i (or (position #\Newline text :start i :end end) end)))
                 ;; A block comment, and the block comments inside it.
                 ((and (char= ch #\#) (< (1+ i) end)
                       (char= (char text (1+ i)) #\|))
                  (incf i 2)
                  (let ((level 1))
                    (loop while (and (plusp level) (< (1+ i) end))
                          do (cond ((and (char= (char text i) #\#)
                                         (char= (char text (1+ i)) #\|))
                                    (incf level)
                                    (incf i 2))
                                   ((and (char= (char text i) #\|)
                                         (char= (char text (1+ i)) #\#))
                                    (decf level)
                                    (incf i 2))
                                   (t (incf i))))
                    ;; Never closed: the rest of the text is inside it.
                    (when (plusp level) (setf i end))))
                 ;; #\x is one character, whatever x turns out to be. Taking
                 ;; three characters is enough for #\( and #\), and the letters
                 ;; left over from a name like #\Space count as nothing.
                 ((and (char= ch #\#) (< (1+ i) end)
                       (char= (char text (1+ i)) #\\))
                  (incf i 3))
                 ((or (char= ch #\() (char= ch #\)))
                  (funcall fn ch i)
                  (incf i))
                 (t (incf i)))))
    nil))

(defun paren-depth (text &optional (end (length text)))
  "Parentheses still open after the first END characters of TEXT, counted the
way SCAN-BRACKETS sees them. A bracket too many takes the count below zero."
  (let ((depth 0))
    (scan-brackets text 0 end
                   (lambda (ch i)
                     (declare (ignore i))
                     (if (char= ch #\() (incf depth) (decf depth))))
    depth))

(defun open-brackets (text &optional (end (length text)))
  "Indices of the parentheses still open after the first END characters of
TEXT, the one opened last first. A closing bracket with nothing open to close
is passed over."
  (let ((stack '()))
    (scan-brackets text 0 end
                   (lambda (ch i)
                     (if (char= ch #\()
                         (push i stack)
                         (pop stack))))
    stack))

(defun line-start (text index)
  "Index of the first character of the line INDEX is on."
  (let ((newline (position #\Newline text :end index :from-end t)))
    (if newline (1+ newline) 0)))

(defun line-end (text index)
  "Index just past the last character of the line INDEX is on."
  (or (position #\Newline text :start index) (length text)))

(defun leading-indent (text start)
  "How many blanks the line beginning at START opens with."
  (let ((i start)
        (n (length text)))
    (loop while (and (< i n) (member (char text i) '(#\Space #\Tab)))
          do (incf i))
    (- i start)))

(defun next-element-start (text i end)
  "Index of the next element at or after I and before END, or NIL when only
blanks, a comment or a closing bracket are left."
  (loop while (and (< i end) (member (char text i) '(#\Space #\Tab)))
        do (incf i))
  (and (< i end)
       (not (member (char text i) '(#\; #\))))
       i))

(defun element-end (text i end)
  "Index just past the element that starts at I, going no further than END.

An element is a list up to its matching bracket, a string up to its closing
quote, or an atom up to the next blank or bracket, with any quote, backquote,
comma or #' in front of it. A list or a string still open at END runs to END."
  (loop while (and (< i end) (member (char text i) '(#\' #\` #\, #\@)))
        do (incf i))
  (when (and (< (1+ i) end) (char= (char text i) #\#)
             (member (char text (1+ i)) '(#\' #\()))
    (incf i (if (char= (char text (1+ i)) #\') 2 1)))
  (cond
    ((>= i end) end)
    ((char= (char text i) #\()
     (let ((depth 0))
       (scan-brackets text i end
                      (lambda (ch at)
                        (if (char= ch #\()
                            (incf depth)
                            (when (zerop (decf depth))
                              (return-from element-end (1+ at))))))
       end))
    ((char= (char text i) #\")
     (incf i)
     (loop while (< i end)
           do (let ((c (char text i)))
                (cond ((char= c #\\) (incf i 2))
                      ((char= c #\") (return-from element-end (1+ i)))
                      (t (incf i)))))
     end)
    (t
     (loop while (< i end)
           do (let ((c (char text i)))
                (cond ((and (char= c #\#) (< (1+ i) end)
                            (char= (char text (1+ i)) #\\))
                       (incf i 3))
                      ((member c '(#\Space #\Tab #\Newline #\( #\) #\" #\;))
                       (return))
                      (t (incf i)))))
     (min i end))))

(defun auto-indent (text point)
  "Blanks to open the line that a newline typed at POINT begins.

Three rules, and no table of operators, so a macro defined a moment ago is
indented like everything else:

1. When the bracket opened last and still open sits after something else on
   its line, the new line lines up with the second element after that bracket
   (the first argument), or one column past the bracket when its line has no
   second element.
2. When that bracket is the first thing on its line, the new line goes two
   columns in from it: the body of a form.
3. When the line being left closes everything it opens and nothing opened
   before it, the new line keeps its indentation.

The open brackets are counted from the start of the form, so a line inherits
what the lines above it left open. Columns are counted within the form, where
the first line starts at column zero, which is how the lines after it are
drawn under it. When the line being left closes the whole form the answer is
zero.

It costs nothing to be wrong about: the result is whitespace, the reader does
not care how much of it there is, and it can be edited away."
  (let* ((start (line-start text point))
         (before (open-brackets text start))
         (after (open-brackets text point)))
    (cond
      ((equal before after) (leading-indent text start))
      ((null after) 0)
      (t
       (let* ((open (first after))
              (open-line (line-start text open))
              (column (- open open-line)))
         (if (= column (leading-indent text open-line))
             (+ column 2)
             (let* ((limit (min point (line-end text open)))
                    (first-element (next-element-start text (1+ open) limit))
                    (second-element
                      (and first-element
                           (next-element-start
                            text (element-end text first-element limit) limit))))
               (if second-element
                   (- second-element open-line)
                   (1+ column)))))))))

(defun previous-row-point (text point)
  "Where the cursor lands moving a row up, or NIL when it is on the first row.
The column is kept where the row above is long enough to have one, and is the
end of that row where it is not."
  (let ((start (line-start text point)))
    (unless (zerop start)
      (let ((above (line-start text (1- start))))
        (+ above (min (- point start) (- (1- start) above)))))))

(defun next-row-point (text point)
  "Where the cursor lands moving a row down, or NIL when it is on the last row."
  (let ((end (line-end text point)))
    (when (< end (length text))
      (let* ((below (1+ end))
             (below-end (line-end text below)))
        (+ below (min (- point (line-start text point))
                      (- below-end below)))))))

(defun enter-action (text &key (commands t) pasting)
  "What Enter means for TEXT: :SUBMIT or :OPEN-LINE.

Inside a bracketed paste a newline always opens a line. The text is arriving
from somewhere else all at once, and a line of it that happens to balance is
not the reader being asked for anything.

A line that opens with a comma is a command and goes at once. Counting
parentheses in one would leave ,help ( unfinished forever, with the reader
waiting for a bracket that the command would never have read. COMMANDS is false
on a continuation line, where a leading comma is an unquote and not a command.

Otherwise an unclosed parenthesis opens a line and everything else is sent,
a stray closing bracket included: the reader reports that, and a line that
cannot be sent is worse than a line that is reported."
  (cond (pasting :open-line)
        ((and commands (command-line-p text)) :submit)
        ((plusp (paren-depth text)) :open-line)
        (t :submit)))

;;; -- Matching bracket --------------------------------------------------------
;;;
;;; With the cursor just past a closing bracket, the bracket it closes is shown
;;; painted, on its own line or on a line above. Which bracket that is comes
;;; from the same scan that decides whether Enter sends the form, so a bracket
;;; in a string, in a comment or in #\( is no more a bracket here than it is
;;; there. The whole input is redrawn on every keystroke, so showing it is only
;;; a matter of what the redraw paints; see RENDER.

(defun matching-open (text point)
  "Index of the opening bracket that the closing bracket just before POINT
closes, or NIL when the character before POINT is not a closing bracket as
SCAN-BRACKETS sees it, or closes nothing."
  (when (and (< 0 point) (<= point (length text))
             (char= (char text (1- point)) #\)))
    (let ((stack '())
          (close (1- point))
          (match nil))
      (scan-brackets text 0 point
                     (lambda (ch i)
                       (if (char= ch #\()
                           (push i stack)
                           (let ((open (pop stack)))
                             (when (= i close) (setf match open))))))
      match)))

(defun paint-prefix (role)
  "The escape sequence that starts text painted as ROLE, or NIL when that text
would not be painted (colour off, or a role with no colour). Painted text ends
with a reset, so the reset is not asked for."
  (let* ((probe "x")
         (painted (paint role probe)))
    (unless (string= painted probe)
      (let ((at (search probe painted)))
        (and at (plusp at) (subseq painted 0 at))))))

(defun match-spans (text point)
  "The painted spans (see RENDER) that show the bracket matching the one before
POINT: one span of one character, or none."
  (let ((open (matching-open text point))
        (on (paint-prefix :match)))
    (and open on (list (list open (1+ open) on)))))

;;; -- Syntax highlighting -----------------------------------------------------
;;;
;;; Strings, comments and keywords in the input are painted as they are typed,
;;; by the same redraw and with the same spans as the matching bracket. The
;;; scan follows SCAN-BRACKETS: what it counts as a string or a comment is what
;;; the bracket count steps over, so the colours never disagree with what Enter
;;; does. It is a scan, not the reader, and is wrong in the same places (a
;;; semicolon inside |a vertical bar symbol| starts a comment here).

(defvar *syntax-highlight* t
  "True to paint strings, comments and keywords in the input line. Nothing is
painted either way when the REPL does not paint (--color=never, NO_COLOR,
TERM=dumb, or output that is not a terminal).")

(defun syntax-delimiter-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page
               #\( #\) #\' #\` #\, #\" #\;)))

(defun syntax-ranges (text &optional (end (length text)))
  "The strings, comments and keywords in the first END characters of TEXT, as a
list of (START END KIND) in order, KIND being :STRING, :COMMENT or :KEYWORD. A
string or a block comment still open at END runs to END."
  (let ((ranges '())
        (i 0))
    (loop while (< i end)
          do (let ((ch (char text i))
                   (start i))
               (cond
                 ((char= ch #\")
                  (incf i)
                  (loop while (< i end)
                        do (let ((c (char text i)))
                             (cond ((char= c #\\) (incf i 2))
                                   ((char= c #\") (incf i) (return))
                                   (t (incf i)))))
                  (push (list start (min i end) :string) ranges))
                 ((char= ch #\;)
                  (setf i (or (position #\Newline text :start i :end end) end))
                  (push (list start i :comment) ranges))
                 ((and (char= ch #\#) (< (1+ i) end)
                       (char= (char text (1+ i)) #\|))
                  (incf i 2)
                  (let ((level 1))
                    (loop while (and (plusp level) (< (1+ i) end))
                          do (cond ((and (char= (char text i) #\#)
                                         (char= (char text (1+ i)) #\|))
                                    (incf level)
                                    (incf i 2))
                                   ((and (char= (char text i) #\|)
                                         (char= (char text (1+ i)) #\#))
                                    (decf level)
                                    (incf i 2))
                                   (t (incf i))))
                    (when (plusp level) (setf i end)))
                  (push (list start i :comment) ranges))
                 ((and (char= ch #\#) (< (1+ i) end)
                       (char= (char text (1+ i)) #\\))
                  (setf i (min end (+ i 3))))
                 ;; A colon that starts a token starts a keyword. One after a
                 ;; package name or after #, as in #:foo, does not.
                 ((and (char= ch #\:)
                       (or (zerop i) (syntax-delimiter-p (char text (1- i)))))
                  (incf i)
                  (loop while (and (< i end)
                                   (not (syntax-delimiter-p (char text i))))
                        do (incf i))
                  (push (list start i :keyword) ranges))
                 (t (incf i)))))
    (nreverse ranges)))

(defun syntax-spans (text)
  "The painted spans (see RENDER) for the strings, comments and keywords of
TEXT, or none when the REPL does not paint."
  (let ((roles (list (cons :string (paint-prefix :string))
                     (cons :comment (paint-prefix :comment))
                     (cons :keyword (paint-prefix :keyword)))))
    (loop for (start end kind) in (syntax-ranges text)
          for on = (cdr (assoc kind roles))
          when (and on (< start end))
            collect (list start end on))))

(defun input-spans (text point &key (syntax *syntax-highlight*) (match t))
  "Everything the redraw paints in TEXT with the cursor at POINT: the syntax
when SYNTAX, the matching bracket when MATCH. The matching bracket is a real
bracket, never inside a string, a comment or a keyword, so the two do not
overlap."
  (let ((spans (append (and syntax (syntax-spans text))
                       (and match (match-spans text point)))))
    (sort spans #'< :key #'first)))

;;; -- Words -------------------------------------------------------------------
;;;
;;; Two notions of a word, both of them readline's, because both are already in
;;; people's fingers.
;;;
;;; Alt+B, Alt+F and Alt+D step over tokens, and a token here is a Lisp token:
;;; *STANDARD-OUTPUT* is one word and not three. A motion that stops inside a
;;; symbol name would be of little use in a Lisp, where the hyphen is the space
;;; of ordinary names.
;;;
;;; Ctrl+W is readline's unix-word-rubout and takes everything back to the last
;;; blank, brackets and all. It is the one people use to take back what they
;;; have just typed, and what they have just typed usually ends in brackets.

(defparameter *whitespace* '(#\Space #\Tab #\Return #\Newline #\Page))

(defun whitespace-char-p (ch)
  (and (member ch *whitespace*) t))

(defparameter *token-delimiters* '(#\( #\) #\' #\` #\, #\" #\;)
  "Characters that end a token without being part of one.")

(defun token-char-p (ch)
  (and (not (whitespace-char-p ch))
       (not (member ch *token-delimiters*))))

(defun word-backward (text point)
  "The start of the token before POINT, or POINT when there is none."
  (let ((i point))
    (loop while (and (> i 0) (not (token-char-p (char text (1- i)))))
          do (decf i))
    (loop while (and (> i 0) (token-char-p (char text (1- i))))
          do (decf i))
    i))

(defun word-forward (text point)
  "The end of the token after POINT, or POINT when there is none."
  (let ((i point)
        (n (length text)))
    (loop while (and (< i n) (not (token-char-p (char text i))))
          do (incf i))
    (loop while (and (< i n) (token-char-p (char text i)))
          do (incf i))
    i))

(defun whitespace-backward (text point)
  "The start of the run of non-blank text before POINT."
  (let ((i point))
    (loop while (and (> i 0) (whitespace-char-p (char text (1- i))))
          do (decf i))
    (loop while (and (> i 0) (not (whitespace-char-p (char text (1- i)))))
          do (decf i))
    i))

;;; -- Bracketed paste ---------------------------------------------------------
;;;
;;; A terminal in bracketed paste mode wraps pasted text in two sequences, so
;;; that a program can tell text that arrived all at once from text that was
;;; typed. Without it a pasted form is indistinguishable from someone typing
;;; very fast, and the editor has to guess at every newline in it.
;;;
;;; Turning the mode on is only safe for a program that reads the delimiters
;;; back, because a terminal that has been asked for them will send them: the
;;; [200~ that has been seen at a dotcl prompt is a delimiter nobody read.
;;; Console.ReadKey hands them over one character at a time -- Escape, then [,
;;; then 2, 0, 0, then ~ -- on Windows and on Unix alike, which is what makes
;;; reading them possible here. Unix is the one worth saying: .NET matches key
;;; sequences against terminfo there, and a sequence it cannot match comes
;;; through unchanged rather than being swallowed.

(defparameter *bracketed-paste-on* (format nil "~C[?2004h" #\Escape))
(defparameter *bracketed-paste-off* (format nil "~C[?2004l" #\Escape))

(defun classify-csi (parameters final)
  "What a control sequence means here, from its parameters and its final byte.

Returns :PASTE-START, :PASTE-END, or :IGNORED for everything else. A sequence
this editor does not know is a key it does not bind, and dropping it is the
point: leaving its bytes in the line is the failure this is here to avoid."
  (cond ((and (char= final #\~) (string= parameters "200")) :paste-start)
        ((and (char= final #\~) (string= parameters "201")) :paste-end)
        (t :ignored)))

(defparameter *escape-sequence-wait* 30
  "Milliseconds to keep looking for the rest of a control sequence.")

(defparameter *escape-sequence-limit* 16
  "How many characters of one control sequence to read before giving up.")

(defun wait-for-key ()
  "True once a key is waiting, NIL if none turns up promptly.
The rest of a control sequence follows its Escape with no gap in between, so a
short wait is what tells a sequence apart from the Escape key pressed on its
own. The wait is short enough not to be felt and long enough to survive a
sequence that arrives split across two reads of a slow link."
  (let ((left *escape-sequence-wait*))
    (loop
      (when (key-waiting-p) (return t))
      (when (<= left 0) (return nil))
      (dotnet:static "System.Threading.Thread" "Sleep" 5)
      (decf left 5))))

(defun read-escape-sequence ()
  "Read what follows an Escape that the key reader handed over unparsed.
Returns :ALT-ENTER, :PASTE-START, :PASTE-END, (:ALT . character) for Alt and an
ordinary key, or :IGNORED."
  (if (not (wait-for-key))
      ;; Escape on its own.
      :ignored
      (let* ((ki (console-read-key))
             (ch (key-char ki)))
        (cond
          ;; Alt and a key is Escape and then the key on a Unix terminal, and
          ;; Alt+Enter is the one this editor binds.
          ((console-key= ki "Enter") :alt-enter)
          ((and (characterp ch) (char= ch #\[))
           (let ((parameters (make-string-output-stream)))
             (dotimes (i *escape-sequence-limit* :ignored)
               (declare (ignorable i))
               (unless (wait-for-key) (return :ignored))
               (let* ((k (console-read-key))
                      (c (key-char k)))
                 (cond ((not (characterp c)) (return :ignored))
                       ;; Parameter and intermediate bytes run to #x3F; the
                       ;; first byte outside that range ends the sequence.
                       ((<= #x20 (char-code c) #x3F) (write-char c parameters))
                       (t (return (classify-csi
                                   (get-output-stream-string parameters)
                                   c))))))))
          ;; Escape and an ordinary character is how a Unix terminal sends Alt
          ;; and that key. Which of them mean anything is the read loop's
          ;; business; this only says which key it was.
          ((and (characterp ch) (graphic-char-p ch)) (cons :alt ch))
          (t :ignored)))))

;;; -- Line modes --------------------------------------------------------------
;;;
;;; A character typed at the start of an empty line can switch the prompt to
;;; another mode for that one line, the way Julia's REPL does it: the character
;;; is not put in the line, the prompt changes, the line is sent to the mode
;;; when Enter is pressed, and the next prompt is the Lisp one again. Backspace
;;; on the empty line switches back without sending anything.
;;;
;;; The character has to be one that cannot start anything worth typing at an
;;; empty Lisp prompt. A semicolon only starts a comment, so taking it costs
;;; nothing. Julia's ? and ] are constituents of symbol names in CL and are not
;;; taken; ,doc answers what ? would. A paste never switches: pasted Lisp often
;;; opens with a ;;; comment, and that is text arriving, not a key pressed.

(defstruct (line-mode (:constructor %make-line-mode))
  char      ; the character that switches to it on an empty line
  name      ; a keyword naming it
  label     ; the prompt, without the blank after it
  role      ; how the prompt is painted
  summary   ; what ,help says about it
  run       ; function of the line sent in this mode
  record    ; function of the line giving its history entry, or NIL for none
  menu      ; function of the line giving the choices to show under it, or NIL
  choose)   ; function of the line, a choice and :ENTER or :TAB; see below

(defvar *line-modes* '()
  "Every line mode, in the order they were defined.")

(defun define-line-mode (char name label role summary run
                         &key record menu choose)
  "Add a line mode, in place of any earlier one on the same character.

RUN is called with the line sent in the mode; a value of :QUIT ends the REPL.
RECORD, when given, turns the line into the entry the history keeps; without
it the mode's lines are kept out of the history.

MENU, when given, is called with the line as it is being typed and returns the
choices to show under it: a list of plists with :LABEL and :DETAIL, or NIL for
no menu. A mode with a menu needs a terminal that can draw one and is not
entered where it cannot (see MENU-USABLE-P). CHOOSE is called with the line, the
marked choice and :ENTER or :TAB, and returns (values :SUBMIT line) to send a
line or (values :EDIT line) to put a line in the buffer and go on typing."
  (let ((mode (%make-line-mode :char char :name name :label label :role role
                               :summary summary :run run :record record
                               :menu menu :choose choose)))
    (setf *line-modes*
          (append (remove char *line-modes* :key #'line-mode-char)
                  (list mode)))
    mode))

(defun line-mode-for-key (ch empty pasting commands &optional (menus t))
  "The mode CH switches to, or NIL when it is an ordinary character.
Only on an EMPTY line at the primary prompt (COMMANDS true; a continuation line
is inside a form) and never inside a paste. A mode with a menu only where MENUS
says one can be drawn: elsewhere the character goes into the line as it did
before there were modes."
  (let ((mode (and empty (not pasting) commands (characterp ch)
                   (find ch *line-modes* :key #'line-mode-char))))
    (and mode
         (or menus (null (line-mode-menu mode)))
         mode)))

(defun mode-prompt-string (mode)
  "The prompt MODE shows, painted as the REPL paints prompts."
  (concatenate 'string (paint (line-mode-role mode) (line-mode-label mode)) " "))

;;; -- The shell mode ----------------------------------------------------------
;;;
;;; The shell is the user's own where there is a convention for naming it:
;;; $SHELL on Unix, %ComSpec% on Windows, which is cmd.exe unless something has
;;; changed it. PowerShell is not chosen on Windows because it is not the
;;; system's command interpreter: pwsh may not be installed, and powershell.exe
;;; takes seconds to start for a one-line command.

(defun nonempty (string)
  (and string (plusp (length string)) string))

(defun shell-invocation (line &key (windows (and (member :windows *features*) t))
                                   (shell (dotcl:getenv "SHELL"))
                                   (comspec (dotcl:getenv "ComSpec")))
  "The program to run LINE with and its arguments: a list of them on Unix, one
command line on Windows, where cmd parses its own. /s with the whole line in
quotes keeps the quotes inside the line as they were typed, and /d leaves out
the AutoRun commands, which would otherwise run before every line."
  (if windows
      (values (or (nonempty comspec) "cmd.exe")
              (format nil "/d /s /c \"~A\"" line))
      (values (or (nonempty shell) "/bin/sh")
              (list "-c" line))))

(defun shell-line-ignored-p (line)
  "True for a line the shell mode does not run: a blank one, or one that starts
with a semicolon. The second is a pasted ;;; comment on a terminal that does
not bracket pastes, whose first semicolon switched the mode."
  (or (zerop (length (trim-whitespace line)))
      (char= (char line 0) #\;)))

(defun run-shell (program arguments)
  "Run PROGRAM on the REPL's own terminal and wait for it. ARGUMENTS is a list,
or one command line. Returns the exit status, or NIL when it could not start;
a status that is not zero, and a failure to start, are said on *ERROR-OUTPUT*."
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (handler-case
      (let ((info (dotnet:new "System.Diagnostics.ProcessStartInfo" program)))
        (setf (dotnet:invoke info "UseShellExecute") nil)
        (if (stringp arguments)
            (setf (dotnet:invoke info "Arguments") arguments)
            (let ((list (dotnet:invoke info "ArgumentList")))
              (dolist (argument arguments)
                (dotnet:invoke list "Add" argument))))
        (let ((process (dotnet:static "System.Diagnostics.Process" "Start" info)))
          (dotnet:invoke process "WaitForExit")
          ;; Ctrl+C while the command ran reached the command too, which is
          ;; who it was meant for. Delivered here as well, it would come out
          ;; as a second ^C at the next prompt.
          (ignore-errors
           (dotnet:static "DotCL.ConditionSystem" "DiscardInterrupt"))
          (let ((status (dotnet:invoke process "ExitCode")))
            (dotnet:invoke process "Dispose")
            (unless (eql status 0)
              (format *error-output* "~A~%"
                      (paint :error (format nil "; exit status ~A" status)
                             *error-output*)))
            status)))
    (error (condition)
      (format *error-output* "~A~%"
              (paint :error (format nil "; cannot run ~A: ~A" program condition)
                     *error-output*))
      nil)))

(defun run-shell-line (line)
  "Run LINE with the shell, unless it is a line the shell mode ignores.
Returns the exit status, or NIL when nothing ran."
  (unless (shell-line-ignored-p line)
    (multiple-value-bind (program arguments) (shell-invocation line)
      (run-shell program arguments))))

(define-line-mode #\; :shell "sh>" :shell
  "Run the line with the shell ($SHELL or /bin/sh; %ComSpec% or cmd.exe on Windows)."
  #'run-shell-line)

;;; -- Main readline -----------------------------------------------------------

;;; Refuse to start rather than fail half way through an edit.
;;;
;;; The caller in the REPL treats NIL as end of file, so a line editor that
;;; cannot run must signal instead of returning: the caller catches the error,
;;; says so, and drops back to plain line input for the rest of the session.
;;; Returning NIL here would end the REPL without a word.
;;;
;;; Asking whether a key is waiting is the probe because it is the same
;;; question the read loop asks, it consumes nothing, and on input that is not
;;; a console it throws with a message that already explains itself. The read
;;; loop cannot be the one to discover this: it treats a failed key read as an
;;; interruption and answers NIL, which is the silent ending this avoids.
(defun ensure-console ()
  "Signal an error unless there is a console to edit on."
  (dotnet:static "System.Console" "get_KeyAvailable")
  ;; An unusable output handle throws out of the width query. TERMINAL-WIDTH
  ;; falls back to a default instead, so the console is asked directly here.
  (dotnet:static "System.Console" "get_WindowWidth")
  t)

(defun read-line-edited (prompt &optional (commands t) (initial ""))
  "Read a form with editing. Returns the string, which may have newlines in it,
   or NIL on EOF (Ctrl+D) or thread interruption. Signals an error when there
   is no console.

   Enter sends the text when its parentheses are closed and opens an indented
   line when they are not, so a form typed or pasted across several lines
   reaches the reader whole. Alt+Enter sends whatever is there and Ctrl+J
   always opens a line. COMMANDS is false on a continuation line, where a
   leading comma is an unquote rather than a command.

   A second value is the line mode the line was typed in, NIL for Lisp.

   INITIAL is text already typed, with the cursor after it: the debugger passes
   the key that closed its restart menu.

   READLINE, further down, is the entry point the read loop calls: it is this
   function plus the comma commands."
  (ensure-console)
  (write-str *bracketed-paste-on*)
  (write-str prompt)
  (let ((prompt-width (prompt-display-width prompt))
        (buf '())           ; list of chars, left-to-right, newlines and all
        (point 0)           ; insertion point (0 = before first char)
        (cursor-row 0)      ; rows below the prompt row the cursor rests on
        (hist-idx -1)       ; -1 = current input
        (saved-buf '())     ; saved buf when browsing history
        (pasting nil)       ; between the two bracketed paste delimiters
        (pasted-return nil) ; the character just pasted was a carriage return
        (too-tall nil)      ; the input outgrew the window; see REFRESH
        (searching nil)     ; the Ctrl+R search, while one is on
        (pending-key nil)   ; a key read once and put back; see the search
        (mode nil)          ; the line mode, or NIL for Lisp
        (menus (menu-usable-p)) ; a menu can be drawn on this terminal
        (menu-text :none)   ; the line MENU-ITEMS were asked for
        (menu-items '())    ; the choices a mode's menu shows for it
        (menu-sel nil)      ; the marked choice, or NIL for none yet
        (menu-top 0)        ; the first choice in view
        (menu-drawn nil)    ; a mode's menu is on the screen under the line
        (ctrl-c-was nil))   ; TreatControlCAsInput before the edit began

    ;; Every edit goes through one redraw, arrow keys included. That writes more
    ;; than nudging the cursor by hand would, but an input line is small and a
    ;; single code path cannot disagree with another about where the cursor is.
    (labels ((text () (coerce buf 'string))

             (current-prompt ()
               (if mode (mode-prompt-string mode) prompt))

             (switch-mode (new)
               ;; Only ever on an empty line, so the input area is the prompt's
               ;; own row and nothing below it: erase it and write the other
               ;; prompt, whose width the redraw then counts from.
               (write-str (ansi-up cursor-row))
               (write-str *ansi-column-1*)
               (write-str *ansi-erase-below*)
               (setf mode new
                     menu-text :none
                     menu-drawn nil)
               (let ((shown (current-prompt)))
                 (write-str shown)
                 (setf prompt-width (prompt-display-width shown)
                       cursor-row 0
                       too-tall nil))
               (when (mode-menu-p)
                 (draw-mode-menu "" 0)))

             (mode-menu-p ()
               (and mode (line-mode-menu mode) t))

             (draw-mode-menu (content at)
               ;; The choices of a mode with a menu, under its line, asked for
               ;; again only when the line has changed; a change also puts the
               ;; mark back on the first choice, or on none while the line is
               ;; empty. Drawn only while the line is on one row: the anchor is
               ;; its end, and the cursor goes back to AT on the same row.
               (unless (equal content menu-text)
                 (setf menu-text content
                       menu-items (funcall (line-mode-menu mode) content)
                       menu-sel (and menu-items (plusp (length content)) 0)
                       menu-top 0))
               (let* ((width (terminal-width))
                      (end-col (layout-column prompt-width content width)))
                 (when (and menu-items (not too-tall) (< end-col width))
                   (multiple-value-bind (out top)
                       (mode-menu-string (completion-menu-labels menu-items)
                                         menu-sel menu-top width
                                         (terminal-height) end-col
                                         (layout-column prompt-width content
                                                        width at))
                     (setf menu-top top
                           menu-drawn t)
                     (write-str out)))))

             (menu-shown-p ()
               (and (mode-menu-p) menu-items (equal menu-text (text))))

             (mode-choose (how)
               ;; Take the marked choice. True when the line is to be sent,
               ;; with the buffer holding it; false when it has been put in
               ;; the buffer to go on typing.
               (multiple-value-bind (action line)
                   (funcall (line-mode-choose mode) (text)
                            (nth menu-sel menu-items) how)
                 (setf buf (coerce line 'list)
                       point (length buf))
                 (or (eq action :submit)
                     (progn (refresh) nil))))

             (draw (content at &optional spans)
               ;; True when the screen was redrawn.
               ;;
               ;; Once the input spans as many rows as the window has, its
               ;; first row has scrolled off the top, and ESC[nA stops at the
               ;; top of the window rather than reaching it: a redraw from
               ;; there would rewrite the wrong rows. There is no way to find
               ;; out where the prompt went without asking the terminal, and
               ;; asking is what this editor does not do. So it stops redrawing
               ;; and lets what is typed echo where it is typed. The buffer
               ;; stays right even where the screen no longer shows it, and a
               ;; form that long is one to keep in a file.
               (multiple-value-bind (out row rows)
                   (render prompt-width content at (terminal-width) cursor-row
                           spans)
                 (cond ((or too-tall (>= rows (terminal-height)))
                        (setf too-tall t)
                        nil)
                       (t (write-str out)
                          (setf cursor-row row)
                          t))))

             (refresh (&optional (at point) (show-match t) (menu t))
               ;; SHOW-MATCH false leaves the matching bracket unpainted: the
               ;; last redraw of a line being sent is what stays in the
               ;; scrollback, and the cursor has left it by then. A line in
               ;; another mode is not Lisp and is not painted at all. MENU
               ;; false leaves a mode's menu off, for a line that is leaving.
               (let ((content (text)))
                 (let ((drawn (draw content at
                                    (and (null mode)
                                         (input-spans content at
                                                      :match show-match)))))
                   ;; A redraw erases everything under the line.
                   (when drawn (setf menu-drawn nil))
                   (when (and menu (mode-menu-p))
                     (draw-mode-menu content at))
                   drawn)))

             ;; The search draws the same way anything else does, with its
             ;; prompt at the front of the content rather than in front of it.
             (search-refresh ()
               (multiple-value-bind (content at)
                   (isearch-content searching *history* (text))
                 (draw content at)))

             (accept-search ()
               ;; Leave the search with what it found in the buffer, and with
               ;; the up arrow carrying on from where it stopped.
               (let ((index (isearch-index searching)))
                 (multiple-value-bind (line at)
                     (isearch-accept searching *history* (text))
                   (when index
                     (when (= hist-idx -1)
                       (setf saved-buf buf))
                     (setf hist-idx index))
                   (setf buf (coerce line 'list)
                         point at
                         searching nil)
                   (refresh))))

             (echo (string)
               ;; Put STRING on the screen where it is being typed, for when
               ;; there is no redraw to put it there.
               (loop for ch across string
                     do (cond ((char= ch #\Newline)
                               (write-ch #\Return)
                               (write-ch #\Newline)
                               (write-str
                                (make-string prompt-width
                                             :initial-element #\Space)))
                              (t (write-ch ch)))))

             (insert (string)
               (setf buf (append (subseq buf 0 point)
                                 (coerce string 'list)
                                 (subseq buf point)))
               (incf point (length string))
               (unless (refresh) (echo string)))

             (open-line ()
               ;; The indentation is real text in the buffer: it goes to the
               ;; reader and into the history, which is what makes a form that
               ;; comes back out of the history look the way it did going in.
               ;; Pasted text brings its own.
               (let ((blanks (if pasting 0 (auto-indent (text) point))))
                 (insert (concatenate 'string
                                      (string #\Newline)
                                      (make-string blanks
                                                   :initial-element #\Space)))))

             (submit ()
               ;; Put the cursor past the last character first, so the newline
               ;; starts below the whole form and not in the middle of it.
               ;; A line typed in another mode is not a form, and is kept out
               ;; of the history the up arrow walks.
               (refresh (length buf) nil nil)
               (write-str (format nil "~%"))
               (let* ((line (text))
                      (entry (cond ((null mode) line)
                                   ((line-mode-record mode)
                                    (funcall (line-mode-record mode) line)))))
                 (when entry (history-record entry))
                 (values line mode)))

             (recall (index)
               (setf hist-idx index)
               (setf buf (coerce (nth index *history*) 'list))
               (setf point (length buf))
               (refresh))

             (kill (from to)
               (setf buf (append (subseq buf 0 from) (subseq buf to))
                     point from)
               (refresh))

             (offer-completions (items span)
               ;; The candidates as a menu under the input, which is where the
               ;; menu is anchored: at the end of the last row, so the menu and
               ;; the erase that takes it away never touch the text. True when
               ;; the menu was offered; false, with nothing drawn, where it
               ;; cannot be (not a terminal, or no room under the input), and
               ;; the candidates are then listed as they were before.
               (when (and (menu-usable-p) (refresh (length buf)))
                 (let ((room (- (terminal-height) cursor-row))
                       (width (terminal-width)))
                   (when (>= room 3)
                     (let* ((anchor (mod (layout-column prompt-width (text) width
                                                        (length buf) prompt-width)
                                         width))
                            (last-ki nil)
                            (read-key
                              (lambda ()
                                (let ((ki (console-read-key-interruptable)))
                                  (setf last-ki ki)
                                  (if ki
                                      (completion-menu-key (console-menu-key ki)
                                                           (key-name ki)
                                                           (key-shift-p ki))
                                      :eof)))))
                       (multiple-value-bind (action value)
                           (run-menu (completion-menu-labels items)
                                     :anchor-col anchor :read-key read-key
                                     :width width :height room)
                         (when (eq action :choose)
                           (multiple-value-setq (buf point)
                             (choose-completion buf span (nth value items))))
                         (refresh)
                         ;; A key the menu did not take goes on to the line.
                         (when (eq action :other)
                           (setf pending-key last-ki))
                         t))))))

             ;; Alt and a letter, however it reached here: a Unix terminal
             ;; sends an Escape first, and Windows reports the modifier.
             (alt-key (ch)
               (case (char-downcase ch)
                 (#\b (setf point (word-backward (text) point))
                      (refresh))
                 (#\f (setf point (word-forward (text) point))
                      (refresh))
                 (#\d (kill point (word-forward (text) point))))))

      (when (plusp (length initial))
        (insert initial))
      (unwind-protect
           (progn
            (setf ctrl-c-was (ctrl-c-as-input t))
            (loop
             ;; Fetch next key in an interruptable manner, unless the search
             ;; read one that turned out not to be for it and put it back.
             (let* ((ki (or pending-key (console-read-key-interruptable)))
                    (ch (when ki (key-char ki))))
               (setf pending-key nil)
               ;; If console-read-key-interruptable returned nil, the thread was interrupted
               ;; (e.g., a ThreadInterruptedException was trapped). Break the loop and return nil.
               (unless ki
                 (return nil))

               (cond
                 ;; While the search is on, the keys mean something else.
                 ;; Anything it does not use ends it, keeping what it found,
                 ;; and is then read again by the ordinary bindings: a mode
                 ;; you have to know your way out of is not worth having.
                 (searching
                  (cond
                    ((and (console-key= ki "R") (key-ctrl-p ki))
                     (setf searching (isearch-again searching *history*))
                     (search-refresh))

                    ((or (console-key= ki "Backspace")
                         (and (console-key= ki "H") (key-ctrl-p ki)))
                     (setf searching (isearch-undo searching))
                     (search-refresh))

                    ;; Ctrl+G: leave the line as it was before the search.
                    ((and (console-key= ki "G") (key-ctrl-p ki))
                     (setf searching nil)
                     (refresh))

                    ;; Enter sends what was found, which is what it does in
                    ;; every other reverse search. The whole entry is on the
                    ;; screen while the search is on, so nothing is sent
                    ;; unseen.
                    ((console-key= ki "Enter")
                     (accept-search)
                     (return (submit)))

                    ((and (characterp ch) (graphic-char-p ch))
                     (setf searching (isearch-type searching ch *history*))
                     (search-refresh))

                    (t
                     (accept-search)
                     (setf pending-key ki))))

                 ;; Escape introduces a sequence the key reader did not
                 ;; recognise. The paste delimiters arrive this way, and so do
                 ;; Alt+Enter and the Alt words on a Unix terminal.
                 ((console-key= ki "Escape")
                  (let ((sequence (read-escape-sequence)))
                    (cond ((eq sequence :alt-enter) (return (submit)))
                          ((eq sequence :paste-start)
                           (setf pasting t pasted-return nil))
                          ((eq sequence :paste-end)
                           (setf pasting nil pasted-return nil)
                           (refresh))
                          ((consp sequence) (alt-key (cdr sequence))))))

                 ;; While a paste is in flight nothing is being typed: no
                 ;; command, no completion, and no decision to make about a
                 ;; newline.
                 (pasting
                  (cond
                    ((not (characterp ch)) (setf pasted-return nil))
                    ((char= ch #\Return)
                     (setf pasted-return t)
                     (open-line))
                    ;; The other half of a carriage return and line feed pair,
                    ;; already counted as the line it ended.
                    ((char= ch #\Newline)
                     (if pasted-return
                         (setf pasted-return nil)
                         (open-line)))
                    (t
                     (setf pasted-return nil)
                     (cond ((graphic-char-p ch) (insert (string ch)))
                           ;; A tab in pasted source is indentation. One space
                           ;; keeps it whitespace without handing the column
                           ;; arithmetic a tab stop to keep track of.
                           ((char= ch #\Tab) (insert " "))))))

                 ;; A mode character on an empty line switches the prompt
                 ;; rather than going into the line.
                 ((and (null mode)
                       (not (key-ctrl-p ki))
                       (not (key-alt-p ki))
                       (line-mode-for-key ch (null buf) pasting commands menus))
                  (switch-mode (line-mode-for-key ch (null buf) pasting commands
                                                  menus)))

                 ;; A mode's menu: the arrows move the mark, TAB takes the
                 ;; marked choice into the line (or, with none marked yet, the
                 ;; only one, or marks the first), and Enter takes it too.
                 ((and (menu-shown-p)
                       (or (console-key= ki "UpArrow")
                           (and (console-key= ki "P") (key-ctrl-p ki))
                           (and (console-key= ki "Tab") (key-shift-p ki))))
                  (setf menu-sel (mode-menu-select :up menu-sel (length menu-items)))
                  (refresh))
                 ((and (menu-shown-p)
                       (or (console-key= ki "DownArrow")
                           (and (console-key= ki "N") (key-ctrl-p ki))))
                  (setf menu-sel (mode-menu-select :down menu-sel (length menu-items)))
                  (refresh))
                 ((and (menu-shown-p) (console-key= ki "Tab"))
                  (cond (menu-sel
                         (when (mode-choose :tab) (return (submit))))
                        ((null (rest menu-items))
                         (setf menu-sel 0)
                         (when (mode-choose :tab) (return (submit))))
                        (t (setf menu-sel 0)
                           (refresh))))
                 ((and (menu-shown-p) menu-sel (console-key= ki "Enter")
                       (not (key-alt-p ki)))
                  (when (mode-choose :enter) (return (submit))))

                 ;; In a mode, Enter sends the line whatever brackets are in
                 ;; it, and Backspace on the empty line goes back to Lisp.
                 ((and mode (console-key= ki "Enter"))
                  (return (submit)))
                 ((and mode (null buf)
                       (or (console-key= ki "Backspace")
                           (and (console-key= ki "H") (key-ctrl-p ki))))
                  (switch-mode nil))
                 ;; History, its search and completion are Lisp's.
                 ((and mode (or (console-key= ki "UpArrow")
                                (console-key= ki "DownArrow")
                                (console-key= ki "Tab")
                                (and (console-key= ki "R") (key-ctrl-p ki)))))

                 ;; Alt+Enter: send the form, closed or not, for the times the
                 ;; count is wrong and the reader should say so.
                 ((and (console-key= ki "Enter") (key-alt-p ki))
                  (return (submit)))

                 ;; Ctrl+J: open a line, closed or not. A terminal sends a line
                 ;; feed for it where Enter sends a carriage return, and .NET
                 ;; reports that line feed as Enter with Control held, so the
                 ;; character is what tells them apart and the key name is the
                 ;; spelling Windows uses.
                 ((or (and (characterp ch) (char= ch #\Newline))
                      (and (console-key= ki "J") (key-ctrl-p ki)))
                  (open-line))

                 ;; Enter: send a finished form, open a line inside an
                 ;; unfinished one.
                 ((console-key= ki "Enter")
                  (case (enter-action (text) :commands commands)
                    (:submit (return (submit)))
                    (t (open-line))))

                 ;; Ctrl+D: EOF
                 ((and (console-key= ki "D") (key-ctrl-p ki))
                  (when (null buf)
                    (write-str (format nil "~%"))
                    (return nil)))

                 ;; Ctrl+C: clear the form. The fresh prompt starts a new input
                 ;; area, so the cursor is back on row zero of it.
                 ;; On a continuation line the earlier lines of the form are
                 ;; held by the read loop, not here, so the loop is told to
                 ;; drop them the way it is told of Ctrl+C on Unix: by the
                 ;; interrupt condition, which it answers with "^C" and a
                 ;; fresh prompt.
                 ((and (console-key= ki "C") (key-ctrl-p ki))
                  (refresh (length buf) t nil)
                  (when (continuation-prompt-p prompt)
                    (signal (make-condition
                             (find-symbol "INTERACTIVE-INTERRUPT"
                                          "DOTCL-INTERNAL"))))
                  (write-str "^C")
                  (write-str (format nil "~%"))
                  (write-str prompt)
                  (setf buf '() point 0 cursor-row 0 hist-idx -1 saved-buf '()
                        pasting nil pasted-return nil too-tall nil
                        mode nil prompt-width (prompt-display-width prompt)))

                 ;; Alt and a letter on Windows, where the key reader reports
                 ;; the modifier instead of sending an Escape ahead of it.
                 ((and (key-alt-p ki)
                       (or (console-key= ki "B")
                           (console-key= ki "F")
                           (console-key= ki "D")))
                  (alt-key (char (key-name ki) 0)))

                 ;; Ctrl+R: search the history backwards.
                 ((and (console-key= ki "R") (key-ctrl-p ki))
                  (setf searching (isearch-start))
                  (search-refresh))

                 ;; Ctrl+L: clear the screen and start again at the top of it.
                 ;; The absolute move is a command to the terminal and not a
                 ;; question to it, which is the thing this editor does not
                 ;; do; and the input area starts over, so the cursor is back
                 ;; on row zero of it.
                 ((and (console-key= ki "L") (key-ctrl-p ki))
                  (write-str (format nil "~C[2J~C[H" #\Escape #\Escape))
                  (write-str (current-prompt))
                  (setf cursor-row 0 too-tall nil)
                  (refresh))

                 ;; Backspace. At the start of a line this takes the newline
                 ;; and joins the line to the one above, which is what it does
                 ;; in any other editor and falls out of holding the form as
                 ;; one piece of text.
                 ((or (console-key= ki "Backspace")
                      (and (console-key= ki "H") (key-ctrl-p ki)))
                  (when (> point 0)
                    (setf buf (append (subseq buf 0 (1- point))
                                      (subseq buf point)))
                    (decf point)
                    (refresh)))

                 ;; Delete
                 ((console-key= ki "Delete")
                  (when (< point (length buf))
                    (setf buf (append (subseq buf 0 point)
                                      (subseq buf (1+ point))))
                    (refresh)))

                 ;; Left arrow
                 ((or (console-key= ki "LeftArrow")
                      (and (console-key= ki "B") (key-ctrl-p ki)))
                  (when (> point 0)
                    (decf point)
                    (refresh)))

                 ;; Right arrow
                 ((or (console-key= ki "RightArrow")
                      (and (console-key= ki "F") (key-ctrl-p ki)))
                  (when (< point (length buf))
                    (incf point)
                    (refresh)))

                 ;; Home / Ctrl+A: the start of the line, which is the start of
                 ;; the form when there is only one of them.
                 ((or (console-key= ki "Home")
                      (and (console-key= ki "A") (key-ctrl-p ki)))
                  (setf point (line-start (text) point))
                  (refresh))

                 ;; End / Ctrl+E
                 ((or (console-key= ki "End")
                      (and (console-key= ki "E") (key-ctrl-p ki)))
                  (setf point (line-end (text) point))
                  (refresh))

                 ;; Up arrow: a line up inside the form, and the form before it
                 ;; once there is no line above.
                 ((console-key= ki "UpArrow")
                  (let ((above (previous-row-point (text) point)))
                    (cond (above
                           (setf point above)
                           (refresh))
                          (t
                           (let ((next-idx (1+ hist-idx)))
                             (when (< next-idx (length *history*))
                               (when (= hist-idx -1)
                                 (setf saved-buf buf))
                               (recall next-idx)))))))

                 ;; Down arrow: a line down, and the form after it once there
                 ;; is no line below.
                 ((console-key= ki "DownArrow")
                  (let ((below (next-row-point (text) point)))
                    (cond (below
                           (setf point below)
                           (refresh))
                          ((> hist-idx 0) (recall (1- hist-idx)))
                          ((= hist-idx 0)
                           (setf hist-idx -1)
                           (setf buf saved-buf)
                           (setf point (length buf))
                           (refresh)))))

                 ;; Tab: completion
                 ((console-key= ki "Tab")
                  (let ((result (complete buf point)))
                    (when result
                      (setf buf (first result)
                            point (second result))
                      (let ((items (third result)))
                        (cond ((and items
                                    (offer-completions items (fourth result))))
                              (items
                               ;; The listing scrolls the form away, so put the
                               ;; prompt and the input back underneath it.
                               ;; Leaving the cursor past the last character
                               ;; keeps the listing from starting inside them.
                               (refresh (length buf))
                               (show-completions items)
                               (write-str prompt)
                               (setf cursor-row 0)
                               (refresh))
                              (t (refresh)))))))

                 ;; Ctrl+K: kill to the end of the line, and where there is
                 ;; nothing left to kill take the newline, which joins the line
                 ;; below to this one.
                 ((and (console-key= ki "K") (key-ctrl-p ki))
                  (let ((end (line-end (text) point)))
                    (kill point (if (and (= end point) (< end (length buf)))
                                    (1+ end)
                                    end))))

                 ;; Ctrl+U: kill to the start of the line
                 ((and (console-key= ki "U") (key-ctrl-p ki))
                  (kill (line-start (text) point) point))

                 ;; Ctrl+W: take back everything to the last blank, brackets
                 ;; and all, which is what it takes back everywhere else.
                 ((and (console-key= ki "W") (key-ctrl-p ki))
                  (kill (whitespace-backward (text) point) point))

                 ;; Printable character
                 ((and (characterp ch)
                       (graphic-char-p ch))
                  (insert (string ch)))))))

        ;; An interrupt (Ctrl+C where it is a signal) leaves the line without
        ;; a redraw, and a mode's menu would stay on the screen under it.
        (when menu-drawn
          (ignore-errors (refresh (length buf) nil nil)))
        ;; However the edit ended, the terminal goes back to sending pasted
        ;; text plain. Leaving the mode on would put the delimiters into
        ;; whatever reads next, which is the fault this whole mechanism is for.
        ;; Ctrl+C goes back to interrupting before the form is evaluated.
        (ctrl-c-as-input ctrl-c-was)
        (write-str *bracketed-paste-off*)))))

;;; -- Inline menu -------------------------------------------------------------
;;;
;;; A list of choices drawn on the rows under the line being typed, with one of
;;; them marked, moved with the up and down arrows and taken with Enter. The
;;; first user is the debugger, which offers its restarts this way; TAB
;;; completion offers several candidates this way (see OFFER-COMPLETIONS in the
;;; editor), and the debugger's :frames offers the backtrace this way (see
;;; FRAME-MENU).
;;;
;;; It draws the way the line editor does, with relative cursor movement only
;;; and no question to the terminal. The menu is written below the anchor row
;;; with a carriage return and a line feed per row, so at the bottom of the
;;; window the terminal scrolls and the rows still land under the anchor; the
;;; cursor then goes back up by the number of rows written, which is right
;;; whether or not anything scrolled. Every row is cut to fit in one row less
;;; than the width, so none of them wraps and the count stays exact. Taking the
;;; menu away is an erase from the anchor to the end of the screen, which
;;; leaves no row of it behind.
;;;
;;; Everything that decides something is a function of its arguments: which
;;; key does what (MENU-STEP), which rows are shown (MENU-WINDOW-TOP), what is
;;; written (RENDER-MENU). RUN-MENU puts them together, reading keys and
;;; writing through functions it is given, so it runs without a terminal too.

(defparameter *menu-max-rows* 10
  "The most rows a menu takes on the screen; a longer one scrolls.")

(defun menu-label-text (label)
  "LABEL as one row: a newline or other control character becomes a blank."
  (map 'string (lambda (ch) (if (graphic-char-p ch) ch #\Space)) label))

(defun truncate-to-width (text width)
  "TEXT cut to at most WIDTH columns, ending in ... when anything was cut."
  (if (<= (string-display-width text) width)
      text
      (let ((room (- width 3))
            (used 0)
            (end 0))
        (loop for ch across text
              for w = (char-display-width ch)
              while (<= (+ used w) room)
              do (incf used w)
                 (incf end))
        (if (< width 3)
            (subseq "..." 0 (max 0 width))
            (concatenate 'string (subseq text 0 end) "...")))))

(defun menu-visible-rows (count height)
  "How many of COUNT choices are shown in a window HEIGHT rows tall.
The anchor row and one more stay free, so the menu never pushes the line being
typed off the top."
  (max 1 (min count *menu-max-rows* (- height 2))))

(defun menu-window-top (selected top rows count)
  "The first choice shown, moved no more than it takes to show SELECTED."
  (let ((top (max 0 (min top (- count rows)))))
    (cond ((< selected top) selected)
          ((>= selected (+ top rows)) (1+ (- selected rows)))
          (t top))))

(defun menu-row-text (label selected-p width)
  "The row for LABEL: a marker, then the label cut to leave the last column
free."
  (truncate-to-width (concatenate 'string (if selected-p "> " "  ")
                                  (menu-label-text label))
                     (max 1 (1- width))))

(defun render-menu (labels selected top rows width anchor-col typed
                    &optional (painter #'identity))
  "The string that draws a menu under the anchor row.

It starts by going to column ANCHOR-COL of the anchor row, writes TYPED there
(the digits of a choice typed by number), erases to the end of the screen, and
writes ROWS rows of LABELS from TOP, SELECTED marked and passed through PAINTER.
It ends with the cursor back on the anchor row after TYPED."
  (with-output-to-string (out)
    (write-string *ansi-column-1* out)
    (write-string (ansi-right anchor-col) out)
    (write-string typed out)
    (write-string *ansi-erase-below* out)
    (loop for i from top below (min (length labels) (+ top rows))
          for label = (nth i labels)
          do (write-char #\Return out)
             (write-char #\Newline out)
             (let ((text (menu-row-text label (= i selected) width)))
               (write-string (if (= i selected) (funcall painter text) text)
                             out)))
    (let ((shown (- (min (length labels) (+ top rows)) top)))
      (write-string (ansi-up shown) out))
    (write-string *ansi-column-1* out)
    (write-string (ansi-right (+ anchor-col (string-display-width typed))) out)))

;;; A line mode's menu (the comma commands) is not modal the way RUN-MENU is:
;;; it is drawn again under the line after every redraw while the line is being
;;; typed, so typing on narrows it, and only the arrows, TAB and Enter are its.
;;; The mark can be on no row at all, which is how it starts on an empty line.

(defun mode-menu-string (labels selected top width height end-col point-col)
  "The string that draws a line mode's menu of LABELS under the input row,
anchored at END-COL where the line ends, then puts the cursor back at
POINT-COL. SELECTED is the marked row or NIL. Returns the string and the first
row in view, which the caller keeps for the next redraw."
  (let* ((count (length labels))
         (rows (menu-visible-rows count height))
         (top (menu-window-top (or selected 0) top rows count)))
    (values (concatenate 'string
                         (render-menu labels (or selected -1) top rows width
                                      end-col "" #'paint-selected)
                         *ansi-column-1*
                         (ansi-right point-col))
            top)))

(defun mode-menu-select (key selected count)
  "The mark after KEY, :UP or :DOWN, in a line mode's menu of COUNT rows.
With no mark yet, down marks the first row and up the last; either way the
mark stops at the ends."
  (cond ((zerop count) nil)
        ((null selected) (if (eq key :down) 0 (1- count)))
        ((eq key :down) (min (1+ selected) (1- count)))
        (t (max 0 (1- selected)))))

(defun erase-menu-string (anchor-col)
  "The string that takes a menu away, and anything typed after ANCHOR-COL."
  (concatenate 'string *ansi-column-1* (ansi-right anchor-col) *ansi-erase-below*))

(defun menu-step (key selected typed count &key numbered)
  "What KEY does to a menu of COUNT choices with SELECTED marked and TYPED
digits typed. Returns the action, and the new SELECTED and TYPED.

The actions are :REDRAW, :NONE (nothing changed), :CHOOSE, :CANCEL, :EOF and
:OTHER (a key the menu does not use, for the caller). KEY is :UP, :DOWN, :HOME,
:END, :ENTER, :CANCEL, :BACKSPACE, :EOF, :IGNORE or a character.

With NUMBERED, digits choose by number the way typing the number did before
there was a menu: they collect on the anchor row and mark the choice they name,
and Enter takes it. A digit that would name no choice starts the number again,
or is refused when it names none on its own either."
  (flet ((redraw (new-selected new-typed)
           (values :redraw new-selected new-typed)))
    (case key
      (:up (if (> selected 0) (redraw (1- selected) "") (values :none selected typed)))
      (:down (if (< selected (1- count)) (redraw (1+ selected) "") (values :none selected typed)))
      (:home (redraw 0 ""))
      (:end (redraw (max 0 (1- count)) ""))
      (:enter (values :choose selected typed))
      (:cancel (values :cancel selected typed))
      (:eof (if (string= typed "")
                (values :eof selected typed)
                (values :none selected typed)))
      (:backspace
       (if (string= typed "")
           (values :none selected typed)
           (let ((shorter (subseq typed 0 (1- (length typed)))))
             (redraw (if (string= shorter "") selected (parse-integer shorter))
                     shorter))))
      (:ignore (values :none selected typed))
      (t
       (cond ((and numbered (characterp key) (digit-char-p key))
              (let ((longer (concatenate 'string typed (string key))))
                (cond ((< (parse-integer longer) count)
                       (redraw (parse-integer longer) longer))
                      ((< (digit-char-p key) count)
                       (redraw (digit-char-p key) (string key)))
                      (t (values :none selected typed)))))
             ;; Anything else is the caller's. After digits, the number was
             ;; the start of an expression, and TYPED goes along with the key.
             (t (values :other selected typed)))))))

(defun console-menu-key (ki)
  "What the console key KI means to a menu."
  (let ((ch (and ki (key-char ki))))
    (cond ((null ki) :eof)
          ((or (console-key= ki "UpArrow")
               (and (console-key= ki "P") (key-ctrl-p ki)))
           :up)
          ((or (console-key= ki "DownArrow")
               (and (console-key= ki "N") (key-ctrl-p ki)))
           :down)
          ((console-key= ki "Home") :home)
          ((console-key= ki "End") :end)
          ((console-key= ki "Enter") :enter)
          ((and (key-ctrl-p ki)
                (or (console-key= ki "C") (console-key= ki "G")))
           :cancel)
          ((and (console-key= ki "D") (key-ctrl-p ki)) :eof)
          ((or (console-key= ki "Backspace")
               (and (console-key= ki "H") (key-ctrl-p ki)))
           :backspace)
          ;; Escape on its own cancels. The arrows arrive already parsed, so an
          ;; Escape with more behind it is a key this menu does not bind.
          ((console-key= ki "Escape")
           (if (wait-for-key)
               (progn (read-escape-sequence) :ignore)
               :cancel))
          ((and (characterp ch) (graphic-char-p ch)
                (not (key-ctrl-p ki)) (not (key-alt-p ki)))
           ch)
          (t :ignore))))

(defun read-menu-key ()
  (console-menu-key (console-read-key-interruptable)))

(defun paint-selected (text)
  (paint :selected text))

(defun run-menu (labels &key (selected 0) (anchor-col 0) numbered
                          (read-key #'read-menu-key) (write #'write-str)
                          (width (terminal-width)) (height (terminal-height))
                          (painter #'paint-selected))
  "Show LABELS as a menu under the row the cursor is on, the cursor being at
ANCHOR-COL, and let keys move through it until one decides.

Returns (values :CHOOSE index), (values :CANCEL nil), (values :EOF nil) or
(values :OTHER key) for a key the menu does not use, which is for the caller to
act on. The menu has been taken off the screen by then, whatever the outcome,
with the cursor at ANCHOR-COL of the anchor row. An interrupt (Ctrl+C where it
is a signal and not a key) cancels.

NUMBERED lets digits choose by number (see MENU-STEP)."
  (let* ((count (length labels))
         (rows (menu-visible-rows count height))
         (top (menu-window-top selected 0 rows count))
         (typed "")
         (shown nil)
         (interrupt (find-symbol "INTERACTIVE-INTERRUPT" "DOTCL-INTERNAL"))
         (ctrl-c-was nil))
    (when (zerop count)
      (return-from run-menu (values :cancel nil)))
    (flet ((draw ()
             (setf top (menu-window-top selected top rows count))
             (funcall write (render-menu labels selected top rows width
                                         anchor-col typed painter))
             (setf shown t)))
      (unwind-protect
           (block menu
             (handler-bind ((condition
                              (lambda (c)
                                (when (and interrupt (typep c interrupt))
                                  (return-from menu (values :cancel nil))))))
               (setf ctrl-c-was (ctrl-c-as-input t))
               (draw)
               (loop
                 (let ((key (funcall read-key)))
                   (multiple-value-bind (action new-selected new-typed)
                       (menu-step key selected typed count :numbered numbered)
                     (setf selected new-selected typed new-typed)
                     (case action
                       (:redraw (draw))
                       (:choose (return-from menu (values :choose selected)))
                       (:cancel (return-from menu (values :cancel nil)))
                       (:eof (return-from menu (values :eof nil)))
                       (:other (return-from menu
                                 (values :other
                                         (if (characterp key)
                                             (concatenate 'string typed (string key))
                                             key))))))))))
        (ctrl-c-as-input ctrl-c-was)
        (when shown
          (funcall write (erase-menu-string anchor-col)))))))

;;; -- The debugger's prompt ---------------------------------------------------
;;;
;;; With the line editor on, the debugger reads its lines here. At a prompt that
;;; offers the restarts, they are a menu under the prompt: the arrows move the
;;; mark and Enter takes it, digits still choose by number, and any other key
;;; puts the menu away and starts an ordinary edited line with that key in it,
;;; for :bt or an expression. Escape or Ctrl+C puts the menu away for the rest
;;; of this debugger level and leaves the numbered list, as it is printed
;;; without a menu.
;;;
;;; No menu where the escape sequences would land somewhere other than a
;;; terminal that obeys them: output redirected, input redirected, TERM=dumb.
;;; The debugger then prints the list and reads numbers, as it always has.
;;; That is the runtime's decision about the line editor, asked without
;;; --readline, so a menu is never drawn where the editor would not be.

(defun menu-usable-p (&key (term (dotnet:static "System.Environment"
                                                 "GetEnvironmentVariable" "TERM"))
                           (output-redirected
                            (dotnet:static "System.Console" "get_IsOutputRedirected"))
                           (input-redirected
                            (dotnet:static "System.Console" "get_IsInputRedirected")))
  "True when a menu can be drawn: input and output are a terminal, and it is
not one that says it cannot move the cursor."
  (let ((decide (find-symbol "%REPL-LINE-EDITING-DECISION" "DOTCL")))
    (if (and decide (fboundp decide))
        (and (funcall decide :auto term (not input-redirected) (not output-redirected))
             t)
        (and (not input-redirected)
             (not output-redirected)
             (not (equal term "dumb"))))))

(defun read-debugger-line (prompt &optional initial)
  "An edited line at the debugger PROMPT, NIL at the end of input. Ctrl+C
drops the line and answers an empty one, which prompts again."
  (let ((interrupt (find-symbol "INTERACTIVE-INTERRUPT" "DOTCL-INTERNAL")))
    (block read
      (handler-bind ((condition
                       (lambda (c)
                         (when (and interrupt (typep c interrupt))
                           (write-str (format nil "^C~%"))
                           (return-from read "")))))
        (values (read-line-edited prompt nil (or initial "")))))))

(defun frame-menu (prompt labels selected
                   &key (usable (menu-usable-p)) (write #'write-str)
                        (read-key #'read-menu-key) (width (terminal-width))
                        (height (terminal-height)) (painter #'paint-selected))
  "The debugger's :frames. Offer LABELS, the backtrace as :bt prints it, as a
menu under PROMPT with frame SELECTED marked.

Returns the index of the frame chosen, :CANCEL when the menu was closed without
one, (:OTHER string) for a key that starts a line instead, or :NO-MENU when no
menu can be drawn and nothing was written.

Enter takes the marked frame, and digits choose by number as :frame N does.
The row is then left reading as if :frame N had been typed at the prompt, so
the scrollback says which frame the output under it is about. Escape, Ctrl+C
and Ctrl+D close the menu and take the prompt away, leaving the selection as it
was; Ctrl+D does not end the debugger here as it does at the prompt. Any other
key closes the menu and starts a line with that key in it."
  (cond
    ((or (null labels) (not usable)) :no-menu)
    (t
     (funcall write prompt)
     (multiple-value-bind (action value)
         (run-menu labels :selected selected :numbered t
                          :anchor-col (prompt-display-width prompt)
                          :read-key read-key :write write
                          :width width :height height :painter painter)
       (ecase action
         (:choose
          (funcall write (format nil ":frame ~D~%" value))
          value)
         ((:cancel :eof)
          (funcall write *ansi-column-1*)
          (funcall write *ansi-erase-below*)
          :cancel)
         (:other
          (funcall write *ansi-column-1*)
          (funcall write *ansi-erase-below*)
          (list :other (if (stringp value) value ""))))))))

(defun debugger-read (prompt labels &optional (selected 0) kind)
  "Read at the debugger PROMPT. With LABELS, the restarts as the debugger lists
them, offer them as a menu first. With KIND :FRAMES, LABELS are the backtrace
instead, offered by FRAME-MENU with frame SELECTED marked.

Returns the index of a chosen restart (or frame), a line typed, NIL at the end
of input, :CANCEL when the frame menu was closed without a choice, or :NO-MENU
when there was no menu to offer or it was put away, in which case nothing was
read and the caller lists the restarts and asks again without LABELS (or, for
frames, prints the backtrace)."
  (cond
    ((eq kind :frames)
     (let ((answer (frame-menu prompt labels selected)))
       (if (consp answer)
           (read-debugger-line prompt (second answer))
           answer)))
    ((null labels) (read-debugger-line prompt))
    ((not (menu-usable-p)) :no-menu)
    (t
     (write-str prompt)
     (let ((anchor (prompt-display-width prompt)))
       (multiple-value-bind (action value)
           (run-menu labels :anchor-col anchor :numbered t)
         (ecase action
           (:choose
            ;; The row left on the screen reads like the list without a menu
            ;; would have it: the prompt, then the restart that was taken.
            (write-str (truncate-to-width (menu-label-text (nth value labels))
                                          (max 1 (- (terminal-width) anchor 1))))
            (write-str (format nil "~%"))
            value)
           (:cancel
            ;; The caller prints the list where the prompt was.
            (write-str *ansi-column-1*)
            (write-str *ansi-erase-below*)
            :no-menu)
           (:eof
            (write-str (format nil "~%"))
            nil)
           (:other
            ;; Start over on a clean row: the editor writes its own prompt.
            (write-str *ansi-column-1*)
            (write-str *ansi-erase-below*)
            (read-debugger-line prompt (if (stringp value) value "")))))))))

;;; -- Comma commands ----------------------------------------------------------
;;;
;;; A line whose first character is a comma is an instruction to the REPL, not a
;;; form to evaluate. The mechanism, the names and the aliases are icl's
;;; (https://github.com/atgreen/icl, MIT licensed), kept the same on purpose so
;;; that what a reader already knows carries over.
;;;
;;; The comma is free to mean this because an unquote outside a backquote is not
;;; a program in the first place, so no reachable spelling is taken away. The one
;;; place a leading comma is ordinary text is a multi-line backquoted form, where
;;; the unquote can start a line of its own; READLINE declines to dispatch there.

(defstruct (command (:constructor %make-command))
  name       ; the command's own name, downcased, without the comma
  aliases    ; other names it answers to, downcased
  args       ; how HELP spells the argument, for humans only
  doc        ; first line for the listing, the whole of it for HELP <command>
  handler)   ; one function of the argument string

(defvar *commands* (make-hash-table :test 'equal)
  "Every name and alias, downcased, to the COMMAND it names.")

(defvar *command-list* '()
  "Every command once, in the order the commands were defined. What HELP lists.")

(defun register-command (command)
  "Put COMMAND in the table under its name and each of its aliases, in place of
any earlier command of the same name. Redefining a command has to retract the
aliases of the definition it replaces, or a dropped alias would go on answering."
  (let ((old (gethash (command-name command) *commands*)))
    (when old
      (dolist (name (cons (command-name old) (command-aliases old)))
        (when (eq (gethash name *commands*) old)
          (remhash name *commands*)))
      (setf *command-list* (remove old *command-list*))))
  (dolist (name (cons (command-name command) (command-aliases command)))
    (setf (gethash name *commands*) command))
  (setf *command-list* (append *command-list* (list command)))
  command)

(defmacro define-command (names (argument &optional (argument-spec "")) doc
                          &body body)
  "Define a comma command.

NAMES is the command's name followed by its aliases, each a string or a symbol
and all matched without regard to case. ARGUMENT is the variable bound to
everything typed after the name, as one trimmed string; ARGUMENT-SPEC is how
HELP spells that argument. DOC is what HELP prints: its first line in the
listing, the whole of it when the command is named.

Exported because a command written in a user init file should be worth no less
than one written here. Require this contrib first, so that the package exists
by the time the init file is read."
  (let ((strings (mapcar (lambda (name) (string-downcase (string name))) names)))
    `(register-command
      (%make-command :name ,(first strings)
                     :aliases ',(rest strings)
                     :args ,argument-spec
                     :doc ,doc
                     :handler (lambda (,argument)
                                (declare (ignorable ,argument))
                                ,@body)))))

;;; -- Parsing a command line --------------------------------------------------

(defun trim-whitespace (string)
  (string-trim *whitespace* string))

(defun split-command (line)
  "Return (values NAME ARGUMENT) for a command line, or NIL.

NAME is what follows the comma up to the first whitespace, downcased. ARGUMENT
is the whole of the rest with the surrounding whitespace taken off. The argument
is never divided further, so a form with spaces inside it and a file name with
spaces inside it both arrive whole and the command reads them itself. A line
that is a comma and nothing else gives an empty name, which DISPATCH answers
with a pointer to HELP rather than with an error."
  (when (command-line-p line)
    (let ((end (or (position-if #'whitespace-char-p line :start 1)
                   (length line))))
      (values (string-downcase (subseq line 1 end))
              (trim-whitespace (subseq line end))))))

;;; -- Running a command -------------------------------------------------------

(defvar *quit-requested* nil
  "Set by QUIT-REPL and read by DISPATCH once the command has returned.")

(defun quit-repl ()
  "Ask the REPL to stop once the running command returns."
  (setf *quit-requested* t))

(defun dispatch (line)
  "Run LINE if it is a command.

Returns :NOT-A-COMMAND when LINE is Lisp for the caller to evaluate, :QUIT when
the command asked to end the session, and :HANDLED when the command has run.

Every way a command can go wrong ends here: an unknown name, a bare comma, and
an error raised inside a command body are each reported on *ERROR-OUTPUT* and
then forgotten. A prompt coming back is worth more than a backtrace, and a
command is a convenience -- it should not be able to end a session that a
mistyped form would not have ended."
  (multiple-value-bind (name argument) (split-command line)
    (cond
      ((null name) :not-a-command)
      ((string= name "")
       (format *error-output* "~A~%"
               (paint :error "; a command is a comma and a name. ,help lists them."
                      *error-output*))
       :handled)
      (t
       (let ((command (gethash name *commands*)))
         (cond
           ((null command)
            (format *error-output* "~A~%"
                    (paint :error (format nil "; no command named ,~A. ,help lists them." name)
                           *error-output*))
            :handled)
           (t
            (let ((*quit-requested* nil))
              (handler-case (funcall (command-handler command) argument)
                (error (condition)
                  (format *error-output* "~A~%"
                          (paint :error (format nil "; ,~A: ~A" name condition)
                                 *error-output*))))
              (if *quit-requested* :quit :handled)))))))))

;;; -- Reading a command's argument --------------------------------------------
;;;
;;; These signal rather than print, because DISPATCH already turns a signal into
;;; one line at the prompt and a command that reported its own errors would have
;;; to be trusted to do it the same way.

(defun require-argument (name argument)
  "ARGUMENT, or a signal when the command was given none."
  (when (string= argument "")
    (error "~A takes an argument. ,help ~A says which." name name))
  argument)

(defun read-argument-form (name argument)
  "The argument read as one Lisp form, in the current package."
  (read-from-string (require-argument name argument)))

(defun read-argument-symbol (name argument)
  "The argument read as a symbol."
  (let ((form (read-argument-form name argument)))
    (unless (symbolp form)
      (error "~A wants a symbol, not ~S." name form))
    form))

(defun argument-namestring (name argument)
  "The argument as a file or directory name.

An argument that opens with a double quote is read as a Lisp string, so a name
with a space in it can be written the way it would be written in code. Anything
else is taken literally, the way a shell takes it, because quoting a path that
needs no quoting is a tax on the common case."
  (let ((text (require-argument name argument)))
    (if (char= (char text 0) #\")
        (let ((value (read-from-string text)))
          (unless (stringp value)
            (error "~A: ~S is not a file name." name value))
          value)
        text)))

(defun as-directory-namestring (text)
  "TEXT with a separator on the end, so it names a directory and not a file."
  (if (or (zerop (length text))
          (member (char text (1- (length text))) '(#\/ #\\)))
      text
      (concatenate 'string text "/")))

;;; -- Reaching other contribs -------------------------------------------------

(defun contrib-function (module package name)
  "PACKAGE:NAME, with MODULE loaded first, or a signal naming what is missing.

Loading these where the command runs rather than where this file loads is what
keeps the REPL's startup free of them, and keeps a REPL whose tree is missing
one of them working everywhere else."
  (ignore-errors (require module))
  (let* ((found (find-package package))
         (symbol (and found (find-symbol name found))))
    (unless (and symbol (fboundp symbol))
      (error "~A is not available here: (require ~S) left no ~A:~A"
             module module package name))
    (symbol-function symbol)))

(defun macroexpand-all-function ()
  "MACROEXPAND-ALL where the image has it, MACROEXPAND where it does not."
  (let* ((package (find-package "DOTCL-CLTL2"))
         (symbol (and package (find-symbol "MACROEXPAND-ALL" package))))
    (if (and symbol (fboundp symbol))
        (symbol-function symbol)
        #'macroexpand)))

(defun jit-disassembler ()
  "DOTCL:JIT-DISASSEMBLE once its contrib has loaded, or NIL.

The function is defined in every image; it is the contrib that installs the
hook behind it. So whether the contrib loaded is the question worth asking, and
calling the function is not a test, it is the answer."
  (let ((symbol (and (ignore-errors (require "dotcl-jitdisasm") t)
                     (find-symbol "JIT-DISASSEMBLE" "DOTCL"))))
    (and symbol (fboundp symbol) (symbol-function symbol))))

;;; -- The commands ------------------------------------------------------------

(defun command-summary (command)
  "The first line of COMMAND's documentation, which is what the listing shows."
  (let ((doc (command-doc command)))
    (subseq doc 0 (or (position #\Newline doc) (length doc)))))

(defun command-spelling (command)
  "The command and its argument, as they are typed."
  (if (string= (command-args command) "")
      (format nil ",~A" (command-name command))
      (format nil ",~A ~A" (command-name command) (command-args command))))

;;; The listing is generated, so a command that exists is a command that is
;;; documented: there is no second list to forget to add a line to.
(define-command ("help" "h" "?") (argument "[command]")
  "List the commands, or explain one of them.
With no argument every command is listed with the first line of what it does.
Name one, with or without its comma, and its aliases and the whole of its
description are printed instead."
  (if (string= argument "")
      (let ((width (reduce #'max *command-list*
                           :key (lambda (c) (length (command-spelling c)))
                           :initial-value 0)))
        (format t "~&Commands. A line that starts with a comma is one of these:~%")
        (dolist (c *command-list*)
          (format t "  ~vA  ~A~%" width (command-spelling c) (command-summary c)))
        (format t "~D commands. ,help <command> describes one of them.~%"
                (length *command-list*))
        (when *line-modes*
          (format t "~%Modes. One of these typed on an empty line switches the prompt for that~%~
                     line; Backspace on the empty line switches back:~%")
          (let ((width (reduce #'max *line-modes*
                               :key (lambda (m) (length (line-mode-label m))))))
            (dolist (mode *line-modes*)
              (format t "  ~C  ~vA  ~A~%" (line-mode-char mode) width
                      (line-mode-label mode) (line-mode-summary mode))))))
      (let* ((name (string-downcase (string-left-trim "," argument)))
             (c (gethash name *commands*)))
        (if (null c)
            (format *error-output* "; no command named ,~A~%" name)
            (progn
              (format t "~&~A~%" (command-spelling c))
              (when (command-aliases c)
                (format t "aliases: ~{,~A~^ ~}~%" (command-aliases c)))
              (format t "~A~%" (command-doc c)))))))

(define-command ("quit" "exit" "q") (argument)
  "Leave the REPL.
The line editor answers the read loop with nothing left to read, which is what
it also answers for end of input, so the session ends the same way either way."
  (quit-repl))

(define-command ("clear") (argument)
  "Clear the screen.
Written as escape sequences rather than through System.Console.Clear, which
throws where there is no console and would take a command down with it."
  (format t "~C[2J~C[H" #\Escape #\Escape)
  (finish-output))

(define-command ("history") (argument)
  "Print the lines the up arrow remembers, oldest first."
  (let ((lines (reverse *history*)))
    (if (null lines)
        (format t "~&; nothing in the history yet~%")
        (loop for line in lines
              for n from 1
              do (format t "~&~4D  ~A~%" n line)))))

(define-command ("in-package" "pkg") (argument "<package>")
  "Make a package the current one.
The prompt changes on the next line rather than this one, because the read loop
asks for the current package once per line instead of keeping the one it
started with."
  (let* ((text (string-left-trim ":" (string-trim "\"" (require-argument ",in-package" argument))))
         (package (or (find-package text) (find-package (string-upcase text)))))
    (when (null package)
      (error "there is no package named ~A" text))
    (setf *package* package)
    (format t "~&~A~%" (package-name package))))

(define-command ("pwd") (argument)
  "Print both senses of where the REPL is: the package and the directory.
A Lisp has two of them and a reader who has moved one wants to see the other."
  (format t "~&package:   ~A~%" (package-name *package*))
  (format t "directory: ~A~%" (namestring *default-pathname-defaults*)))

(define-command ("cd") (argument "[directory]")
  "Change directory, in the shell's sense of the word.
Moves the process working directory as well as *default-pathname-defaults*, so
a relative name resolves under the new directory afterwards whether it is
resolved in Lisp or in .NET. With no argument it goes to the home directory.
icl spells package switching ,cd; here that is ,in-package and ,cd is the
shell's."
  (let* ((text (if (string= argument "")
                   (namestring (user-homedir-pathname))
                   (argument-namestring ",cd" argument)))
         (new (dotcl:chdir (as-directory-namestring text))))
    (setf *default-pathname-defaults* new)
    (format t "~&~A~%" (namestring new))))

(define-command ("doc" "d") (argument "<symbol>")
  "Print the documentation a symbol carries.
Every kind of it at once, function and variable and type and the rest, because
which kind a given name carries is the part the reader does not know yet."
  (let ((symbol (read-argument-symbol ",doc" argument))
        (found nil))
    (dolist (kind '(function variable type structure setf compiler-macro))
      (let ((text (ignore-errors (documentation symbol kind))))
        (when text
          (setf found t)
          (format t "~&~A as a ~A:~%~A~%"
                  symbol (string-downcase (symbol-name kind)) text))))
    (unless found
      (format t "~&; ~A carries no documentation~%" symbol))))

(define-command ("describe" "desc") (argument "<symbol or form>")
  "Describe an object: its type, its value, and whatever else it is carrying.
A symbol is described as itself, the way icl takes a symbol name, so ,describe
car describes CAR. Any other form is evaluated and its value described, so
,describe 'car says the same thing and ,describe #'car describes the function."
  (let ((form (read-argument-form ",describe" argument)))
    (describe (if (symbolp form) form (eval form)))))

(define-command ("apropos" "ap") (argument "<pattern>")
  "List the symbols whose names contain a string."
  (apropos (string-trim "\"" (require-argument ",apropos" argument))))

(define-command ("args") (argument "<symbol>")
  "Print how a function is called.
Answered by dotcl-lsp-api, which is where an editor asks the same question, so
the prompt and the editor cannot come to disagree about a lambda list."
  (let* ((text (require-argument ",args" argument))
         (description (funcall (contrib-function "dotcl-lsp-api"
                                                 "DOTCL-LSP-API" "DESCRIBE-AT")
                               text (length text)))
         (signatures (getf description :signatures)))
    (cond ((null description)
           (format t "~&; nothing here is named ~A~%" text))
          ((null signatures)
           (format t "~&~A: nothing that has a lambda list~%"
                   (getf description :name)))
          (t (dolist (signature signatures)
               (format t "~&~A~%" signature))))))

(define-command ("mx") (argument "<form>")
  "Expand a macro call once."
  (pprint (macroexpand-1 (read-argument-form ",mx" argument)))
  (terpri))

(define-command ("mxa") (argument "<form>")
  "Expand a form and everything inside it until no macro is left."
  (pprint (funcall (macroexpand-all-function)
                   (read-argument-form ",mxa" argument)))
  (terpri))

(define-command ("time") (argument "<form>")
  "Evaluate a form and report what it cost.
The values come out after the timing, because the read loop is given a blank
line by a command and so prints nothing itself."
  (let ((values (multiple-value-list
                 (eval (list 'time (read-argument-form ",time" argument))))))
    (dolist (value values)
      (format t "~&~S~%" value))))

(define-command ("load" "ld") (argument "<file>")
  "Load a file."
  (load (argument-namestring ",load" argument)))

(define-command ("ql") (argument "<system>")
  "Load a system with quicklisp, fetching it first if this image lacks it."
  (let ((name (string-trim "\":" (require-argument ",ql" argument))))
    (funcall (contrib-function "quicklisp" "QL" "QUICKLOAD") name)))

(define-command ("trace") (argument "<symbol>")
  "Trace a function: print its arguments and its result at every call."
  (eval (list 'trace (read-argument-symbol ",trace" argument))))

(define-command ("untrace") (argument "[symbol]")
  "Stop tracing a function, or with no argument stop tracing everything."
  (if (string= argument "")
      (eval (list 'untrace))
      (eval (list 'untrace (read-argument-symbol ",untrace" argument)))))

;;; dotcl-jitdisasm rather than dotcl-decompiler, because they answer different
;;; questions and only one of them is the question a prompt asks. jitdisasm
;;; takes a function object and prints the machine code the JIT produced for it,
;;; which is what ",dis my-function" means. The decompiler recovers C# for a
;;; named .NET type, so it wants a type name rather than a function, and for an
;;; ordinary compiled Lisp function there is no type name to give it; it also
;;; fetches an engine from NuGet on first use, which is a download at a prompt.
(define-command ("dis") (argument "<symbol>")
  "Disassemble a function to the native code the JIT produced for it.
Falls back to printing CIL where dotcl-jitdisasm is not built, which is a
weaker answer than native code but a better one than none."
  (let ((symbol (read-argument-symbol ",dis" argument)))
    (unless (fboundp symbol)
      (error "~A names no function" symbol))
    (let ((disassemble-native (jit-disassembler)))
      (cond (disassemble-native
             (funcall disassemble-native (symbol-function symbol)))
            (t
             (format t "~&; dotcl-jitdisasm is not available here, showing CIL~%")
             (disassemble symbol))))))

;;; -- The command prompt ------------------------------------------------------
;;;
;;; A comma typed on an empty line switches the prompt to cmd> for one line,
;;; with the command names offered in a menu under it: typing narrows the menu,
;;; the arrows move the mark, TAB puts the marked name in the line and Enter
;;; runs it. SLIME's REPL answers a comma on an empty line with a command
;;; prompt that completes, and this is that. Nothing is lost by taking the
;;; comma: at the start of a line, outside a backquote, it could only be a
;;; reader error.
;;;
;;; Typed straight on, the result is what it always was: ,doc car arrives as
;;; cmd> doc car and runs ,doc car, and the history keeps it as ,doc car. The
;;; menu is only a way to find a name. Where no menu can be drawn the comma is
;;; not taken and the line goes as typed (see LINE-MODE-FOR-KEY).

(defun command-names (command)
  (cons (command-name command) (command-aliases command)))

(defun command-menu-items (text)
  "The commands to offer for TEXT, typed at cmd>: every command with a name or
an alias that TEXT begins, one row each, NIL once TEXT has a blank in it (the
name is finished and the argument is being typed). A command named by TEXT
exactly comes first, so Enter on a whole name runs that name."
  (unless (find-if #'whitespace-char-p text)
    (let* ((typed (string-downcase text))
           (items
             (loop for c in *command-list*
                   for matching = (remove-if-not
                                   (lambda (name)
                                     (and (<= (length typed) (length name))
                                          (string= typed name :end2 (length typed))))
                                   (command-names c))
                   when matching
                     collect (list :label (if (member (command-name c) matching
                                                      :test #'string=)
                                              (command-name c)
                                              (first matching))
                                   :detail (command-summary c)
                                   :command c))))
      (stable-sort items #'>
                   :key (lambda (item)
                          (if (member typed (command-names (getf item :command))
                                      :test #'string=)
                              1 0))))))

(defun command-argument-required-p (command)
  "True when COMMAND cannot run without an argument: its argument is spelt
<like this> rather than [like this] or not at all."
  (let ((spec (command-args command)))
    (and (plusp (length spec)) (char= (char spec 0) #\<))))

(defun command-line-choose (text item how)
  "What taking ITEM from the cmd> menu does to TEXT, by Enter (HOW :ENTER) or by
TAB (:TAB). Returns (values :SUBMIT line) or (values :EDIT line).

Enter on a name typed out in full runs it as typed, whichever row is marked
first, so ,cd and the rest behave as they did without a menu. Otherwise Enter
runs the marked command when it can run without an argument, and puts its name
and a blank in the line when it cannot. TAB only fills in the name, with the
blank when the command takes an argument at all."
  (let* ((command (getf item :command))
         (label (getf item :label))
         (with-blank (concatenate 'string label " ")))
    (cond ((and (eq how :enter)
                (member (string-downcase text) (command-names command)
                        :test #'string=))
           (values :submit text))
          ((eq how :tab)
           (values :edit (if (string= (command-args command) "") label with-blank)))
          ((command-argument-required-p command) (values :edit with-blank))
          (t (values :submit label)))))

(defun run-command-line (line)
  "Run LINE, typed at cmd>, as the comma command it spells. A blank line runs
nothing. Returns :QUIT when the command asked to end the session."
  (unless (zerop (length (trim-whitespace line)))
    (dispatch (concatenate 'string "," line))))

(defun command-history-entry (line)
  "The history keeps a cmd> line as the comma command it is, so the up arrow
brings it back in a form that runs at the Lisp prompt too."
  (unless (zerop (length (trim-whitespace line)))
    (concatenate 'string "," line)))

(define-line-mode #\, :command "cmd>" :command
  "Run a comma command, picked from a menu of the names."
  #'run-command-line
  :record #'command-history-entry
  :menu #'command-menu-items
  :choose #'command-line-choose)

;;; -- REPL integration --------------------------------------------------------
;;;
;;; Commands are run where the line is read, not where it is evaluated.
;;;
;;; The read loop is C# and knows nothing about them, and it does not have to:
;;; answering it with the empty string after a command has run is enough,
;;; because it already skips a blank line, and going round again is exactly what
;;; re-reads the current package and so redraws the prompt after ,in-package.
;;; ,quit answers NIL, which the loop already treats as end of input. Neither
;;; needs a second hook on the C# side.
;;;
;;; A continuation line is left alone. While a form is unfinished the loop
;;; prompts with spaces in place of the prompt, and on such a line a leading
;;; comma is ordinary input: it is how an unquote is written inside a backquoted
;;; form typed across several lines.

(defun continuation-prompt-p (prompt)
  "True for the blank prompt the read loop uses while a form is unfinished."
  (and (plusp (length prompt))
       (every (lambda (ch) (char= ch #\Space)) prompt)))

(defun answer-line (line mode continuation)
  "What READLINE answers the read loop for LINE, typed in MODE (NIL for Lisp)
on a CONTINUATION line or not. A line in a mode, and a comma command, run here,
and the loop is answered with a blank line, which it skips."
  (cond ((null line) nil)
        (mode
         (if (eq (handler-case (funcall (line-mode-run mode) line)
                   (error (condition)
                     (format *error-output* "~A~%"
                             (paint :error (format nil "; ~A: ~A"
                                                   (line-mode-label mode) condition)
                                    *error-output*))
                     nil))
                 :quit)
             nil
             ""))
        (continuation line)
        (t (case (dispatch line)
             (:not-a-command line)
             (:quit nil)
             (t "")))))

(defun readline (prompt)
  "Read a line with editing, running it instead when it is a comma command or
was typed in a line mode.

Returns the line, or NIL on EOF (Ctrl+D), on thread interruption and on ,quit,
or the empty string once a command or a mode's line has run. Signals an error
when there is no console."
  (let ((continuation (continuation-prompt-p prompt)))
    (multiple-value-bind (line mode) (read-line-edited prompt (not continuation))
      (answer-line line mode continuation))))

(defun enable ()
  "Wire dotcl-repl:readline into the REPL read loop.
The history of earlier sessions is read here rather than when this file is
loaded, so that requiring the contrib to call one function out of it does not
touch the file."
  (load-history)
  (dotcl::%set-repl-readline-hook #'readline)
  (dotcl::%set-debugger-read-hook #'debugger-read))

(defun disable ()
  "Restore the default Console.ReadLine-based REPL read.
Bracketed paste goes off with it, in case something ended an edit without
unwinding through the place that normally turns it off."
  (ignore-errors (write-str *bracketed-paste-off*))
  (dotcl::%set-debugger-read-hook nil)
  (dotcl::%set-repl-readline-hook nil))

;;; -- Default completer -------------------------------------------------------
;;;
;;; TAB is worth nothing without a completer, and requiring every reader to
;;; write one is a poor trade for a bundled REPL. dotcl-lsp-api answers exactly
;;; the shape *completer* wants, so wire it here when it is available and leave
;;; TAB inert when it is not.
;;;
;;; Command names are answered here instead of being delegated, since a command
;;; line is not Lisp and no Lisp completer can say anything useful about one.
;;; Everything else is passed straight on, so installing dotcl-lsp-api still
;;; gets the whole of what it knows.

(defvar *base-completer* nil
  "Where COMPLETE-LINE sends everything that is not a command name.")

(defun command-name-completions (text offset)
  "Candidates for the command name a line opens with, or NIL when the cursor is
not inside one."
  (when (and (plusp (length text)) (char= (char text 0) #\,))
    (let ((end (or (position-if #'whitespace-char-p text :start 1)
                   (length text))))
      (when (<= offset end)
        (let* ((typed (string-downcase (subseq text 1 end)))
               (items (loop for c in *command-list*
                            append (loop for name in (cons (command-name c)
                                                           (command-aliases c))
                                         when (and (<= (length typed) (length name))
                                                   (string= typed name
                                                            :end2 (length typed)))
                                           collect (list :label name
                                                         :detail (command-summary c))))))
          (when items
            ;; START is 1, not 0: the comma stays where it is and only the name
            ;; is replaced.
            (list :start 1 :end end :items items)))))))

(defun complete-line (text offset)
  "Command names on a command line, the installed completer on any other."
  (or (command-name-completions text offset)
      (and *base-completer* (funcall *base-completer* text offset))))

(defun install-default-completer ()
  "Set *COMPLETER*, and return T when dotcl-lsp-api is behind it."
  (when (null *completer*)
    (ignore-errors (require "dotcl-lsp-api"))
    (let* ((package (find-package "DOTCL-LSP-API"))
           (symbol (and package (find-symbol "COMPLETIONS" package))))
      (when (and symbol (fboundp symbol))
        (setf *base-completer* (symbol-function symbol))))
    ;; Installed even with no base completer: command names are completable on
    ;; their own, and TAB doing something is the point.
    (setf *completer* #'complete-line)
    (and *base-completer* t)))

(install-default-completer)

(provide "dotcl-repl")
