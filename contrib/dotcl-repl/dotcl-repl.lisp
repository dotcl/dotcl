;;; dotcl-repl.lisp — Terminal readline for dotcl
;;;
;;; Usage: (require "dotcl-repl")
;;;        (dotcl-repl:readline "CL-USER> ")
;;;
;;; Features:
;;;   - Character-by-character raw terminal input via System.Console
;;;   - Left/right/home/end cursor movement
;;;   - Backspace, Delete
;;;   - Up/down arrow history
;;;   - Tab completion hook (*completer*)
;;;   - CJK-aware display width (wide chars counted as 2 columns)

(defpackage :dotcl-repl
  (:use :cl)
  (:export #:readline
           #:*history*
           #:*history-max*
           #:*completer*
           #:enable
           #:disable))

(in-package :dotcl-repl)

;;; ── Public state ────────────────────────────────────────────────────────────

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

;;; ── East Asian Width ────────────────────────────────────────────────────────

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

;;; ── System.Console wrappers ─────────────────────────────────────────────────

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

(defun key-char (ki)
  (dotnet:invoke ki "KeyChar"))

(defun key-key (ki)
  (dotnet:invoke ki "Key"))

(defun key-modifiers (ki)
  (dotnet:invoke ki "Modifiers"))

(defun console-key= (ki name)
  (string= (dotnet:invoke (key-key ki) "ToString") name))

(defun key-ctrl-p (ki)
  (let ((mods (dotnet:invoke (key-modifiers ki) "ToString")))
    (search "Control" mods)))

(defun cursor-col ()
  (dotnet:static "System.Console" "get_CursorLeft"))

(defun set-cursor-col (n)
  (dotnet:static "System.Console" "set_CursorLeft" n))

(defun cursor-row ()
  (dotnet:static "System.Console" "get_CursorTop"))

(defun write-str (s)
  (dotnet:static "System.Console" "Write" s))

(defun write-ch (ch)
  (dotnet:static "System.Console" "Write" ch))

;;; ── Display helpers ─────────────────────────────────────────────────────────

(defun terminal-width ()
  (let ((w (dotnet:static "System.Console" "get_WindowWidth")))
    (if (and (numberp w) (> w 0)) w 80)))

(defun move-to-col (col)
  "Set cursor to column COL, handling line wrapping."
  (let ((w (terminal-width)))
    (set-cursor-col (mod col w))))

;;; Redraws the line from the start column of the prompt end.
;;; prompt-col: column where input starts (after prompt).
;;; buf: character list (left to right).
;;; point: cursor position in buf (0-based char index).
(defun redraw (prompt-col buf point)
  (let* ((w (terminal-width))
         (start-row (cursor-row))
         ;; Go back to prompt start column
         (content (coerce buf 'string))
         (before-point (subseq content 0 point))
         (display-before (string-display-width before-point))
         (cursor-abs-col (+ prompt-col display-before))
         (cursor-abs-row (+ start-row (floor cursor-abs-col w))))
    ;; Move to prompt column on current row basis
    (set-cursor-col prompt-col)
    ;; Clear to end of line (and beyond if multiline — simple: rewrite)
    (write-str content)
    ;; Clear remainder
    (let* ((total-display (string-display-width content))
           (total-abs (+ prompt-col total-display))
           (end-col (mod total-abs w)))
      (dotnet:static "System.Console" "Write"
                     (make-string (max 0 (- w end-col)) :initial-element #\Space)))
    ;; Reposition cursor
    (set-cursor-col (mod cursor-abs-col w))
    cursor-abs-row))

;;; ── History ─────────────────────────────────────────────────────────────────

(defun history-push (line)
  (when (and (> (length line) 0)
             (not (equal line (car *history*))))
    (push line *history*)
    (when (> (length *history*) *history-max*)
      (setf *history* (subseq *history* 0 *history-max*)))))

;;; ── Completion ──────────────────────────────────────────────────────────────

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
  "Return (new-buf new-point items-to-show) after tab completion, or NIL.

The completer sees the whole line and the cursor, and says which span its
candidates replace, so completing inside a string literal works the same as
completing a symbol. A unique candidate is inserted; several are extended as far
as they agree and then listed, the way a shell does it."
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
                     (when (rest items) items))))
            ;; Nothing more to insert, but the reader deserves to see what the
            ;; choices are -- with .NET members the signature is the point.
            ((rest items) (list buf point items))
            (t nil)))))))

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

;;; ── Main readline ───────────────────────────────────────────────────────────

(defun readline (prompt)
  "Read a line with editing. Returns the string, or NIL on EOF (Ctrl+D)
   or thread interruption."
  (write-str prompt)
  (let ((prompt-col (cursor-col))
        (buf '())          ; list of chars, left-to-right
        (point 0)          ; insertion point (0 = before first char)
        (hist-idx -1)      ; -1 = current input
        (saved-buf '()))   ; saved buf when browsing history

    (loop
      ;; Fetch next key in an interruptable manner.
      (let* ((ki (console-read-key-interruptable))
             (ch (when ki (key-char ki))))
        ;; If console-read-key-interruptable returned nil, the thread was interrupted
        ;; (e.g., a ThreadInterruptedException was trapped). Break the loop and return nil.
        (unless ki
          (return nil))

        (cond
          ;; Enter
          ((console-key= ki "Enter")
           (write-str (format nil "~%"))
           (let ((line (coerce buf 'string)))
             (history-push line)
             (return line)))

          ;; Ctrl+D — EOF
          ((and (console-key= ki "D") (key-ctrl-p ki))
           (when (null buf)
             (write-str (format nil "~%"))
             (return nil)))

          ;; Ctrl+C — clear line
          ((and (console-key= ki "C") (key-ctrl-p ki))
           (write-str "^C")
           (write-str (format nil "~%"))
           (write-str prompt)
           (setf buf '() point 0 hist-idx -1 saved-buf '()))

          ;; Backspace
          ((or (console-key= ki "Backspace")
               (and (console-key= ki "H") (key-ctrl-p ki)))
           (when (> point 0)
             (setf buf (append (subseq buf 0 (1- point))
                               (subseq buf point)))
             (decf point)
             (redraw prompt-col buf point)))

          ;; Delete
          ((console-key= ki "Delete")
           (when (< point (length buf))
             (setf buf (append (subseq buf 0 point)
                               (subseq buf (1+ point))))
             (redraw prompt-col buf point)))

          ;; Left arrow
          ((or (console-key= ki "LeftArrow")
               (and (console-key= ki "B") (key-ctrl-p ki)))
           (when (> point 0)
             (decf point)
             (let* ((before (coerce (subseq buf 0 point) 'string))
                    (col (mod (+ prompt-col (string-display-width before))
                              (terminal-width))))
               (set-cursor-col col))))

          ;; Right arrow
          ((or (console-key= ki "RightArrow")
               (and (console-key= ki "F") (key-ctrl-p ki)))
           (when (< point (length buf))
             (incf point)
             (let* ((before (coerce (subseq buf 0 point) 'string))
                    (col (mod (+ prompt-col (string-display-width before))
                              (terminal-width))))
               (set-cursor-col col))))

          ;; Home / Ctrl+A
          ((or (console-key= ki "Home")
               (and (console-key= ki "A") (key-ctrl-p ki)))
           (setf point 0)
           (set-cursor-col prompt-col))

          ;; End / Ctrl+E
          ((or (console-key= ki "End")
               (and (console-key= ki "E") (key-ctrl-p ki)))
           (setf point (length buf))
           (let* ((before (coerce buf 'string))
                  (col (mod (+ prompt-col (string-display-width before))
                            (terminal-width))))
             (set-cursor-col col)))

          ;; Up arrow — history previous
          ((console-key= ki "UpArrow")
           (let ((history *history*)
                 (next-idx (1+ hist-idx)))
             (when (< next-idx (length history))
               (when (= hist-idx -1)
                 (setf saved-buf buf))
               (setf hist-idx next-idx)
               (setf buf (coerce (nth hist-idx history) 'list))
               (setf point (length buf))
               (redraw prompt-col buf point))))

          ;; Down arrow — history next
          ((console-key= ki "DownArrow")
           (cond
             ((> hist-idx 0)
              (decf hist-idx)
              (let ((history *history*))
                (setf buf (coerce (nth hist-idx history) 'list))
                (setf point (length buf))
                (redraw prompt-col buf point)))
             ((= hist-idx 0)
              (setf hist-idx -1)
              (setf buf saved-buf)
              (setf point (length buf))
              (redraw prompt-col buf point))))

          ;; Tab — completion
          ((console-key= ki "Tab")
           (let ((result (complete buf point)))
             (when result
               (setf buf (first result)
                     point (second result))
               (let ((items (third result)))
                 (cond (items
                        ;; The listing scrolls the line away, so put the prompt
                        ;; and the input back underneath it.
                        (show-completions items)
                        (write-str prompt)
                        (redraw prompt-col buf point))
                       (t (redraw prompt-col buf point)))))))

          ;; Ctrl+K — kill to end of line
          ((and (console-key= ki "K") (key-ctrl-p ki))
           (setf buf (subseq buf 0 point))
           (redraw prompt-col buf point))

          ;; Ctrl+U — kill to beginning
          ((and (console-key= ki "U") (key-ctrl-p ki))
           (setf buf (subseq buf point)
                 point 0)
           (redraw prompt-col buf point))

          ;; Printable character
          ((and (characterp ch)
                (graphic-char-p ch))
           (setf buf (append (subseq buf 0 point)
                             (list ch)
                             (subseq buf point)))
           (incf point)
           (redraw prompt-col buf point)))))))

;;; ── REPL integration ────────────────────────────────────────────────────────

(defun enable ()
  "Wire dotcl-repl:readline into the REPL read loop."
  (dotcl::%set-repl-readline-hook #'readline))

(defun disable ()
  "Restore the default Console.ReadLine-based REPL read."
  (dotcl::%set-repl-readline-hook nil))

;;; ── Default completer ───────────────────────────────────────────────────────
;;;
;;; TAB is worth nothing without a completer, and requiring every reader to
;;; write one is a poor trade for a bundled REPL. dotcl-lsp-api answers exactly
;;; the shape *completer* wants, so wire it here when it is available and leave
;;; TAB inert when it is not.

(defun install-default-completer ()
  "Set *COMPLETER* from dotcl-lsp-api, and return T when one was installed."
  (when (null *completer*)
    (ignore-errors (require "dotcl-lsp-api"))
    (let* ((package (find-package "DOTCL-LSP-API"))
           (symbol (and package (find-symbol "COMPLETIONS" package))))
      (when (and symbol (fboundp symbol))
        (setf *completer* (symbol-function symbol))
        t))))

(install-default-completer)

(provide "dotcl-repl")
