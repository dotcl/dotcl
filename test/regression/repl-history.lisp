;;; The bundled REPL's history: the file it survives in, the Ctrl+R search over
;;; it, and the word motions that make a long recalled form editable.
;;;
;;; The history used to live only in memory, so the one thing rlwrap still had
;;; over this editor was that it remembered what you typed yesterday. A file
;;; brings two questions with it -- what happens when two sessions write to it,
;;; and what happens when one of them dies mid-write -- and both are answered
;;; by writing whole lines, one per entry, as they are accepted. That makes
;;; every part of it a function of a string, so all of it is checked here
;;; without a terminal and without a second process.

(require "dotcl-repl")

(defvar *rh-tmp*
  (let ((dir (concatenate 'string
                          (regression-temp-dir)
                          "/dotcl-repl-history-test/")))
    (ensure-directories-exist dir)
    dir))

(defun rh-file (name)
  "A path under this test's own directory, with nothing at it."
  (let ((path (pathname (concatenate 'string *rh-tmp* name))))
    (ignore-errors (delete-file path))
    path))

(defun rh-write (path text)
  "Put TEXT at PATH exactly, control characters and all."
  (dotnet:static "System.IO.File" "WriteAllText" (namestring path) text)
  path)

(defun rh-read (path)
  (dotcl-repl::read-history-text path))

;;; A session: its own *HISTORY*, pointed at PATH, recording LINES in order.
;;; This is what a second REPL amounts to as far as the file is concerned,
;;; since each of them only ever appends whole lines to it.
(defun rh-session (path lines &key (start '()))
  (let ((dotcl-repl::*history-file* path)
        (dotcl-repl::*history* start))
    (dolist (line lines)
      (dotcl-repl::history-record line))
    dotcl-repl::*history*))

;;; -- The line format ---------------------------------------------------------

;;; A form typed over several lines is one entry, and comes back as the lines
;;; it was written on.
(deftest rh-multi-line-entry-round-trips
  (let ((form (format nil "(defun add1 (x)~%  (+ x 1))")))
    (equal (dotcl-repl::decode-history-entry
            (dotcl-repl::encode-history-entry form))
           form))
  t)

;;; Encoded, it is one line, which is what lets the file be appended to and cut
;;; short without an entry ever being confused with another.
(deftest rh-encoded-entry-is-one-line
  (count #\Newline
         (dotcl-repl::encode-history-entry (format nil "a~%b~%c")))
  0)

(deftest rh-backslash-survives
  (let ((text "(format nil \"\\n\")"))
    (equal (dotcl-repl::decode-history-entry
            (dotcl-repl::encode-history-entry text))
           text))
  t)

;;; A backslash at the very end has nothing to escape and stands for itself.
(deftest rh-trailing-backslash-decodes-to-itself
  (dotcl-repl::decode-history-entry "a\\")
  "a\\")

;;; -- Reading a file ----------------------------------------------------------

(deftest rh-empty-file-is-no-history
  (dotcl-repl::parse-history-file "")
  nil)

(deftest rh-entries-come-back-oldest-first
  (dotcl-repl::parse-history-file (format nil "one~%two~%three~%"))
  ("one" "two" "three"))

;;; An entry and its newline are written in one call, so a line with no
;;; newline after it is a write that did not finish.
(deftest rh-unterminated-last-line-is-dropped
  (dotcl-repl::parse-history-file (format nil "one~%two~%half"))
  ("one" "two"))

;;; A file that was cut short inside a block comes back with a run of NULs in
;;; it. No control character survives encoding, so such a line is damage.
(deftest rh-line-with-control-characters-is-dropped
  (dotcl-repl::parse-history-file
   (format nil "one~%~C~C~C~%two~%" (code-char 0) (code-char 0) (code-char 0)))
  ("one" "two"))

(deftest rh-blank-lines-are-dropped
  (dotcl-repl::parse-history-file (format nil "one~%~%two~%"))
  ("one" "two"))

;;; A file someone has opened in an editor that writes CRLF still reads.
(deftest rh-carriage-returns-are-tolerated
  (dotcl-repl::parse-history-file (format nil "one~C~%two~C~%" #\Return #\Return))
  ("one" "two"))

;;; *HISTORY* is newest first, and holds at most *HISTORY-MAX* of them however
;;; long the file is.
(deftest rh-history-is-newest-first
  (dotcl-repl::history-from-file-text (format nil "one~%two~%three~%") 10)
  ("three" "two" "one"))

(deftest rh-history-stops-at-the-limit
  (dotcl-repl::history-from-file-text
   (format nil "one~%two~%three~%four~%five~%") 3)
  ("five" "four" "three"))

;;; -- Writing a file ----------------------------------------------------------

(deftest rh-what-is-recorded-comes-back
  (let ((path (rh-file "round-trip")))
    (rh-session path '("(+ 1 2)" "(defun f () 1)"))
    (dotcl-repl::history-from-file-text (rh-read path) 500))
  ("(defun f () 1)" "(+ 1 2)"))

(deftest rh-a-multi-line-form-comes-back-whole
  (let ((path (rh-file "multi-line"))
        (form (format nil "(defun add1 (x)~%  (+ x 1))")))
    (rh-session path (list form))
    (equal (first (dotcl-repl::history-from-file-text (rh-read path) 500))
           form))
  t)

;;; The line before is not written again, which is the rule *HISTORY* already
;;; followed in memory.
(deftest rh-a-repeated-line-is-recorded-once
  (let ((path (rh-file "repeat")))
    (rh-session path '("(+ 1 2)" "(+ 1 2)" "(+ 1 3)"))
    (dotcl-repl::parse-history-file (rh-read path)))
  ("(+ 1 2)" "(+ 1 3)"))

;;; Two REPLs open at once. Each appends its own lines as they are accepted,
;;; so the file ends up with all of them in the order they were typed and
;;; neither session's work is lost. This is the reason nothing is written at
;;; exit: an exit that writes the whole history is an exit that overwrites the
;;; other session's.
(deftest rh-two-sessions-both-keep-what-they-typed
  (let ((path (rh-file "two-sessions")))
    (rh-session path '("first in A"))
    (rh-session path '("first in B"))
    (rh-session path '("second in A"))
    (dotcl-repl::parse-history-file (rh-read path)))
  ("first in A" "first in B" "second in A"))

;;; A session that never exits cleanly has still written everything it
;;; accepted, because it wrote each line when it accepted it.
(deftest rh-a-killed-session-keeps-its-history
  (let ((path (rh-file "killed")))
    ;; No unwinding, no exit hook: just the lines, and then the file.
    (rh-session path '("(+ 1 2)" "(+ 3 4)"))
    (length (dotcl-repl::parse-history-file (rh-read path))))
  2)

;;; The file grows by appending, so it is the read that trims it, and only
;;; once it is over the limit. What is left is the newest *HISTORY-MAX*.
(deftest rh-a-long-file-is-trimmed-when-it-is-read
  (let ((path (rh-file "long"))
        (dotcl-repl::*history-max* 5)
        (dotcl-repl::*history-file-max* 10))
    (rh-write path (with-output-to-string (out)
                     (dotimes (i 20)
                       (format out "line ~D~%" i))))
    (let ((dotcl-repl::*history-file* path)
          (dotcl-repl::*history-loaded* nil)
          (dotcl-repl::*history* '()))
      (dotcl-repl::load-history)
      (values dotcl-repl::*history*
              (dotcl-repl::parse-history-file (rh-read path)))))
  ("line 19" "line 18" "line 17" "line 16" "line 15")
  ("line 15" "line 16" "line 17" "line 18" "line 19"))

;;; Under the limit it is left exactly as it is: a rewrite is the one operation
;;; here that another session could lose an entry to, so it happens as rarely
;;; as it can.
(deftest rh-a-short-file-is-left-alone
  (let ((path (rh-file "short"))
        (dotcl-repl::*history-max* 5)
        (dotcl-repl::*history-file-max* 10))
    (rh-write path (with-output-to-string (out)
                     (dotimes (i 4)
                       (format out "line ~D~%" i))))
    (let ((dotcl-repl::*history-file* path)
          (dotcl-repl::*history-loaded* nil)
          (dotcl-repl::*history* '()))
      (dotcl-repl::load-history)
      (equal (rh-read path)
             (format nil "line 0~%line 1~%line 2~%line 3~%"))))
  t)

;;; Reading it twice in one session does not read it twice: the second call is
;;; what enabling the editor again would do, and it must not double the list.
(deftest rh-the-file-is-read-once
  (let ((path (rh-file "read-once")))
    (rh-session path '("one" "two"))
    (let ((dotcl-repl::*history-file* path)
          (dotcl-repl::*history-loaded* nil)
          (dotcl-repl::*history* '()))
      (dotcl-repl::load-history)
      (dotcl-repl::load-history)
      dotcl-repl::*history*))
  ("two" "one"))

;;; -- Giving up quietly -------------------------------------------------------

;;; A place that cannot be written to is not a reason to interrupt a session.
;;; The path here has an ordinary file where it needs a directory.
(deftest rh-an-unwritable-place-is-not-an-error
  (let* ((blocker (rh-file "blocker"))
         (path (pathname (concatenate 'string (namestring blocker)
                                      "/nope/history"))))
    (rh-write blocker "in the way")
    (let ((dotcl-repl::*history-file* path))
      (dotcl-repl::history-record "(+ 1 2)"))
    ;; It ran, it wrote nothing, and it said nothing.
    (rh-read path))
  nil)

(deftest rh-reading-what-is-not-there-is-not-an-error
  (rh-read (rh-file "absent"))
  nil)

;;; NIL turns the file off altogether, which is what DOTCL_NO_HISTORY does and
;;; what an init file can do.
(deftest rh-history-can-be-turned-off
  (let ((path (rh-file "turned-off")))
    (let ((dotcl-repl::*history-file* nil)
          (dotcl-repl::*history* '()))
      (dotcl-repl::history-record "(+ 1 2)")
      (values dotcl-repl::*history* (rh-read path))))
  ("(+ 1 2)")
  nil)

;;; The file sits beside the init file rather than in the cache tree, because
;;; `dotcl clean` empties the cache tree and a history that a maintenance
;;; command eats is not a history.
(deftest rh-the-default-file-is-beside-the-init-file
  (let ((file (dotcl-repl::default-history-file)))
    ;; NIL where DOTCL_NO_HISTORY is set, which is a legitimate environment to
    ;; run the suite in.
    (or (null file)
        (and (equal (pathname-name file) "history")
             (null (pathname-type file))
             (equal (pathname-directory file)
                    (pathname-directory (dotcl:user-init-file)))
             t)))
  t)

;;; -- Ctrl+R ------------------------------------------------------------------

;;; Newest first, the way *HISTORY* is.
(defparameter *rh-history*
  (list "(third 3)" "(second 2)" "(first 1)" "(+ 1 2)"))

(defun rh-type (state text)
  "Type TEXT into STATE one character at a time."
  (let ((s state))
    (loop for ch across text
          do (setf s (dotcl-repl::isearch-type s ch *rh-history*)))
    s))

(defun rh-state (state)
  "A search as (query index failed), which is all of it that is state."
  (list (dotcl-repl::isearch-query state)
        (dotcl-repl::isearch-index state)
        (and (dotcl-repl::isearch-failed state) t)))

;;; Nothing typed yet: nothing has matched, and the line being edited is what
;;; is on show.
(deftest rh-a-new-search-shows-the-line-being-edited
  (let ((state (dotcl-repl::isearch-start)))
    (values (rh-state state)
            (dotcl-repl::isearch-line state *rh-history* "(in progress")))
  ("" nil nil)
  "(in progress")

;;; A match: the newest entry holding it, and where in it the query sits.
(deftest rh-a-match-is-found
  (let ((state (rh-type (dotcl-repl::isearch-start) "first")))
    (values (rh-state state)
            (dotcl-repl::isearch-line state *rh-history* "")
            (dotcl-repl::isearch-match state)))
  ("first" 2 nil)
  "(first 1)"
  1)

;;; No match: the entry on show stays where it is, and the search says it has
;;; failed rather than throwing away what was found.
(deftest rh-a-failed-search-keeps-the-entry
  (let ((state (rh-type (dotcl-repl::isearch-start) "firstx")))
    (values (rh-state state)
            (dotcl-repl::isearch-line state *rh-history* "")
            (dotcl-repl::isearch-match state)))
  ("firstx" 2 t)
  "(first 1)"
  1)

;;; Typing on past a failure and rubbing it out again comes back to the match.
(deftest rh-backspace-undoes-a-failed-keystroke
  (let ((state (dotcl-repl::isearch-undo
                (rh-type (dotcl-repl::isearch-start) "firstx"))))
    (rh-state state))
  ("first" 2 nil))

;;; Ctrl+R again walks back through the matches, oldest last.
(deftest rh-repeated-ctrl-r-walks-back
  (let* ((one (rh-type (dotcl-repl::isearch-start) "("))
         (two (dotcl-repl::isearch-again one *rh-history*))
         (three (dotcl-repl::isearch-again two *rh-history*)))
    (list (dotcl-repl::isearch-index one)
          (dotcl-repl::isearch-index two)
          (dotcl-repl::isearch-index three)))
  (0 1 2))

;;; Walking off the end says so and stays on the oldest match, rather than
;;; wrapping round to the newest, which is what leaves someone searching the
;;; same three entries for ever without noticing.
(deftest rh-walking-past-the-oldest-match-fails-in-place
  (let ((state (rh-type (dotcl-repl::isearch-start) "second")))
    (rh-state (dotcl-repl::isearch-again state *rh-history*)))
  ("second" 1 t))

;;; And backspace from there steps back to it, because it undoes a Ctrl+R the
;;; same way it undoes a character.
(deftest rh-backspace-undoes-a-ctrl-r
  (let* ((one (rh-type (dotcl-repl::isearch-start) "("))
         (two (dotcl-repl::isearch-again one *rh-history*)))
    (list (dotcl-repl::isearch-index two)
          (dotcl-repl::isearch-index (dotcl-repl::isearch-undo two))))
  (1 0))

;;; Backspace with nothing left to undo stays put rather than coming apart.
(deftest rh-backspace-at-the-start-is-harmless
  (rh-state (dotcl-repl::isearch-undo (dotcl-repl::isearch-start)))
  ("" nil nil))

;;; Accepting puts the entry in the buffer with the cursor on the match.
(deftest rh-accepting-a-search-gives-the-line-and-the-point
  (dotcl-repl::isearch-accept (rh-type (dotcl-repl::isearch-start) "second")
                              *rh-history* "(in progress")
  "(second 2)"
  1)

;;; Accepting a search that never matched leaves the line that was being
;;; edited exactly as it was, with the cursor at its end.
(deftest rh-accepting-with-no-match-keeps-the-line
  (dotcl-repl::isearch-accept (dotcl-repl::isearch-start)
                              *rh-history* "(in prog")
  "(in prog"
  8)

;;; A multi-line form is one entry and is found by searching for any of it.
(deftest rh-a-multi-line-form-can-be-searched-for
  (let* ((form (format nil "(defun add1 (x)~%  (+ x 1))"))
         (history (list form "(+ 1 2)"))
         (state (dotcl-repl::isearch-type (dotcl-repl::isearch-start)
                                          #\d history)))
    (list (dotcl-repl::isearch-index state)
          (equal (dotcl-repl::isearch-line state history "") form)))
  (0 t))

;;; -- What the search looks like ----------------------------------------------

;;; The search prompt is part of the content and not a prompt, because the
;;; prompt is the one thing the redraw never writes a second time and this one
;;; changes width on every keystroke. The point is measured from the start of
;;; the content, so it is past the prompt as well.
(deftest rh-the-search-prompt-is-in-the-content
  (dotcl-repl::isearch-content (rh-type (dotcl-repl::isearch-start) "second")
                               *rh-history* "")
  "(reverse-i-search)`second': (second 2)"
  29)

(deftest rh-a-failed-search-says-so
  (values (dotcl-repl::isearch-content
           (rh-type (dotcl-repl::isearch-start) "secondx") *rh-history* ""))
  "(failed reverse-i-search)`secondx': (second 2)")

;;; The final byte of a control sequence says which operation it is. Collecting
;;; them is how this file asserts that the search draws with the same four
;;; relative movements as everything else, and in particular that it did not
;;; bring back the cursor position report that the redraw was rewritten to get
;;; rid of.
(defun rh-csi-finals (s)
  (let ((finals '())
        (n (length s)))
    (loop for i below (1- n)
          when (and (char= (char s i) #\Escape) (char= (char s (1+ i)) #\[))
            do (let ((j (+ i 2)))
                 (loop while (and (< j n)
                                  (or (digit-char-p (char s j))
                                      (char= (char s j) #\;)))
                       do (incf j))
                 (when (< j n) (pushnew (char s j) finals))))
    (sort finals #'char<)))

(defun rh-render-search (state)
  "What a redraw of STATE writes, on a terminal narrow enough to wrap it."
  (multiple-value-bind (content point)
      (dotcl-repl::isearch-content state *rh-history* "")
    (values (dotcl-repl::render 8 content point 40 0))))

(deftest rh-the-search-draws-with-relative-movement-only
  (rh-csi-finals (rh-render-search
                  (rh-type (dotcl-repl::isearch-start) "second")))
  (#\A #\C #\G #\J))

(deftest rh-the-search-never-asks-for-the-cursor-position
  (search (format nil "~C[6n" #\Escape)
          (rh-render-search (rh-type (dotcl-repl::isearch-start) "second")))
  nil)

;;; A search prompt long enough to wrap is drawn like any other long content,
;;; which is the whole reason for putting it there: the wrapping arithmetic
;;; already existed and did not have to learn about a prompt that moves.
(deftest rh-a-wrapped-search-still-lands-on-the-match
  (let ((state (rh-type (dotcl-repl::isearch-start) "second")))
    (multiple-value-bind (content point)
        (dotcl-repl::isearch-content state *rh-history* "")
      (multiple-value-bind (out row rows)
          (dotcl-repl::render 8 content point 20 0)
        (declare (ignore out))
        (list row rows))))
  (1 3))

;;; -- Words -------------------------------------------------------------------

;;; A hyphen is not a word boundary in a Lisp: a motion that stopped inside
;;; *STANDARD-OUTPUT* would be of no use to anyone.
(deftest rh-a-symbol-is-one-word
  (let ((text "(setf *standard-output* s)"))
    (dotcl-repl::word-backward text 22))
  6)

(deftest rh-word-backward-crosses-the-bracket
  (let ((text "(foo bar)"))
    (list (dotcl-repl::word-backward text 9)    ; from after the )
          (dotcl-repl::word-backward text 8)    ; from before it
          (dotcl-repl::word-backward text 4)))  ; from the end of foo
  (5 5 1))

(deftest rh-word-backward-at-the-start-stays
  (dotcl-repl::word-backward "(foo bar)" 0)
  0)

(deftest rh-word-forward-lands-after-the-token
  (let ((text "(foo bar)"))
    (list (dotcl-repl::word-forward text 0)
          (dotcl-repl::word-forward text 4)
          ;; Nothing but the closing bracket is left, so it stops at the end.
          (dotcl-repl::word-forward text 8)))
  (4 8 9))

(deftest rh-word-motion-crosses-lines
  (let ((text (format nil "(defun f ()~%  (g))")))
    (list (dotcl-repl::word-forward text 10)
          (dotcl-repl::word-backward text 17)))
  (16 15))

;;; Ctrl+W is the other notion: back to the last blank, brackets and all,
;;; because what it takes back is what was just typed.
(deftest rh-ctrl-w-takes-back-to-the-last-blank
  (let ((text "(foo bar)"))
    (list (dotcl-repl::whitespace-backward text 9)
          ;; The opening bracket goes with it: this is the notion of a word
          ;; that has no notion of a bracket.
          (dotcl-repl::whitespace-backward text 4)))
  (5 0))

;;; Trailing blanks go with it rather than stopping it.
(deftest rh-ctrl-w-steps-over-trailing-blanks
  (dotcl-repl::whitespace-backward "(foo bar   " 11)
  5)

(deftest rh-ctrl-w-at-the-start-stays
  (dotcl-repl::whitespace-backward "   " 3)
  0)
