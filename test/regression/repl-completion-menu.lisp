;;; TAB completion with several candidates offers them as a menu under the
;;; input: the arrows move the mark, Enter or TAB takes it, Escape leaves the
;;; line as it was, and any other key closes the menu and goes on to the line.
;;;
;;; The line editor's loop needs a terminal; what it is made of does not. Here:
;;; the span a chosen candidate replaces, the replacement, the rows, what each
;;; key means to this menu, and the menu itself run on a script of keys.

(require "dotcl-repl")

(defun rcm-complete (text &optional (point (length text)))
  (dotcl-repl::complete (coerce text 'list) point))

;;; -- The span -------------------------------------------------------------------

;;; Several candidates sharing more than what was typed: the shared part is
;;; inserted, and the span covers the token as it now stands.
(deftest rcm-span-after-common-prefix
  (let ((r (rcm-complete "(nstring-")))
    (list (coerce (first r) 'string) (second r) (length (third r)) (fourth r)))
  ("(nstring-" 9 3 (1 . 9)))

(deftest rcm-span-extended
  (let ((r (rcm-complete "(nstring-d" 10)))
    ;; One candidate: inserted, nothing to offer.
    (list (coerce (first r) 'string) (third r)))
  ("(nstring-downcase" nil))

;;; A token with text after the cursor: the span runs to the token's end, so
;;; a chosen candidate replaces all of it.
(deftest rcm-span-cursor-inside-token
  (let ((r (rcm-complete "(nstring-xyz)" 9)))
    (fourth r))
  (1 . 9))

;;; -- Taking a candidate -----------------------------------------------------------

(deftest rcm-choose
  (multiple-value-bind (buf point)
      (dotcl-repl::choose-completion (coerce "(nstring- \"a\")" 'list) '(1 . 9)
                                     '(:label "nstring-upcase"))
    (list (coerce buf 'string) point))
  ("(nstring-upcase \"a\")" 15))

;;; Choosing from the menu of a real completion puts the whole candidate in.
(deftest rcm-choose-from-complete
  (let* ((r (rcm-complete "(nstring-"))
         (item (find "nstring-upcase" (third r)
                     :key (lambda (i) (getf i :label)) :test #'string-equal)))
    (coerce (dotcl-repl::choose-completion (first r) (fourth r) item) 'string))
  "(nstring-upcase")

;;; -- The rows -------------------------------------------------------------------

;;; Details line up after the longest label; an item without one is its label.
(deftest rcm-labels
  (dotcl-repl::completion-menu-labels
   '((:label "Append" :detail "(String)")
     (:label "AppendLine" :detail "()")
     (:label "Clear")))
  ("Append      (String)" "AppendLine  ()" "Clear"))

;;; A label longer than 24 does not push every detail that far right.
(deftest rcm-labels-long
  (let ((rows (dotcl-repl::completion-menu-labels
               (list (list :label (make-string 30 :initial-element #\a) :detail "d")
                     (list :label "b" :detail "e")))))
    (list (length (first rows)) (second rows)))
  (33 "b                         e"))

;;; -- The keys --------------------------------------------------------------------

(deftest rcm-keys
  (list (dotcl-repl::completion-menu-key :ignore "Tab" nil)
        (dotcl-repl::completion-menu-key :ignore "Tab" t)
        (dotcl-repl::completion-menu-key :up "UpArrow" nil)
        (dotcl-repl::completion-menu-key :down "DownArrow" nil)
        (dotcl-repl::completion-menu-key :enter "Enter" nil)
        (dotcl-repl::completion-menu-key :cancel "Escape" nil)
        (dotcl-repl::completion-menu-key :cancel "C" nil)
        (dotcl-repl::completion-menu-key :eof "D" nil)
        (dotcl-repl::completion-menu-key :backspace "Backspace" nil)
        (dotcl-repl::completion-menu-key :ignore "LeftArrow" nil)
        (dotcl-repl::completion-menu-key :ignore "Escape" nil)
        (dotcl-repl::completion-menu-key #\u "U" nil))
  (:enter :up :up :down :enter :cancel :cancel :cancel :pass :pass :ignore #\u))

;;; -- The menu on a script of keys ---------------------------------------------

(defun rcm-esc (s)
  (with-output-to-string (out)
    (loop for ch across s
          do (if (char= ch #\Escape) (write-string "<ESC>" out) (write-char ch out)))))

(defun rcm-run (keys)
  "Run a completion menu of three rows on KEYS, each (menu-key name shift-p),
the way the editor maps them. Returns the action, the value and what was
written."
  (let ((out (make-string-output-stream)))
    (multiple-value-bind (action value)
        (dotcl-repl::run-menu
         '("nstring-capitalize" "nstring-downcase" "nstring-upcase")
         :anchor-col 18
         :read-key (lambda ()
                     (let ((k (pop keys)))
                       (if k (apply #'dotcl-repl::completion-menu-key k) :eof)))
         :write (lambda (s) (write-string s out))
         :width 80 :height 24
         :painter (lambda (s) (concatenate 'string "[" s "]")))
      (list action value (rcm-esc (get-output-stream-string out))))))

;;; Down, then TAB: the second row is taken.
(deftest rcm-run-down-tab
  (subseq (rcm-run '((:down "DownArrow" nil) (:ignore "Tab" nil))) 0 2)
  (:choose 1))

;;; Shift+TAB goes up.
(deftest rcm-run-shift-tab
  (subseq (rcm-run '((:down "DownArrow" nil) (:down "DownArrow" nil)
                     (:ignore "Tab" t) (:enter "Enter" nil)))
          0 2)
  (:choose 1))

;;; Escape: nothing taken, and the menu erased from the anchor.
(deftest rcm-run-escape
  (let ((r (rcm-run '((:cancel "Escape" nil)))))
    (list (first r) (second r)
          (let ((s (third r)))
            (subseq s (- (length s) 23)))))
  (:cancel nil "<ESC>[G<ESC>[18C<ESC>[J"))

;;; A typed character or an arrow along the line closes the menu and is the
;;; caller's to put on the line.
(deftest rcm-run-pass
  (list (subseq (rcm-run '((#\u "U" nil))) 0 1)
        (subseq (rcm-run '((:ignore "LeftArrow" nil))) 0 1)
        (subseq (rcm-run '((:backspace "Backspace" nil))) 0 1))
  ((:other) (:other) (:other)))

;;; Ctrl+D closes the menu and does not end the input.
(deftest rcm-run-ctrl-d
  (first (rcm-run '((:eof "D" nil))))
  :cancel)

;;; What is drawn: the rows under the anchor with the first one marked, and the
;;; cursor back at the anchor, after the text.
(deftest rcm-run-drawn
  (let ((s (third (rcm-run '((:cancel "Escape" nil))))))
    (and (search "<ESC>[G<ESC>[18C<ESC>[J" s)
         (search "[> nstring-capitalize]" s)
         (search "  nstring-downcase" s)
         (search "<ESC>[3A<ESC>[G<ESC>[18C" s)
         t))
  t)
