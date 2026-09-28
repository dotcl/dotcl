;;; The debugger's :frames: the backtrace offered as a menu under the prompt.
;;;
;;; The arrows (or digits, as :frame N takes them) mark a frame and Enter
;;; selects it the way :frame N does; the row is left reading ":frame N".
;;; Escape, Ctrl+C and Ctrl+D close the menu without changing the selection,
;;; and any other key starts an ordinary line with that key in it. Without a
;;; menu the debugger prints the backtrace as :bt does.
;;;
;;; FRAME-MENU reads keys and writes through functions it is given, so it is
;;; run here on a script of keys, without a terminal.

(require "dotcl-repl")

(defun rfm-esc (s)
  "S with ESC spelled as <E>, CR as <R> and LF as <N>."
  (with-output-to-string (out)
    (loop for ch across s
          do (case ch
               (#\Escape (write-string "<E>" out))
               (#\Return (write-string "<R>" out))
               (#\Newline (write-string "<N>" out))
               (t (write-char ch out))))))

(defun rfm-labels (count &optional (current 0))
  "Backtrace labels the way the debugger makes them."
  (loop for i below count
        collect (format nil "~A ~2D: (FRAME-~D ~D)"
                        (if (= i current) "-->" "   ") i i i)))

(defun rfm-run (labels keys &rest options)
  "FRAME-MENU at the prompt \"0] \" over LABELS on the keys KEYS. Returns the
answer and what was written, escapes spelled out."
  (let ((out (make-string-output-stream)))
    (let ((answer
            (apply #'dotcl-repl::frame-menu "0] " labels
                   (or (getf options :selected) 0)
                   :usable (getf options :usable t)
                   :read-key (lambda () (or (pop keys) :eof))
                   :write (lambda (s) (write-string s out))
                   :painter (lambda (s) (concatenate 'string "[" s "]"))
                   :width (getf options :width 80)
                   :height (getf options :height 24)
                   '())))
      (list answer (rfm-esc (get-output-stream-string out))))))

(defun rfm-count (needle s)
  (loop with start = 0
        for at = (search needle s :start2 start)
        while at
        count t
        do (setf start (1+ at))))

(defun rfm-ends-with (suffix s)
  (and (>= (length s) (length suffix))
       (string= suffix s :start2 (- (length s) (length suffix)))))

;;; -- Choosing -------------------------------------------------------------------

;;; Down and Enter: the next frame, and the row reads as the command typed.
(deftest rfm-down-enter
  (let ((r (rfm-run (rfm-labels 3) (list :down :enter))))
    (list (first r)
          (and (search "[> -->  0: (FRAME-0 0)]" (second r)) t)
          (and (search "[>      1: (FRAME-1 1)]" (second r)) t)
          (rfm-ends-with "<E>[G<E>[3C<E>[J:frame 1<N>" (second r))))
  (1 t t t))

;;; The mark starts on the frame already selected, so Enter alone keeps it.
(deftest rfm-starts-on-selected
  (let ((r (rfm-run (rfm-labels 5 3) (list :enter) :selected 3)))
    (list (first r)
          (and (search "[> -->  3: (FRAME-3 3)]" (second r)) t)))
  (3 t))

;;; Digits choose by number, two of them past ten frames.
(deftest rfm-digits
  (list (first (rfm-run (rfm-labels 15) (list #\1 #\2 :enter)))
        (first (rfm-run (rfm-labels 5) (list #\4 :enter)))
        ;; 9 names no frame of five and is refused; 2 then does.
        (first (rfm-run (rfm-labels 5) (list #\9 #\2 :enter))))
  (12 4 2))

(deftest rfm-digits-shown
  (and (search "<E>[G<E>[3C1<E>[J"
               (second (rfm-run (rfm-labels 15) (list #\1 #\2 :enter))))
       t)
  t)

;;; -- Closing without a choice -----------------------------------------------------

;;; Escape (and Ctrl+C, Ctrl+G) and Ctrl+D close the menu, leave no :frame row,
;;; and take the prompt row away for the debugger to write again.
(deftest rfm-cancel
  (let ((esc (rfm-run (rfm-labels 3) (list :down :cancel)))
        (eof (rfm-run (rfm-labels 3) (list :down :eof))))
    (list (first esc) (first eof)
          (search ":frame" (second esc))
          (rfm-ends-with "<E>[G<E>[3C<E>[J<E>[G<E>[J" (second esc))
          (rfm-ends-with "<E>[G<E>[3C<E>[J<E>[G<E>[J" (second eof))))
  (:cancel :cancel nil t t))

;;; Another key starts a line with it, after any digits typed.
(deftest rfm-other
  (list (first (rfm-run (rfm-labels 3) (list :down #\:)))
        (first (rfm-run (rfm-labels 3) (list #\1 #\+)))
        (rfm-ends-with "<E>[G<E>[J" (second (rfm-run (rfm-labels 3) (list #\()))))
  ((:other ":") (:other "1+") t))

;;; No menu where none can be drawn: nothing written, and the debugger prints
;;; the backtrace instead.
(deftest rfm-no-menu
  (list (rfm-run (rfm-labels 3) (list :enter) :usable nil)
        (rfm-run '() (list :enter)))
  ((:no-menu "") (:no-menu "")))

;;; -- Many frames, long frames ------------------------------------------------------

;;; Thirty frames: ten rows at a time, End scrolls to the last.
(deftest rfm-scrolls
  (let ((r (rfm-run (rfm-labels 30) (list :end :enter))))
    (list (first r)
          (and (search "[>     29: (FRAME-29 29)]" (second r)) t)
          (search "FRAME-19 " (second r) :start2 (search "<E>[J" (second r) :from-end t))
          (rfm-count "<E>[10A" (second r))))
  (29 t nil 2))

;;; A short window: the menu keeps the prompt row and one more free.
(deftest rfm-short-window
  (let ((r (rfm-run (rfm-labels 30) (list :enter) :height 6)))
    (list (first r)
          (and (search "<E>[4A" (second r)) t)
          (search "<E>[5A" (second r))))
  (0 t nil))

;;; A long call form is cut to leave the last column free.
(deftest rfm-long-frame-cut
  (let* ((long (format nil "-->  0: (FRAME ~A)" (make-string 200 :initial-element #\x)))
         (r (rfm-run (list long "     1: (OTHER)") (list :down :enter) :width 40))
         (row (let* ((s (second r))
                     (start (+ (search "<N>  -->" s) 3)))
                (subseq s start (search "<R>" s :start2 start)))))
    (list (first r) (length row) (rfm-ends-with "..." row)))
  (1 39 t))
