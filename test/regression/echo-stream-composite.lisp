;;; regression: skip-under-interp
;;; An echo stream echoes what is taken from ITS input, wherever it sits: inside a
;;; concatenated stream, as the input half of a two-way stream, under a synonym stream,
;;; inside another echo stream. Only the characters read from the echo stream are echoed;
;;; those of the next component of a concatenated stream are not. The expected values are
;;; SBCL's (CLISP agrees) except for PEEK-CHAR, see below.
;;;
;;; PEEK-CHAR: CLHS does not say whether a peeked character is echoed. SBCL echoes it at
;;; the peek, CLISP when it is read; dotcl follows CLISP (echo = consumed).

(defun %ecs-echo (str)
  (let ((o (make-string-output-stream)))
    (values (make-echo-stream (make-string-input-stream str) o) o)))

(defun %ecs-out (o) (get-output-stream-string o))

(deftest echo-stream-composite.concat-echo-first
  (multiple-value-bind (e o) (%ecs-echo "ab")
    (let ((c (make-concatenated-stream e (make-string-input-stream "cd"))))
      (list (read-char c) (read-char c) (read-char c) (read-char c) (%ecs-out o))))
  (#\a #\b #\c #\d "ab"))

(deftest echo-stream-composite.concat-echo-second
  (multiple-value-bind (e o) (%ecs-echo "cd")
    (let ((c (make-concatenated-stream (make-string-input-stream "ab") e)))
      (list (read-char c) (read-char c) (read-char c) (%ecs-out o))))
  (#\a #\b #\c "c"))

(deftest echo-stream-composite.concat-read-line-across-boundary
  (multiple-value-bind (e o) (%ecs-echo "ab")
    (let ((c (make-concatenated-stream e (make-string-input-stream "cd"))))
      (list (read-line c) (%ecs-out o))))
  ("abcd" "ab"))

(deftest echo-stream-composite.concat-peek-echoes-when-consumed
  (multiple-value-bind (e o) (%ecs-echo "ab")
    (let ((c (make-concatenated-stream e)))
      (let* ((peeked (peek-char nil c)) (after-peek (%ecs-out o))
             (read (read-char c)))
        (list peeked after-peek read (%ecs-out o)))))
  (#\a "" #\a "a"))

(deftest echo-stream-composite.concat-unread-not-echoed-again
  (multiple-value-bind (e o) (%ecs-echo "ab")
    (let* ((c (make-concatenated-stream e)) (x (read-char c)))
      (unread-char x c)
      (list (read-char c) (read-char c) (%ecs-out o))))
  (#\a #\b "ab"))

(deftest echo-stream-composite.two-way-input-echo
  (multiple-value-bind (e o) (%ecs-echo "ab")
    (let ((tw (make-two-way-stream e (make-string-output-stream))))
      (list (read-char tw) (peek-char nil tw) (read-line tw) (%ecs-out o))))
  (#\a #\b "b" "ab"))

(deftest echo-stream-composite.two-way-unread
  (multiple-value-bind (e o) (%ecs-echo "ab")
    (let* ((tw (make-two-way-stream e (make-string-output-stream))) (x (read-char tw)))
      (unread-char x tw)
      (list (read-char tw) (%ecs-out o))))
  (#\a "a"))

(deftest echo-stream-composite.echo-over-concat-over-echo
  (multiple-value-bind (e o) (%ecs-echo "ab")
    (let* ((o2 (make-string-output-stream))
           (e2 (make-echo-stream (make-concatenated-stream e) o2)))
      (list (read-char e2) (read-char e2) (%ecs-out o) (%ecs-out o2))))
  (#\a #\b "ab" "ab"))

(defvar *ecs-syn* nil)
(deftest echo-stream-composite.synonym-of-concat-of-echo
  (multiple-value-bind (e o) (%ecs-echo "ab")
    (let* ((c (make-concatenated-stream e)) (s (make-synonym-stream '*ecs-syn*)))
      (setq *ecs-syn* c)
      (list (read-char s) (%ecs-out o))))
  (#\a "a"))

(deftest echo-stream-composite.read-sequence-across-boundary
  (multiple-value-bind (e o) (%ecs-echo "ab")
    (let ((c (make-concatenated-stream e (make-string-input-stream "cd"))) (v (make-string 4)))
      (list (read-sequence v c) v (%ecs-out o))))
  (4 "abcd" "ab"))

(deftest echo-stream-composite.lisp-reader-through-concat
  (multiple-value-bind (e o) (%ecs-echo "(a b) c")
    (let ((c (make-concatenated-stream e)))
      (list (read c) (%ecs-out o))))
  ((a b) "(a b) "))

(deftest echo-stream-composite.echo-of-two-way-of-echo
  (multiple-value-bind (e o) (%ecs-echo "ab")
    (let* ((tw (make-two-way-stream e (make-string-output-stream)))
           (o2 (make-string-output-stream))
           (e2 (make-echo-stream tw o2)))
      (list (read-char e2) (%ecs-out o) (%ecs-out o2))))
  (#\a "a" "a"))

(deftest echo-stream-composite.concat-of-two-echoes
  (multiple-value-bind (e1 o1) (%ecs-echo "ab")
    (multiple-value-bind (e2 o2) (%ecs-echo "cd")
      (let ((c (make-concatenated-stream e1 e2)))
        (list (read-line c) (%ecs-out o1) (%ecs-out o2)))))
  ("abcd" "ab" "cd"))

;;; A closed echo output does not make PEEK-CHAR fail (it echoes nothing).
(deftest echo-stream-composite.peek-with-closed-output
  (multiple-value-bind (e o) (%ecs-echo "ab")
    (close o)
    (list (peek-char nil e) (peek-char t e)))
  (#\a #\a))

;;; Echoed characters move the line state of the echo's output, through any wrappers.
(defvar *ecs-syn2* nil)
(deftest echo-stream-composite.fresh-line-sees-echoed-text
  (multiple-value-bind (e o) (%ecs-echo "ab")
    (let ((s (make-synonym-stream '*ecs-syn2*)))
      (setq *ecs-syn2* e)
      (read-char s)
      (list (fresh-line s) (%ecs-out o))))
  (t "a
"))
