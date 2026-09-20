;;;; A whitespace tokenizer over a simple-string, against IlParity.Tokenizer.
;;;;
;;;; The only case whose element type is a character rather than an integer, and
;;;; the one that asks what a character costs: the scan reads one element per
;;;; iteration and compares it against constants, so a representation that boxes
;;;; a character pays on every character of the input.
;;;;
;;;; It returns numbers rather than substrings on purpose -- consing the tokens
;;;; would make the case about the allocator instead of about the scan.

(in-package :cl-user)

(defstruct (ilp-tok (:constructor %make-ilp-tok (text pos)))
  (text "" :type simple-string)
  (pos 0 :type fixnum))

(defun ilp-tok-new (text)
  (declare (simple-string text) (optimize (speed 3) (safety 0) (debug 0)))
  (%make-ilp-tok text 0))

(declaim (inline ilp-tok-space-p))
(defun ilp-tok-space-p (c)
  (declare (character c) (optimize (speed 3) (safety 0) (debug 0)))
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline)))

(defun ilp-tok-next (tk)
  "The length of the next token, or -1 at end of input."
  (declare (type ilp-tok tk) (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((s (ilp-tok-text tk))
         (n (length s))
         (i (ilp-tok-pos tk)))
    (declare (simple-string s) (fixnum n i))
    (loop while (and (< i n) (ilp-tok-space-p (schar s i)))
          do (setq i (the fixnum (1+ i))))
    (when (>= i n)
      (setf (ilp-tok-pos tk) i)
      (return-from ilp-tok-next -1))
    (let ((start i))
      (declare (fixnum start))
      (loop while (and (< i n) (not (ilp-tok-space-p (schar s i))))
            do (setq i (the fixnum (1+ i))))
      (setf (ilp-tok-pos tk) i)
      (the fixnum (- i start)))))

(defun ilp-tok-digest (tk)
  "Token count and the sum of the character codes, in one pass. The codes are
   folded in so that a scan which skipped or repeated a character changes the
   answer."
  (declare (type ilp-tok tk) (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((s (ilp-tok-text tk))
         (n (length s))
         (tokens 0)
         (sum 0)
         (i 0))
    (declare (simple-string s) (fixnum n tokens sum i))
    (loop while (< i n)
          do (loop while (and (< i n) (ilp-tok-space-p (schar s i)))
                   do (setq i (the fixnum (1+ i))))
             (when (>= i n) (return))
             (setq tokens (the fixnum (1+ tokens)))
             (loop while (and (< i n) (not (ilp-tok-space-p (schar s i))))
                   do (setq sum (the fixnum (+ sum (char-code (schar s i)))))
                      (setq i (the fixnum (1+ i)))))
    (the fixnum (+ (the fixnum (* tokens 1000003)) sum))))

(defun ilp-tok-selfcheck (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((out (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (dotimes (i n)
      (declare (fixnum i))
      (vector-push-extend (code-char (+ (char-code #\a) (mod i 26))) out)
      (when (= (mod i 5) 4) (vector-push-extend #\Space out))
      (when (= (mod i 17) 16) (vector-push-extend #\Tab out)))
    (let* ((text (coerce out 'simple-string))
           (acc (ilp-tok-digest (ilp-tok-new text)))
           (u (ilp-tok-new text))
           (count 0))
      (declare (simple-string text) (fixnum acc count))
      (loop (let ((len (ilp-tok-next u)))
              (declare (fixnum len))
              (when (< len 0) (return))
              (setq count (the fixnum (+ count len)))))
      (the fixnum (+ acc count)))))
