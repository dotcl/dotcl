;;; Reading a child's output line by line: the buffer behind the stream keeps only what
;;; has not been read, and every character is delivered once and in order, whatever the
;;; sizes of the chunks the child's pipe delivers.

(require "asdf")

#-windows
(progn
  (defun %pol-lines (count)
    (let* ((p (uiop:launch-program
               (list "awk" (format nil "BEGIN{for(i=1;i<=~d;i++)print i}" count))
               :output :stream))
           (s (uiop:process-info-output p))
           (n 0) (sum 0) (ok t) (expect 1))
      (loop for line = (read-line s nil) while line
            do (incf n) (incf sum (length line))
               (unless (equal line (princ-to-string expect)) (setq ok nil))
               (incf expect))
      (uiop:wait-process p)
      (list n sum ok)))

  (deftest process-output-large.lines-in-order
    (%pol-lines 200000)
    (200000 1088895 t))

  (deftest process-output-large.small-output
    (%pol-lines 3)
    (3 3 t))

  (deftest process-output-large.mixed-char-and-line-reads
    ;; READ-CHAR and READ-LINE interleaved: every character arrives once.
    (let* ((p (uiop:launch-program
               (list "awk" "BEGIN{for(i=1;i<=50000;i++)print i}") :output :stream))
           (s (uiop:process-info-output p))
           (total 0) (reads 0))
      (loop
        (let ((c (read-char s nil)))
          (unless c (return))
          (incf total)
          (incf reads)
          (when (zerop (mod reads 5))
            (multiple-value-bind (line missing) (read-line s nil)
              (when line (incf total (+ (length line) (if missing 0 1))))))))
      (uiop:wait-process p)
      total)
    288894))
