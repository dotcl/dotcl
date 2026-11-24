;;; SUBSEQ of a string a program has written to (SETF CHAR, SETF AREF, REPLACE, FILL,
;;; NSTRING-UPCASE) takes only the piece asked for. It used to build the whole string
;;; again first, so a 5-character piece of a long buffer cost the buffer's length each time.

(defun %sws-written (n)
  (let ((s (make-string n :initial-element #\a)))
    (dotimes (i n) (setf (char s i) (code-char (+ 65 (mod i 26)))))
    s))

(deftest subseq-written-string.pieces
  (let ((s (%sws-written 60)) (p (make-string 10 :initial-element #\-)))
    (setf (aref p 3) #\X)
    (list (subseq s 0 5) (subseq s 26 31) (subseq s 55) (subseq s 60) (subseq s 0 0)
          (subseq p 2 5) (subseq p 0) (length (subseq s 10 40))
          (let ((q (copy-seq "hello world"))) (replace q "HELLO") (subseq q 0 8))
          (let ((q (copy-seq "abcdef"))) (nstring-upcase q :start 2) (subseq q 1 5))
          (let ((q (copy-seq "abcdef"))) (fill q #\z :start 4) (subseq q 3))
          (subseq (let ((q (make-string 3))) (setf (char q 0) #\a (char q 1) #\b (char q 2) #\c) q) 1 3)))
  ("ABCDE" "ABCDE" "DEFGH" "" ""
   "-X-" "---X------" 30 "HELLO wo" "bCDE" "dzz" "bc"))

(deftest subseq-written-string.bounds-and-errors
  (let ((s (%sws-written 20)))
    (list (handler-case (subseq s 5 4) (error () :error))
          (handler-case (subseq s 0 21) (error () :error))
          (handler-case (subseq s 21) (error () :error))
          (handler-case (subseq s -1) (error () :error))
          (handler-case (subseq s 1.5) (error () :error))
          (subseq s 20) (subseq s 19 20)))
  (:error :error :error :error :error "" "T"))

(deftest subseq-written-string.the-piece-is-a-new-string
  (let* ((s (%sws-written 12)) (p (subseq s 2 6)))
    (setf (char p 0) #\!)
    (setf (char s 3) #\?)
    (list p (subseq s 2 6) (equal (subseq s 0 12) s) (eq (subseq s 0 12) s)
          (typep p '(simple-array character (*)))))
  ("!DEF" "C?EF" t nil t))

(defun %sws-bytes (s) (min-allocated-bytes (lambda () (dotimes (i 1000) (subseq s 100 105))) 5))

(deftest-compiled-only subseq-written-string.allocation-does-not-depend-on-the-string
  (let ((small (%sws-bytes (%sws-written 200))) (big (%sws-bytes (%sws-written 50000))))
    (list (< big (* 1000 150)) (< (abs (- big small)) 4000)))
  (t t))
