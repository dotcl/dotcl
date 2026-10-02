;;; An array displaced to a string shares the string's storage (CLHS
;;; MAKE-ARRAY, "displaced array"): a write through either one is seen by the
;;; other.
;;;
;;; Bug: a string target was wrapped in a fresh CHARACTER vector holding a
;;; copy of its characters, so the displaced array and the string drifted
;;; apart after the first write to either.

(deftest displaced-string-sharing.string-to-array
  (let* ((s (copy-seq "Hello world"))
         (d (make-array 5 :element-type 'character :displaced-to s
                          :displaced-index-offset 6)))
    (setf (char s 6) #\W)
    (setf (schar s 10) #\D)
    (copy-seq d))
  "WorlD")

(deftest displaced-string-sharing.array-to-string
  (let* ((s (copy-seq "Hello world"))
         (d (make-array 5 :element-type 'character :displaced-to s)))
    (setf (char d 0) #\J)
    (setf (aref d 4) #\O)
    s)
  "JellO world")

(deftest displaced-string-sharing.index-offset
  (let* ((s (copy-seq "abcdefgh"))
         (d (make-array 3 :element-type 'character :displaced-to s
                          :displaced-index-offset 2)))
    (setf (char d 2) #\Z)
    (multiple-value-bind (target offset) (array-displacement d)
      (list s (copy-seq d) (eq target s) offset (length d))))
  ("abcdZfgh" "cdZ" t 2 3))

(deftest displaced-string-sharing.two-arrays-one-string
  (let* ((s (copy-seq "abcdef"))
         (d1 (make-array 3 :element-type 'character :displaced-to s))
         (d2 (make-array 3 :element-type 'character :displaced-to s
                           :displaced-index-offset 2)))
    (setf (char d1 2) #\X)
    (copy-seq d2))
  "Xde")

(deftest displaced-string-sharing.fill-pointer
  (let* ((s (copy-seq "abcdef"))
         (d (make-array 6 :element-type 'character :displaced-to s
                          :fill-pointer 2)))
    (setf (char s 1) #\B)
    (vector-push #\C d)
    (list (copy-seq d) (length d) s))
  ("aBC" 3 "aBCdef"))

(deftest displaced-string-sharing.adjust-array
  (let* ((s (copy-seq "Hello world"))
         (w (make-array 0 :element-type 'character :adjustable t
                          :displaced-to s)))
    (adjust-array w 5 :displaced-to s :displaced-index-offset 6)
    (setf (char s 6) #\W)
    (setf (char w 4) #\D)
    (let ((before (copy-seq w)))
      (adjust-array w 3 :displaced-to s :displaced-index-offset 0)
      (setf (char w 0) #\J)
      (list before (copy-seq w) s)))
  ("WorlD" "Jel" "Jello WorlD"))

(deftest displaced-string-sharing.adjust-non-adjustable
  (let* ((s (copy-seq "abcdef"))
         (v (make-array 2 :element-type 'character :initial-element #\x))
         (d (adjust-array v 3 :displaced-to s :displaced-index-offset 3)))
    (setf (char s 3) #\D)
    (copy-seq d))
  "Def")

(deftest displaced-string-sharing.string-functions
  (let* ((s (copy-seq "the quick brown fox"))
         (d (make-array 5 :element-type 'character :displaced-to s
                          :displaced-index-offset 4)))
    (setf (char s 4) #\Q)
    (list (string= d "Quick")
          (search "ck" d)
          (subseq d 1 3)
          (progn (replace d "QUA") s)
          (with-output-to-string (o) (write-string d o))
          (string-upcase d)))
  (t 3 "ui" "the QUAck brown fox" "QUAck" "QUACK"))
