;;; Every character output stream knows its column (DOTCL:CHARPOS), and ~T and
;;; the pretty printer count from it.
;;;
;;; Only string output streams used to know where the line began: on a file,
;;; the console, or a composite stream around them, (format s "~10Tx") after
;;; "abc" on the same line counted from column 0 and put x at column 13, and a
;;; logical block opened in the middle of a line indented its continuation lines
;;; from column 0. The expected strings and columns are SBCL 2.6.8's
;;; (SB-KERNEL:CHARPOS).

(defparameter *scol-dir* (regression-temp-dir))
(defun scol-path (name)
  (format nil "~a/dotcl-scol-~a-~d.txt" *scol-dir* name (random 1000000000)))

(defun scol-body (s)
  (write-string "abc" s) (format s "~10Tx~%")
  (write-char #\a s) (write-char #\b s) (format s "~8,4T|~%")
  (format s "ab~%cd") (format s "~10Ty~%")
  (write-string "abc" s)
  (let ((*print-right-margin* 40) (*print-pretty* t))
    (pprint-logical-block (s nil :prefix "(")
      (write-string "aa" s) (pprint-newline :mandatory s) (write-string "bb" s)))
  (terpri s)
  (write-string "abcde" s)
  (let ((*print-right-margin* 40) (*print-pretty* t))
    (pprint-logical-block (s nil)
      (write-string "x" s) (pprint-indent :block 3 s)
      (pprint-newline :mandatory s) (write-string "y" s)))
  (terpri s)
  (write-string "abcd" s)
  (let ((*print-right-margin* 40) (*print-pretty* t))
    (format s "~@<~a ~:_~a~:>" "p" "q"))
  (format s "~3@T|~%")
  (prin1 'foo s) (format s "~12T=~%")
  (write-line "12345" s) (format s "~5Tq~%")
  (write-string (format nil "a~%bc") s) (format s "~6T!~%"))

(defparameter *scol-expected*
  (format nil "abc       x~%ab      |~%ab~%cd        y~%abc(aa~%    bb~%~
               abcdex~%        y~%abcdp q   |~%FOO         =~%12345~%     q~%~
               a~%bc    !~%"))

(defun scol-file-text (fn)
  (let ((p (scol-path "body")))
    (unwind-protect
         (progn
           (with-open-file (s p :direction :output :if-exists :supersede)
             (funcall fn s))
           (with-open-file (s p)
             (let ((out (make-string (file-length s))))
               (subseq out 0 (read-sequence out s)))))
      (ignore-errors (delete-file p)))))

(deftest stream-column-string
  (with-output-to-string (s) (scol-body s))
  #.*scol-expected*)

(deftest stream-column-file
  (scol-file-text #'scol-body)
  #.*scol-expected*)

(deftest stream-column-composites-over-file
  (list (scol-file-text (lambda (f) (scol-body (make-broadcast-stream f))))
        (scol-file-text (lambda (f) (scol-body (make-two-way-stream
                                                 (make-string-input-stream "") f))))
        (scol-file-text (lambda (f) (scol-body (make-echo-stream
                                                 (make-string-input-stream "") f))))
        (scol-file-text (lambda (f)
                          (let ((*scol-target* f))
                            (declare (special *scol-target*))
                            (scol-body (make-synonym-stream '*scol-target*))))))
  #.(list *scol-expected* *scol-expected* *scol-expected* *scol-expected*))

(deftest stream-column-fill-pointer-string
  (let ((str (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s str) (scol-body s))
    (coerce str 'simple-string))
  #.*scol-expected*)

(defun scol-charpos-trace (s)
  (let (r)
    (push (dotcl:charpos s) r)
    (write-string "abc" s) (push (dotcl:charpos s) r)
    (write-char #\d s) (push (dotcl:charpos s) r)
    (format s "xy~%z") (push (dotcl:charpos s) r)
    (terpri s) (push (dotcl:charpos s) r)
    (prin1 12345 s) (push (dotcl:charpos s) r)
    (fresh-line s) (push (dotcl:charpos s) r)
    (write-line "12" s) (push (dotcl:charpos s) r)
    (write-sequence "tuv" s) (push (dotcl:charpos s) r)
    (format s "~10T") (push (dotcl:charpos s) r)
    (nreverse r)))

(deftest stream-column-charpos
  (list (scol-charpos-trace (make-string-output-stream))
        (let (r) (scol-file-text (lambda (s) (setq r (scol-charpos-trace s)))) r)
        (let (r) (scol-file-text
                  (lambda (f) (setq r (scol-charpos-trace
                                       (make-broadcast-stream f (make-string-output-stream))))))
             r)
        (let ((str (make-array 3 :element-type 'character :fill-pointer 3
                                 :adjustable t :initial-contents "pre")))
          (with-output-to-string (s str) (scol-charpos-trace s)))
        (dotcl:charpos (make-broadcast-stream)))
  ((0 3 4 1 0 5 0 0 3 10)
   (0 3 4 1 0 5 0 0 3 10)
   (0 3 4 1 0 5 0 0 3 10)
   (3 6 7 1 0 5 0 0 3 10)
   0))
