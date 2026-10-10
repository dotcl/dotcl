;;; A newline inside text printed into a logical block with a per-line prefix
;;; starts a new line of the block, so the prefix follows it: the newline in a
;;; string printed with ~S / ~A / PRIN1 / PRINC, and in text written with
;;; WRITE-STRING or WRITE-LINE. No indentation is added after such a newline.
;;; The *-TO-STRING functions and a string stream opened inside the block print
;;; to a stream of their own, so the prefix does not get into what they return.
;;; Expected strings are SBCL 2.6.8's.

(defun pplln (fn)
  (with-output-to-string (s)
    (let ((*print-pretty* t))
      (pprint-logical-block (s nil :per-line-prefix ";; ")
        (funcall fn s)))))

(deftest pplln-format-s
  (pplln (lambda (s) (format s "~S" (format nil "a~%b"))))
  ";; \"a
;; b\"")

(deftest pplln-format-a
  (pplln (lambda (s) (format s "~A" (format nil "~%c~%d"))))
  #.(format nil ";; ~%;; c~%;; d"))

(deftest pplln-prin1-princ
  (pplln (lambda (s) (prin1 (format nil "a~%b") s) (princ (format nil "~%c") s)))
  ";; \"a
;; b\"
;; c")

(deftest pplln-write-string
  (pplln (lambda (s) (write-string (format nil "a~%b") s)))
  ";; a
;; b")

(deftest pplln-write-line
  (pplln (lambda (s) (write-line (format nil "a~%b") s) (write-string "c" s)))
  ";; a
;; b
;; c")

(deftest pplln-to-string-is-fresh
  (pplln (lambda (s)
           (write-string (prin1-to-string (format nil "a~%b")) s)
           (write-string (with-output-to-string (o) (prin1 (format nil "~%c") o)) s)
           (write-string (format nil "~S" (format nil "~%d")) s)))
  ";; \"a
;; b\"\"
;; c\"\"
;; d\"")

(deftest pplln-prefix-block-no-indent
  (with-output-to-string (s)
    (let ((*print-pretty* t))
      (write-string "xx" s)
      (pprint-logical-block (s nil :prefix "(" :suffix ")")
        (write-string (format nil "ab~%cd") s))))
  "xx(ab
cd)")

(deftest pplln-format-block
  (let ((*print-pretty* t))
    (format nil "~@<;; ~@;~A ~S~:>" (format nil "one~%two") (list (format nil "x~%y"))))
  ";; one
;; two (\"x
;; y\")")
