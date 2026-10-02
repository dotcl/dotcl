;;; The pretty printer drops the blanks at the end of a line before a line
;;; break it emits, also when the output goes to a string with a fill pointer
;;; (WITH-OUTPUT-TO-STRING (s string)).
;;;
;;; The trimming only knew the writer of an ordinary string output stream, so
;;; a string given to WITH-OUTPUT-TO-STRING kept "3 " where a fresh string got
;;; "3". data-frame's PRINT-DF test prints a table that way (each cell written
;;; with a space after it, rows ended by a mandatory newline). Blanks the string
;;; held before the stream was opened are the caller's and stay. Expected values
;;; are SBCL's.

(defun ptfp-rows (s)
  (let ((*print-pretty* t))
    (pprint-logical-block (s '(("A" "3") ("B" "33")))
      (loop (pprint-exit-if-list-exhausted)
            (let ((row (pprint-pop)))
              (pprint-logical-block (s row :per-line-prefix ";; ")
                (loop (pprint-exit-if-list-exhausted)
                      (write-string (pprint-pop) s)
                      (write-char #\Space s))))
            (pprint-newline :mandatory s)))))

(deftest pprint-trim-fill-pointer-string.same-as-fresh-string
  (let ((a (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s a) (ptfp-rows s))
    (list (coerce a 'simple-string) (with-output-to-string (s) (ptfp-rows s))))
  (#.(format nil ";; A 3~%;; B 33~%") #.(format nil ";; A 3~%;; B 33~%")))

(deftest pprint-trim-fill-pointer-string.keeps-earlier-content
  (let ((a (make-array 3 :element-type 'character :fill-pointer 3 :adjustable t
                         :initial-contents "x  ")))
    (with-output-to-string (s a)
      (let ((*print-pretty* t))
        (pprint-logical-block (s nil) (pprint-newline :mandatory s))))
    (subseq a 0 3))
  "x  ")
