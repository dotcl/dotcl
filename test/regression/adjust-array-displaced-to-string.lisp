;;; ADJUST-ARRAY with :DISPLACED-TO a string displaces the array into it.
;;;
;;; Bug: the adjustable-array path only recognised a general vector as the
;;; displacement target. A string target fell through as if no :DISPLACED-TO
;;; had been given, so the array came back holding #\Nul characters. spinneret
;;; walks the words of a text through one adjustable window displaced into
;;; the string, and printed every word as blanks.

(deftest adjust-array-displaced-to-string.window
  (let* ((string "Hello world")
         (w (make-array 0 :element-type (array-element-type string)
                          :adjustable t :displaced-to string
                          :displaced-index-offset 0)))
    (list (copy-seq (adjust-array w 5 :displaced-to string :displaced-index-offset 0))
          (copy-seq (adjust-array w 5 :displaced-to string :displaced-index-offset 6))
          (length w)))
  ("Hello" "world" 5))

(deftest adjust-array-displaced-to-string.from-plain-adjustable
  (let ((w (make-array 3 :element-type 'character :adjustable t :initial-element #\x)))
    (adjust-array w 2 :displaced-to "abcd" :displaced-index-offset 1)
    (multiple-value-bind (target offset) (array-displacement w)
      (list (copy-seq w) (coerce target 'list) offset)))
  ("bc" (#\a #\b #\c #\d) 1))

(deftest adjust-array-displaced-to-string.not-an-array
  (let ((w (make-array 3 :adjustable t)))
    (handler-case (progn (adjust-array w 2 :displaced-to 42) :no-error)
      (type-error () :type-error)))
  :type-error)
