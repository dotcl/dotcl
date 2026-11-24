;;; Arguments that are wrong are an error, and the nearby right ones still work:
;;; ADJUST-ARRAY :displaced-index-offset needs :displaced-to, the character bag
;;; of STRING-TRIM holds characters and is a proper sequence, and the result
;;; argument of the BIT-xxx operators is a bit array, T or NIL.

(defmacro %dla-err (form) `(handler-case (progn ,form :no-error) (error () :error)))

(deftest diagnostics-lenient-args.adjust-array-offset-needs-displaced-to
  (list (%dla-err (adjust-array (make-array 2) 2 :displaced-index-offset 1))
        (length (adjust-array (make-array 2) 2 :displaced-index-offset 0))
        (let ((target (make-array 5 :initial-contents '(0 1 2 3 4))))
          (aref (adjust-array (make-array 2 :adjustable t) 2 :displaced-to target
                                                             :displaced-index-offset 3)
                0)))
  (:error 2 3))

(deftest diagnostics-lenient-args.string-trim-bag
  (list (%dla-err (string-trim '(1 2) "a1b"))
        (%dla-err (string-trim (cons #\a #\b) "abc"))
        (%dla-err (string-trim (vector #\a 1) "abc"))
        (string-trim '(#\a #\b) "abcab")
        (string-trim "ab" "abcab")
        (string-trim (vector #\a #\b) "abcab")
        (string-left-trim nil "abc")
        (string-right-trim '(#\c) "abc"))
  (:error :error :error "c" "c" "c" "abc" "ab"))

(deftest diagnostics-lenient-args.bit-result-argument
  (list (%dla-err (bit-and #*101 #*110 (make-array 3)))
        (%dla-err (bit-not #*101 (make-array 3)))
        (bit-and #*101 #*110)
        (bit-and #*101 #*110 nil)
        (let ((r (make-array 3 :element-type 'bit)))
          (eq r (bit-and #*101 #*110 r)))
        (let ((a (copy-seq #*101)))
          (bit-and a #*110 t)
          a)
        (bit-not #*101 nil))
  (:error :error #*100 #*100 t #*100 #*010))
