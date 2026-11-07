;;; (setf (schar s j) (code-char x)) with fixnum j and x stores the code without
;;; building a character object. A code with no character, a bad index and a
;;; non-string are decided exactly as before.

(defun %scc-set (s j x) (declare (fixnum j x)) (setf (schar s j) (code-char x)))
(defun %scc-fill (n)
  (declare (fixnum n))
  (let ((s (make-string n)))
    (dotimes (k n s) (setf (schar s k) (code-char (+ 65 (mod k 26)))))))

(deftest set-char-code-raw.values
  (let ((s (make-string 4 :initial-element #\a))
        (a (make-array 3 :element-type 'character :initial-element #\b :adjustable t :fill-pointer 3)))
    (list (char-code (%scc-set s 1 66)) (char-code (%scc-set s 3 #xe9))
          (map 'list #'char-code s) (char-code (%scc-set s 0 #xffff))
          (char-code (%scc-set a 2 67)) (coerce a 'string)
          (%scc-fill 30)))
  (66 233 (97 66 97 233) 65535 67 "bbC"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZABCD"))

(deftest set-char-code-raw.errors
  (let ((s (make-string 4 :initial-element #\a)))
    (list (handler-case (%scc-set s 4 66) (error () :error))
          (handler-case (%scc-set s -1 66) (error () :error))
          (handler-case (%scc-set s 1 #x10000) (error () :error))
          (handler-case (%scc-set s 1 -3) (error () :error))
          (handler-case (%scc-set 5 1 66) (error () :error))
          s))
  (:error :error :error :error :error "aaaa"))
