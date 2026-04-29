;;; format.lisp — ANSI tests for format function

;;; --- ~A (aesthetic) ---

(deftest format-a.1
  (format nil "~A" 42)
  "42")

(deftest format-a.2
  (format nil "~A" "hello")
  "hello")

(deftest format-a.3
  (format nil "~A" nil)
  "NIL")

(deftest format-a.4
  (format nil "~A" t)
  "T")

(deftest format-a.5
  (format nil "~A" :key)
  ":KEY")

(deftest format-a.6
  (format nil "~A ~A ~A" 1 2 3)
  "1 2 3")

;;; --- ~S (standard) ---

(deftest format-s.1
  (format nil "~S" "hello")
  "\"hello\"")

(deftest format-s.2
  (format nil "~S" 42)
  "42")

(deftest format-s.3
  (format nil "~S" nil)
  "NIL")

;;; --- ~D (decimal) ---

(deftest format-d.1
  (format nil "~D" 42)
  "42")

(deftest format-d.2
  (format nil "~D" -7)
  "-7")

(deftest format-d.3
  (format nil "~D" 0)
  "0")

;;; --- ~B (binary) ---

(deftest format-b.1
  (format nil "~B" 10)
  "1010")

(deftest format-b.2
  (format nil "~B" 0)
  "0")

(deftest format-b.3
  (format nil "~B" 255)
  "11111111")

;;; --- ~O (octal) ---

(deftest format-o.1
  (format nil "~O" 8)
  "10")

(deftest format-o.2
  (format nil "~O" 0)
  "0")

(deftest format-o.3
  (format nil "~O" 63)
  "77")

;;; --- ~X (hex) ---

(deftest format-x.1
  (format nil "~X" 255)
  "FF")

(deftest format-x.2
  (format nil "~X" 0)
  "0")

(deftest format-x.3
  (format nil "~X" 16)
  "10")

;;; --- ~% (newline) ---

(deftest format-newline.1
  (format nil "~%")
  "
")

(deftest format-newline.2
  (format nil "a~%b")
  "a
b")

;;; --- ~~ (tilde) ---

(deftest format-tilde.1
  (format nil "~~")
  "~")

;;; --- ~C (character) ---

(deftest format-c.1
  (format nil "~C" #\a)
  "a")

;;; --- ~[...~] (conditional) ---

(deftest format-cond.1
  (format nil "~[zero~;one~;two~]" 0)
  "zero")

(deftest format-cond.2
  (format nil "~[zero~;one~;two~]" 1)
  "one")

(deftest format-cond.3
  (format nil "~[zero~;one~;two~]" 2)
  "two")

;;; --- ~:[...~] (boolean conditional) ---

(deftest format-bool.1
  (format nil "~:[no~;yes~]" nil)
  "no")

(deftest format-bool.2
  (format nil "~:[no~;yes~]" t)
  "yes")

(deftest format-bool.3
  (format nil "~:[no~;yes~]" 42)
  "yes")

;;; --- Combinations ---

(deftest format-combo.1
  (format nil "~A is ~D" "answer" 42)
  "answer is 42")

(deftest format-combo.2
  (format nil "x=~D, y=~D" 10 20)
  "x=10, y=20")

;;; --- destination nil returns string ---

(deftest format-dest.nil
  (stringp (format nil "hello"))
  t)

;;; --- Min-width for ~A ---

(deftest format-a-width.1
  (format nil "~10A" "hi")
  "hi        ")

(do-tests-summary)
