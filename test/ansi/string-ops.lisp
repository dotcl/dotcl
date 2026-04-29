;;; string-ops.lisp — ANSI tests for string operations

;;; --- string= ---

(deftest string=.1
  (notnot (string= "hello" "hello"))
  t)

(deftest string=.2
  (string= "hello" "world")
  nil)

(deftest string=.3
  (string= "" "")
  t)

;;; --- string< ---

(deftest string<.1
  (notnot (string< "abc" "abd"))
  t)

(deftest string<.2
  (string< "abd" "abc")
  nil)

(deftest string<.prefix
  (notnot (string< "abc" "abcd"))
  t)

;;; --- string> ---

(deftest string>.1
  (notnot (string> "abd" "abc"))
  t)

(deftest string>.2
  (string> "abc" "abd")
  nil)

;;; --- string<= ---

(deftest string<=.1
  (notnot (string<= "abc" "abd"))
  t)

(deftest string<=.2
  (notnot (string<= "abc" "abc"))
  t)

(deftest string<=.3
  (string<= "abd" "abc")
  nil)

;;; --- string>= ---

(deftest string>=.1
  (notnot (string>= "abd" "abc"))
  t)

(deftest string>=.2
  (notnot (string>= "abc" "abc"))
  t)

(deftest string>=.3
  (string>= "abc" "abd")
  nil)

;;; --- string/= ---

(deftest string/=.1
  (notnot (string/= "abc" "abd"))
  t)

(deftest string/=.2
  (string/= "abc" "abc")
  nil)

;;; --- string-upcase / string-downcase ---

(deftest string-upcase.1
  (string-upcase "hello")
  "HELLO")

(deftest string-upcase.already
  (string-upcase "HELLO")
  "HELLO")

(deftest string-upcase.empty
  (string-upcase "")
  "")

(deftest string-downcase.1
  (string-downcase "HELLO")
  "hello")

(deftest string-downcase.already
  (string-downcase "hello")
  "hello")

(deftest string-downcase.empty
  (string-downcase "")
  "")

;;; --- concatenate ---

(deftest concatenate.string.1
  (concatenate 'string "hello" " " "world")
  "hello world")

(deftest concatenate.string.2
  (concatenate 'string "a" "b" "c")
  "abc")

(deftest concatenate.string.empty
  (concatenate 'string "" "hello" "")
  "hello")

(deftest concatenate.string.single
  (concatenate 'string "only")
  "only")

(deftest concatenate.list.1
  (concatenate 'list '(1 2) '(3 4))
  (1 2 3 4))

(deftest concatenate.list.empty
  (concatenate 'list '() '(1 2) '())
  (1 2))

;;; --- search ---

(deftest search.1
  (search "ll" "hello")
  2)

(deftest search.2
  (search "xyz" "hello")
  nil)

(deftest search.at-start
  (search "he" "hello")
  0)

(deftest search.empty-pattern
  (search "" "hello")
  0)

;;; --- string ---

(deftest string.1
  (string 'hello)
  "HELLO")

(deftest string.2
  (string "already")
  "already")

;;; --- string from char ---

(deftest string.char
  (string #\a)
  "a")

;;; --- coerce to string ---

(deftest coerce.string.1
  (coerce #\a 'string)
  "a")

(do-tests-summary)
