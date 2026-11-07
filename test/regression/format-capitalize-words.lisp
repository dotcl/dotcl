;;; FORMAT's ~:( capitalizes words as STRING-CAPITALIZE does (CLHS 22.3.8.3): a word is a
;;; run of letters and digits, so an apostrophe starts a new word and a word that begins
;;; with a digit stays lower case after it. .NET's title case gave "Don't Stop" and
;;; "3D Rendering". ~@( , ~:@( and ~( are unchanged.

(defun %fcw (control text) (format nil control text))

(deftest format-capitalize-words.same-as-string-capitalize
  (loop for text in '("don't stop" "3d rendering" "hello world" "HELLO WORLD" "a-b c_d e.f"
                      "x1y2 z" "o'neil's" "" "  two  spaces ")
        always (string= (%fcw "~:(~A~)" text) (string-capitalize text)))
  t)

(deftest format-capitalize-words.examples
  (list (%fcw "~:(~A~)" "don't stop") (%fcw "~:(~A~)" "3d rendering") (%fcw "~:(~A~)" "hello world"))
  ("Don'T Stop" "3d Rendering" "Hello World"))

(deftest format-capitalize-words.other-modes-unchanged
  (list (%fcw "~(~A~)" "Hello WORLD") (%fcw "~@(~A~)" "hello wORLD") (%fcw "~:@(~A~)" "hello"))
  ("hello world" "Hello world" "HELLO"))
