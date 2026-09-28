;;; A Lisp character reaches a System.Char parameter.
;;;
;;; The binder matches overloads on the runtime type of each argument, and a
;;; character used to arrive as the Lisp object itself: it matched only an
;;; (object) overload, so the method received a LispChar and saw its printed
;;; representation. (dotnet:static "System.Console" "Write" #\a) printed
;;; #\LATIN_SMALL_LETTER_A, and #\Newline printed its name instead of moving
;;; the cursor. A method with only a char overload was reported as not found.
;;;
;;; The reverse must keep holding: a STRING, including a one-character one,
;;; still picks the string overload. Convert.ToInt32 discriminates the two
;;; sharply, since the char overload returns the character's code while the
;;; string overload parses the text as a number.

;;; --- a character picks the char overload ---

(deftest char-overload-convert-to-int32   ; ToInt32(char) -> code, not ToInt32(string)
  (dotnet:static "System.Convert" "ToInt32" #\7)
  55)

(deftest char-overload-index-of           ; String.IndexOf(char) beside IndexOf(string)
  (dotnet:invoke "abc" "IndexOf" #\b)
  1)

(deftest char-overload-append             ; the shape that garbled terminal output
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" #\a)
    (dotnet:invoke sb "ToString"))
  "a")

(deftest char-overload-append-newline     ; a newline is one control character, not a name
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "Append" #\Newline)
    (let ((s (dotnet:invoke sb "ToString")))
      (list (length s) (char-code (char s 0)))))
  (1 10))

(deftest char-overload-char-only-method   ; no string overload of this arity at all
  (dotnet:static "System.Char" "IsDigit" #\7)
  t)

(deftest char-overload-round-trip         ; in as System.Char, back out as a character
  (dotnet:static "System.Char" "ToUpper" #\a)
  #\A)

(deftest char-overload-constructor        ; new String(char, int), not (string, int)
  (dotnet:invoke (dotnet:new "System.String" #\a 3) "ToString")
  "aaa")

(deftest char-overload-typed-direct-call  ; the compiled path, param type declared
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (%dotnet-call-direct "System.Text.StringBuilder" "Append" ("System.Char") sb #\a)
    (dotnet:invoke sb "ToString"))
  "a")

(deftest char-overload-array-element      ; char[] store goes through the same conversion
  (let ((a (dotnet:new-array "System.Char" #\a #\b)))
    (list (dotnet:invoke a "get_Length")
          (dotnet:invoke a "GetValue" 0)))
  (2 #\a))

;;; --- a string still picks the string overload ---

(deftest char-overload-string-still-parsed  ; ToInt32("7") parses; it does not return 55
  (dotnet:static "System.Convert" "ToInt32" "7")
  7)

(deftest char-overload-one-char-string-not-char
  ;; A one-character string must NOT start binding to ToInt32(char): that would
  ;; return 120 for "x" instead of signalling a format error.
  (handler-case (progn (dotnet:static "System.Convert" "ToInt32" "x") :no-error)
    (error () :error))
  :error)

(deftest char-overload-string-index-of
  (dotnet:invoke "abc" "IndexOf" "b")
  1)

(deftest char-overload-string-not-a-char
  ;; ...and the conversion stays one-way: a string does not reach a char-only
  ;; parameter just because it holds one character.
  (handler-case (progn (dotnet:static "System.Char" "IsDigit" "7") :no-error)
    (error () :error))
  :error)

;;; --- a character where only an (object) parameter exists ---

(deftest char-overload-object-parameter   ; String.Format(string, object)
  (dotnet:static "System.String" "Format" "[{0}]" #\a)
  "[a]")

(deftest char-overload-is-instance-of     ; a character's natural .NET type is System.Char
  (dotnet:is-instance-of #\a "System.Char")
  t)
