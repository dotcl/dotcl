;;; dotcl-lsp-api:documentation-url -- the reference page for the name at point.
;;;
;;; The URLs are built rather than looked up, so what is tested is the spelling
;;; each side files its pages under: percent-encoding for symbol names that read
;;; as URL syntax, and the CLR-to-documentation shape for .NET (nesting as a
;;; dot, generic arity as a hyphen, the type that declares an inherited member).

(require "dotcl-lsp-api")

(defun ldu (text &optional offset)
  (dotcl-lsp-api:documentation-url text (or offset (1- (length text)))))

(defun ldu-url (text &optional offset)
  (getf (ldu text offset) :url))

;;; A COMMON-LISP symbol, with the cursor inside the token rather than after it.
(deftest ldu-symbol
  (ldu-url "(car x)" 2)
  "http://l1sp.org/cl/car")

;;; A name that reads as URL syntax has to survive being put in a path.
(deftest ldu-symbol-encoded
  (list (ldu-url "(1+ n)" 2) (ldu-url "(char= a b)" 3) (ldu-url "*print-base*" 4))
  ("http://l1sp.org/cl/1%2B"
   "http://l1sp.org/cl/char%3D"
   "http://l1sp.org/cl/%2Aprint-base%2A"))

;;; Nothing else has a page: a symbol of this image is not on the web.
(deftest ldu-not-standard
  (list (ldu "(dotnet:invoke x)" 10) (ldu "(ldu-url x)" 3))
  (nil nil))

;;; The kind travels with the URL, for a client that wants to say what it opened.
(deftest ldu-symbol-kind
  (let ((r (ldu "(car x)" 2)))
    (list (getf r :name) (getf r :kind)))
  ("car" :symbol))

;;; A literal type name in type position.
(deftest ldu-type
  (ldu-url "(dotnet:new \"System.Text.StringBuilder\")" 30)
  "https://learn.microsoft.com/dotnet/api/system.text.stringbuilder")

;;; Candidates spell a generic type without its arity, because that is what
;;; make-generic-type takes; the reference files it under the arity.
(deftest ldu-generic-arity
  (ldu-url "(dotnet:make-generic-type \"System.Collections.Generic.List\" nil)" 40)
  "https://learn.microsoft.com/dotnet/api/system.collections.generic.list-1")

;;; A member of a literal receiver.
(deftest ldu-member
  (ldu-url "(dotnet:invoke \"System.Text.StringBuilder\" \"AppendLine\")" 46)
  "https://learn.microsoft.com/dotnet/api/system.text.stringbuilder.appendline")

;;; An inherited member is filed under the type that declares it, not under the
;;; receiver -- there is no StringBuilder.GetType page.
(deftest ldu-inherited-member
  (ldu-url "(dotnet:invoke \"System.Text.StringBuilder\" \"GetType\")" 46)
  "https://learn.microsoft.com/dotnet/api/system.object.gettype")

;;; A static of a literal type, reached through the other spelling.
(deftest ldu-static-member
  (ldu-url "(dotnet:static \"System.Math\" \"Sqrt\")" 32)
  "https://learn.microsoft.com/dotnet/api/system.math.sqrt")

;;; A string that is not in an interop position denotes nothing to look up.
(deftest ldu-plain-string
  (ldu "(format t \"System.Math\")" 15)
  nil)

;;; A name nobody can resolve gets no URL rather than a guessed one.
(deftest ldu-unknown-type
  (ldu "(dotnet:new \"No.Such.Type\")" 18)
  nil)
