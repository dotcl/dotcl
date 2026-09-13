;;; dotcl-lsp-api:describe-at -- what the name under the cursor is.
;;;
;;; The same positions completion and documentation-url work from, answered
;;; from the image instead: how it is called, and what it does. This is what an
;;; editor shows for code already written, where completion has nothing to say.

(require "dotcl-lsp-api")

(defun lda (text offset) (dotcl-lsp-api:describe-at text offset))

;;; A standard symbol: what it is, where it lives, and its page.
;;;
;;; Not its signature: whether a standard function still knows its lambda list
;;; depends on how the image was built -- one loaded from a core has lost it --
;;; and this is about the description, not about that.
(deftest lda-symbol
  (let ((d (lda "(car x)" 2)))
    (list (getf d :name) (getf d :kind) (getf d :package) (getf d :url)))
  ("car" :function "COMMON-LISP" "http://l1sp.org/cl/car"))

;;; A symbol of this image has no page, and still has everything else.
(deftest lda-own-symbol
  (let ((d (lda "(dotcl-lsp-api:describe-at text 3)" 20)))
    (list (getf d :name)
          (and (first (getf d :signatures)) t)
          (and (getf d :documentation) t)
          (getf d :url)))
  ("describe-at" t t nil))

;;; A .NET member: every overload, the sentence, and the page it is on.
(deftest lda-member
  (let ((d (lda "(dotnet:invoke \"System.Text.StringBuilder\" \"AppendLine\")" 46)))
    (list (getf d :name) (getf d :kind)
          (length (getf d :signatures))
          (and (getf d :documentation) t)
          (and (getf d :returns) t)
          (getf d :url)))
  ("System.Text.StringBuilder.AppendLine" :member 4 t t
   "https://learn.microsoft.com/dotnet/api/system.text.stringbuilder.appendline"))

;;; The parameters of a member come with it.
(deftest lda-parameters
  (let ((d (lda "(dotnet:static \"System.Math\" \"Sqrt\")" 32)))
    (list (getf d :name) (mapcar #'car (getf d :parameters))))
  ("System.Math.Sqrt" ("d")))

;;; A type in type position.
(deftest lda-type
  (let ((d (lda "(dotnet:new \"System.Text.StringBuilder\")" 30)))
    (list (getf d :name) (getf d :kind) (and (getf d :documentation) t)))
  ("System.Text.StringBuilder" :type t))

;;; A string that is not a name denotes nothing.
(deftest lda-plain-string
  (lda "(format t \"hi\")" 12)
  nil)

;;; A name this image does not have is not invented.
(deftest lda-unknown-symbol
  (lda "(no-such-function-anywhere x)" 3)
  nil)
