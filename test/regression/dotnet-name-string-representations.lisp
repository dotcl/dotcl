;;; A type or member name may be any string, not only a simple one.
;;;
;;; CL strings have two runtime representations here, and a caller that builds
;;; names rather than writing them literally -- a completion provider working on
;;; a document, anything holding text in an adjustable buffer -- hands over the
;;; other one. Every interop entry point takes a name, so every one of them has
;;; to read both.

(defun dnsr-grow (text)
  "TEXT in an adjustable, fill-pointered string."
  (let ((s (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for c across text do (vector-push-extend c s))
    s))

(defparameter *dnsr-adjustable* (dnsr-grow "System.Text.StringBuilder"))
(defparameter *dnsr-subseq* (subseq (dnsr-grow "xx System.Text.StringBuilder yy") 3 28))
(defparameter *dnsr-displaced*
  (make-array 25 :element-type 'character
                 :displaced-to (dnsr-grow "xx System.Text.StringBuilder yy")
                 :displaced-index-offset 3))

(deftest dnsr-all-are-strings
  (list (stringp *dnsr-adjustable*) (stringp *dnsr-subseq*) (stringp *dnsr-displaced*))
  (t t t))

(deftest dnsr-resolve-type
  (mapcar (lambda (name)
            (dotnet:invoke (dotnet:resolve-type name) "FullName"))
          (list *dnsr-adjustable* *dnsr-subseq* *dnsr-displaced*))
  ("System.Text.StringBuilder" "System.Text.StringBuilder" "System.Text.StringBuilder"))

(deftest dnsr-new
  (dotnet:invoke (dotnet:invoke (dotnet:new *dnsr-adjustable*) "Append" "ok") "ToString")
  "ok")

;;; The member name is the other half of a call, and travels the same way.
(deftest dnsr-invoke-member-name
  (let ((builder (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke builder (dnsr-grow "Append") "grown")
    (dotnet:invoke builder (dnsr-grow "ToString")))
  "grown")

(deftest dnsr-static
  (dotnet:static (dnsr-grow "System.Math") (dnsr-grow "Sqrt") 16.0d0)
  4.0d0)

(deftest dnsr-members
  (let ((names (mapcar (lambda (m) (getf m :name))
                       (dotnet:members *dnsr-subseq* :prefix (dnsr-grow "AppendLine")))))
    (and (member "AppendLine" names :test #'string=) t))
  t)

(deftest dnsr-make-array
  (dotnet:invoke (dotnet:make-array (dnsr-grow "System.Int32") 4) "Length")
  4)

;;; A keyword names a type by its name, not by the colon it prints with.
(deftest dnsr-keyword-designator
  (dotnet:invoke (dotnet:resolve-type :|System.Math|) "FullName")
  "System.Math")
