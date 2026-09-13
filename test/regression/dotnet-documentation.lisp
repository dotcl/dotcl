;;; dotnet:documentation -- the prose the framework ships for a type or member.
;;;
;;; The entries are keyed by a documentation ID built from the member, so what
;;; is tested is that the ID comes out right: an overload picked by parameter
;;; type, a property asked about through its accessor, an inherited member
;;; found on the type that declares it, and a generic type under its arity.

(defun dd (&rest args)
  (getf (apply #'dotnet:documentation args) :summary))

;;; A type.
(deftest dd-type
  (dd "System.Text.StringBuilder")
  "Represents a mutable string of characters. This class cannot be inherited.")

;;; A method, with the cross-reference elements read as the names they point at
;;; rather than left as T:System.Text.StringBuilder.
(deftest dd-method
  (dd "System.Text.StringBuilder" "AppendLine")
  "Appends the default line terminator to the end of the current StringBuilder object.")

;;; A property is documented under its own name; dotnet:invoke calls it by its
;;; accessor, and the accessor has no entry of its own.
(deftest dd-property-through-accessor
  (equal (dd "System.Text.StringBuilder" "get_Length")
         (dd "System.Text.StringBuilder" "Length"))
  t)

;;; An inherited member is filed under the type that declares it.
(deftest dd-inherited
  (dd "System.Text.StringBuilder" "GetType")
  "Gets the Type of the current instance.")

;;; A generic type is filed under its arity, and its members under it.
(deftest dd-generic-member
  (dd "System.Collections.Generic.List`1" "Add")
  "Adds an object to the end of the List.")

;;; A static.
(deftest dd-static
  (dd "System.Math" "Sqrt")
  "Returns the square root of a specified number.")

;;; The rest of the entry, not only the sentence.
(deftest dd-parts
  (let ((d (dotnet:documentation "System.IO.File" "ReadAllText")))
    (list (mapcar #'car (getf d :parameters))
          (and (getf d :returns) t)
          (and (member "FileNotFoundException" (getf d :exceptions)
                       :key #'car :test #'string=)
               t)))
  (("path") t t))

;;; Nothing to say is NIL, not an error.
(deftest dd-unknown-member
  (dotnet:documentation "System.Math" "NoSuchMemberAtAll")
  nil)

;;; A type defined in this image has no XML anywhere.
(deftest dd-undocumented-type
  (dotnet:documentation "System.Object" "NoSuchMemberAtAll")
  nil)

;;; dotnet:members carries the sentence when asked, and not otherwise. The file
;;; is read in the background, so this waits for the reading the other tests
;;; have already triggered.
(deftest dd-members-carry-it
  (progn
    (dotnet:documentation "System.Text.StringBuilder" "AppendLine")
    (let ((with (dotnet:members "System.Text.StringBuilder"
                                :prefix "AppendLine" :documentation t))
          (without (dotnet:members "System.Text.StringBuilder" :prefix "AppendLine")))
      (list (and (every (lambda (m) (getf m :documentation)) with) t)
            (some (lambda (m) (getf m :documentation)) without))))
  (t nil))
