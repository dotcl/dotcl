;;; Regression: DotclHost's explicit collection conversions.
;;;
;;; A .NET array handed to DotclHost.Call arrives as a foreign object, not as a
;;; Lisp sequence — deliberately, so a byte[] stays the same buffer. That left a
;;; host with no way to say "pass this as a Lisp list": the dotcl-classlib
;;; template had to take its arguments as &rest scalars. ToLispList /
;;; ToLispVector are that way in; ToClrArray / ToClrList are the way back.

(defun %hm-to-list (enumerable)
  (dotnet:static "DotCL.DotclHost" "ToLispList" enumerable))

(defun %hm-to-vector (enumerable)
  (dotnet:static "DotCL.DotclHost" "ToLispVector" enumerable))

;;; A .NET char[] becomes a real Lisp list.
(deftest host-marshal-array-to-list
  (let ((chars (%hm-to-list (dotnet:invoke "abc" "ToCharArray"))))
    (list (listp chars) (length chars) chars))
  (t 3 (#\a #\b #\c)))

;;; ... and the elements are Lisp values, not wrapped foreign objects, so
;;; ordinary sequence functions work on them.
(deftest host-marshal-list-is-usable
  (coerce (%hm-to-list (dotnet:invoke "abc" "ToCharArray")) 'string)
  "abc")

;;; The vector form.
(deftest host-marshal-array-to-vector
  (let ((v (%hm-to-vector (dotnet:invoke "xy" "ToCharArray"))))
    (list (vectorp v) (length v) (aref v 1)))
  (t 2 #\y))

;;; An empty sequence converts to NIL / an empty vector, not to an error.
(deftest host-marshal-empty
  (let ((empty (dotnet:invoke "" "ToCharArray")))
    (list (%hm-to-list empty) (length (%hm-to-vector empty))))
  (nil 0))

;;; The way back: a Lisp list or vector to a .NET array.
(deftest host-marshal-list-to-clr
  (dotnet:invoke
   (dotnet:static-generic "DotCL.DotclHost" "ToClrArray" '("System.String")
                          (list "p" "q"))
   "Length")
  2)

(deftest host-marshal-vector-to-clr
  (dotnet:invoke
   (dotnet:static-generic "DotCL.DotclHost" "ToClrArray" '("System.Int32")
                          (vector 1 2 3))
   "Length")
  3)

;;; NIL is the empty sequence.
(deftest host-marshal-nil-to-clr
  (dotnet:invoke
   (dotnet:static-generic "DotCL.DotclHost" "ToClrArray" '("System.String") nil)
   "Length")
  0)
