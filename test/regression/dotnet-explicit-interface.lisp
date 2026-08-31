;;; Members implemented explicitly for an interface.
;;;
;;; List<T> declares `bool ICollection<T>.IsReadOnly`, which is private on the
;;; concrete type, so InvokeMember never found it: every such member was
;;; unreachable from Lisp, dotnet:cast included. Reflection through the interface
;;; dispatches correctly, so an unmatched member name is now looked for among the
;;; interfaces the type implements.
;;;
;;; Where two interfaces declare the name and the type implements each of them
;;; separately, the two can legitimately disagree -- so that is an error naming
;;; both, not a guess. dotnet:cast picks one.

(defun dei-int-list ()
  (let ((list (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                    (list "System.Int32")))))
    (dotnet:invoke list "Add" 1)
    list))

;;; Declared by IList alone: no ambiguity, so it resolves.
(deftest dei-single-interface-member
  (dotnet:invoke (dei-int-list) "IsFixedSize")
  nil)

;;; Declared by both ICollection<T> and IList, implemented separately.
(deftest dei-ambiguous-member-is-reported
  (handler-case (progn (dotnet:invoke (dei-int-list) "IsReadOnly") :no-error)
    (error (condition)
      (let ((text (format nil "~a" condition)))
        (list (and (search "IsReadOnly" text) t) (and (search "dotnet:cast" text) t)))))
  (t t))

;;; A cast names the interface to use.
(deftest dei-cast-selects-the-interface
  (dotnet:invoke (dotnet:cast (dei-int-list) "System.Collections.IList") "IsReadOnly")
  nil)

(deftest dei-cast-selects-the-generic-interface
  (dotnet:invoke (dotnet:cast (dei-int-list)
                              (dotnet:make-generic-type "System.Collections.Generic.ICollection"
                                                        (list "System.Int32")))
                 "IsReadOnly")
  nil)

;;; The ordinary path is untouched: a public member still resolves on the type.
(deftest dei-public-member-unchanged
  (dotnet:invoke (dei-int-list) "Count")
  1)

;;; A name that is nowhere is still an error, not an interface probe that
;;; silently finds something else.
(deftest dei-unknown-member-still-errors
  (handler-case (progn (dotnet:invoke (dei-int-list) "NoSuchMemberHere") :no-error)
    (error () :error))
  :error)

;;; --- generic methods --------------------------------------------------------
;;;
;;; dotnet:invoke-generic looked only at the concrete type's public methods, so a
;;; generic method implemented explicitly (IFeatureCollection.Get<TFeature> is the
;;; shape that matters) was unreachable the same way. No BCL type expresses this
;;; conveniently, so the test type lives in TestSupport (DEBUG builds only).

(defun dei-echo () (dotnet:new "DotCL.TestSupport.ExplicitEcho"))

(deftest dei-generic-explicit-method
  (dotnet:invoke-generic (dei-echo) "Echo" '("System.String") "hi")
  "hi")

(deftest dei-generic-explicit-method-via-cast
  (dotnet:invoke-generic (dotnet:cast (dei-echo) "DotCL.TestSupport.IEcho")
                         "Echo" '("System.Int32") 7)
  7)

(deftest dei-generic-unknown-still-errors
  (handler-case (progn (dotnet:invoke-generic (dei-echo) "Nope" '("System.Int32") 1) :no-error)
    (error () :error))
  :error)

;;; A known difference from C#: where the concrete type has a public member of the
;;; same name as an explicit one, the concrete member wins even after a cast --
;;; ((IEcho)e).Where is "interface" in C# but "class" here. A cast currently steers
;;; overload resolution and the interface search; it does not redirect a lookup
;;; that succeeds on the type itself.
(deftest dei-cast-does-not-shadow-a-public-member
  (dotnet:invoke (dotnet:cast (dei-echo) "DotCL.TestSupport.IEcho") "Where")
  "class")
