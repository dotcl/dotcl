;;; DOTNET:REF reaches an indexer that is not called Item (String and StringBuilder name it
;;; Chars through DefaultMemberAttribute). List, Dictionary and arrays are unchanged.
(require "dotnet-class")

(deftest dotnet-ref-named-indexer.named-indexer
  (list (dotnet:ref "abc" 1)
        (dotnet:ref (dotnet:new "System.Text.StringBuilder" "xyz") 2)
        (let ((sb (dotnet:new "System.Text.StringBuilder" "abc")))
          (setf (dotnet:ref sb 0) #\Z)
          (dotnet:invoke sb "ToString")))
  (#\b #\z "Zbc"))

(deftest dotnet-ref-named-indexer.item-indexers-unchanged
  (let ((l (dotnet:new "System.Collections.ArrayList")))
    (dotnet:invoke l "Add" 7)
    (list (dotnet:ref l 0)
          (progn (setf (dotnet:ref l 0) 9) (dotnet:ref l 0))
          (handler-case (dotnet:ref "abc" 10) (error () :error))))
  (7 9 :error))
