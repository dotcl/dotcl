;;; (dotnet:members ... :documentation :wait) reads the documentation file first, so the
;;; sentences are there on the first call (T keeps not waiting, for completion lists).
;;; (dotnet:type-names ... :limit n) caps the whole answer, not each of its two parts.

(deftest dotnet-members-docs-and-type-names-limit.wait
  (let ((m (dotnet:members "System.Collections.Generic.List`1" :prefix "AddRange" :documentation :wait)))
    (list (consp m) (every (lambda (x) (stringp (getf x :documentation))) m)))
  (t t))

(deftest dotnet-members-docs-and-type-names-limit.type-names-limit
  (let ((all (dotnet:type-names "System." :limit 100))
        (few (dotnet:type-names "System." :limit 3))
        (few-t (dotnet:type-names "System.Text.S" :limit 2)))
    (list (<= (length few) 3) (<= (length few-t) 2) (> (length all) (length few))
          (every (lambda (x) (member (getf x :kind) '(:class :namespace))) few)))
  (t t t t))
