;;; LINQ calls whose lambda result type must be inferred, and collection selectors.
;;;
;;; Enumerable.Zip<TFirst,TSecond,TResult> used to bind only the receiver: the
;;; second collection and the lambda were LispFunction / raw values reflection
;;; could not instantiate. SelectMany's collection selector returned a Lisp list
;;; or vector, which the delegate marshaller could not turn into IEnumerable<T>,
;;; and Cast<TResult> / OfType<TResult> in a chain had nowhere to take their one
;;; type argument from.

(defun dllr-ints (&rest xs)
  (let ((l (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                 (list "System.Int32")))))
    (dolist (x xs l) (dotnet:invoke l "Add" x))))

(defun dllr-list (enumerable)
  (let ((lst (dotnet:invoke enumerable "ToList")) (acc '()))
    (dotimes (i (dotnet:invoke lst "get_Count") (nreverse acc))
      (push (dotnet:invoke lst "get_Item" i) acc))))

(deftest dllr-zip-result-selector-static
  (dllr-list (dotnet:static "System.Linq.Enumerable" "Zip"
                            (dllr-ints 3 1 2) (dllr-ints 3 1 2)
                            (lambda (a b) (+ a b))))
  (6 2 4))

(deftest dllr-zip-result-selector-chain
  (dllr-list (dotnet:-> (dllr-ints 3 1 2)
                        ("Zip" (dllr-ints 3 1 2) (lambda (a b) (+ a b)))
                        "ToList"))
  (6 2 4))

(deftest dllr-zip-two-collections
  (list (dotnet:-> (dllr-ints 1 2) ("Zip" (dllr-ints 3 4)) "ToList" "Count")
        (dotnet:invoke (dotnet:static "System.Linq.Enumerable" "Zip"
                                      (dllr-ints 1 2) (dllr-ints 3 4))
                       "Count"))
  (2 2))

(deftest dllr-zip-three-collections
  (list (dotnet:-> (dllr-ints 1 2) ("Zip" (dllr-ints 3 4) (dllr-ints 5 6))
                   "ToList" "Count")
        (dotnet:invoke (dotnet:static "System.Linq.Enumerable" "Zip"
                                      (dllr-ints 1 2) (dllr-ints 3 4) (dllr-ints 5 6))
                       "Count"))
  (2 2))

(deftest dllr-selectmany-dotnet-enumerable
  (dllr-list (dotnet:-> (dllr-ints 3 1 2)
                        ("SelectMany" (lambda (x)
                                        (dotnet:static "System.Linq.Enumerable"
                                                       "Range" 0 x)))
                        "ToList"))
  (0 1 2 0 0 1))

(deftest dllr-selectmany-lisp-list
  (dllr-list (dotnet:-> (dllr-ints 3 1 2)
                        ("SelectMany" (lambda (x) (list x x)))
                        "ToList"))
  (3 3 1 1 2 2))

(deftest dllr-selectmany-lisp-vector
  (dllr-list (dotnet:-> (dllr-ints 3 1 2)
                        ("SelectMany" (lambda (x) (vector x x)))
                        "ToList"))
  (3 3 1 1 2 2))

(deftest dllr-selectmany-result-selector
  (dllr-list (dotnet:-> (dllr-ints 3 1 2)
                        ("SelectMany" (lambda (x) (list x x))
                                      (lambda (x y) (+ x y)))
                        "ToList"))
  (6 6 2 2 4 4))

(deftest dllr-selectmany-indexed
  (dotnet:-> (dllr-ints 3 1 2)
             ("SelectMany" (lambda (x i) (list x i)))
             "ToList" "Count")
  6)

(deftest dllr-join
  (dllr-list (dotnet:-> (dllr-ints 3 1 2)
                        ("Join" (dllr-ints 3 1 2) (lambda (x) x) (lambda (y) y)
                                (lambda (x y) (+ x y)))
                        "ToList"))
  (6 2 4))

(deftest dllr-groupjoin
  (dllr-list (dotnet:-> (dllr-ints 3 1 2)
                        ("GroupJoin" (dllr-ints 3 1 2) (lambda (x) x) (lambda (y) y)
                                     (lambda (x ys) (dotnet:invoke ys "Count")))
                        "ToList"))
  (1 1 1))

(deftest dllr-selectmany-bad-result-error
  (handler-case
      (progn (dotnet:-> (dllr-ints 1)
                        ("SelectMany" (lambda (x) x))
                        "ToList")
             :no-error)
    (error (e)
      (let ((m (princ-to-string e)))
        (and (search "IEnumerable" m) (search "INTEGER" m) t))))
  t)

(deftest dllr-cast-type-name-in-chain
  (dllr-list (dotnet:-> (dllr-ints 3 1 2) ("Cast" "System.Object") "ToList"))
  (3 1 2))

(deftest dllr-oftype-type-name-in-chain
  (dllr-list (dotnet:-> (dllr-ints 3 1 2) ("OfType" "System.Int32") "ToList"))
  (3 1 2))

(deftest dllr-cast-type-object-in-chain
  (let ((type (dotnet:resolve-type "System.Object")))
    (dllr-list (dotnet:-> (dllr-ints 3 1 2) ("Cast" type) "ToList")))
  (3 1 2))

(deftest dllr-oftype-type-object-in-chain
  (let ((type (dotnet:resolve-type "System.Int32")))
    (dllr-list (dotnet:-> (dllr-ints 3 1 2) ("OfType" type) "ToList")))
  (3 1 2))

(deftest dllr-cast-after-where
  (dllr-list (dotnet:-> (dllr-ints 3 1 2)
                        ("Where" (lambda (x) (> x 1)))
                        ("Cast" "System.Object") "ToList"))
  (3 2))

(deftest dllr-oftype-after-where
  (dllr-list (dotnet:-> (dllr-ints 3 1 2)
                        ("Where" (lambda (x) (> x 1)))
                        ("OfType" "System.Int32") "ToList"))
  (3 2))
