;;; dotnet:members -- what can be called on a .NET type.
;;;
;;; The interop surface names members with strings, so an image that knows a
;;; type still tells a reader nothing about it. These tests pin the shape of
;;; the listing rather than exact counts: the BCL grows, but the entries a
;;; caller has to be able to find do not change.

(defun dm-names (result)
  (mapcar (lambda (m) (getf m :name)) result))

(defun dm-find (result name signature)
  (find-if (lambda (m) (and (string= (getf m :name) name)
                            (string= (getf m :signature) signature)))
           result))

;;; A plain instance method, with its overloads kept apart by signature.
(deftest dm-instance-method
  (let ((ms (dotnet:members "System.Net.Sockets.TcpClient" :prefix "Connect")))
    (and (dm-find ms "Connect" "(String, Int32) => Void")
         (dm-find ms "Connect" "(IPEndPoint) => Void")
         t))
  t)

;;; Property accessors are listed under the names dotnet:invoke takes, not as
;;; bare property names -- that is the spelling a caller needs.
(deftest dm-property-accessors
  (let ((names (dm-names (dotnet:members "System.Text.StringBuilder" :prefix "set_"))))
    (and (member "set_Length" names :test #'string=)
         (member "set_Capacity" names :test #'string=)
         t))
  t)

;;; An optional trailing parameter is marked, because omitting one is exactly
;;; what makes a call fail to resolve.
(deftest dm-optional-parameter-marked
  (let ((ms (dotnet:members "System.String" :kind :method :prefix "Split")))
    (and (some (lambda (m) (search "?" (getf m :signature))) ms) t))
  t)

;;; :kind keeps one kind.
(deftest dm-kind-filter
  (every (lambda (m) (eq (getf m :kind) :property))
         (dotnet:members "System.Text.StringBuilder" :kind :property))
  t)

;;; :static :static keeps the static side, and reports it.
(deftest dm-static-filter
  (let ((ms (dotnet:members "System.Math" :static :static :prefix "Sqrt")))
    (and ms (every (lambda (m) (getf m :static)) ms) t))
  t)

(deftest dm-instance-filter-drops-statics
  (notany (lambda (m) (getf m :static))
          (dotnet:members "System.Text.StringBuilder" :static :instance))
  t)

;;; A live object answers for its runtime type, so a value at hand can be asked
;;; what it accepts without naming its type.
(deftest dm-from-object
  (let ((names (dm-names (dotnet:members (dotnet:new "System.Text.StringBuilder")
                                         :prefix "AppendLine"))))
    (and (member "AppendLine" names :test #'string=) t))
  t)

;;; Extension methods reach the listing: dotnet:invoke calls them instance-style,
;;; so a listing without them would omit calls that work. LINQ over a string
;;; (IEnumerable<char>) is the case that needs the generic receiver match.
(deftest dm-extension-methods
  (let ((names (dm-names (dotnet:members "System.String" :prefix "Where"))))
    (and (member "Where" names :test #'string=) t))
  t)

(deftest dm-extensions-nil-drops-them
  (dm-names (dotnet:members "System.String" :prefix "Where" :extensions nil))
  nil)

;;; The receiver is dropped from an extension method's printed parameters, so the
;;; signature matches the call the reader is about to write.
(deftest dm-extension-signature-drops-receiver
  (let ((ms (dotnet:members "System.String" :kind :method :prefix "Where")))
    (and ms (notany (lambda (m) (search "(IEnumerable" (getf m :signature))) ms) t))
  t)

;;; :inherited nil keeps what the type itself declares.
(deftest dm-declared-only-is-smaller
  (< (length (dotnet:members "System.Text.StringBuilder" :inherited nil))
     (length (dotnet:members "System.Text.StringBuilder")))
  t)

;;; Every entry carries the full plist.
(deftest dm-plist-shape
  (let ((m (first (dotnet:members "System.Math" :prefix "Sqrt"))))
    (and (stringp (getf m :name))
         (keywordp (getf m :kind))
         (stringp (getf m :signature))
         (stringp (getf m :declaring-type))
         t))
  t)

;;; An unknown type is an error, not an empty listing: an empty answer reads as
;;; "this type has no such member", which is a different fact.
(deftest dm-unknown-type-errors
  (handler-case (progn (dotnet:members "No.Such.Type.At.All") :no-error)
    (error () :error))
  :error)

;;; Keyword values are string designators. A prefix with a fill pointer is what
;;; SUBSEQ of an adjustable string returns -- and what a JSON parser hands back --
;;; so accepting only simple strings dropped the filter and answered with every
;;; member instead.
(deftest dm-prefix-with-fill-pointer
  (let ((adjustable (make-array 7 :element-type 'character
                                  :adjustable t :fill-pointer 7
                                  :initial-contents "AppendL")))
    (= (length (dotnet:members "System.Text.StringBuilder" :prefix adjustable))
       (length (dotnet:members "System.Text.StringBuilder" :prefix "AppendL"))))
  t)

(deftest dm-prefix-symbol-designator
  (plusp (length (dotnet:members "System.Text.StringBuilder" :prefix '|Append|)))
  t)

(deftest dm-kind-string-designator
  (every (lambda (m) (eq (getf m :kind) :property))
         (dotnet:members "System.Text.StringBuilder" :kind "property"))
  t)
