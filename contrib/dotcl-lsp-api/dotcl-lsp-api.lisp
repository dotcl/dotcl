;;; dotcl-lsp-api -- what an editor asks a live image about the code at a point.
;;;
;;; The shape of the answers is LSP's (a label, a kind, a detail string, and the
;;; range to replace); the content is dotcl's. No protocol lives here: the wire
;;; belongs to whatever speaks it, and this file is equally callable from the
;;; bundled REPL, from a swank slimefun, or from an LSP server in another image.
;;;
;;;   (dotcl-lsp-api:completions "(dotnet:invoke sb \"App" 22)
;;;   => (:start 19 :end 22
;;;       :items ((:label "Append" :kind :method
;;;                :detail "(String) => StringBuilder") ...))
;;;
;;; Text and offset rather than a parsed form, because that is what every caller
;;; has: the REPL holds the line up to point, an LSP server holds the document
;;; and a position. A parsed-form interface would make each of them write a Lisp
;;; reader first.

(defpackage :dotcl-lsp-api
  (:use :cl)
  (:export #:completions #:documentation-url #:describe-at))

(in-package :dotcl-lsp-api)

;;;; Scanning

(defun %scan (text offset)
  "Describe where OFFSET sits in TEXT.
Returns (values IN-STRING-P STRING-START ELEMENTS TOKEN-START), where ELEMENTS
are the (start . end) pairs of the enclosing form's already-complete elements,
operator first."
  (let ((n (min offset (length text)))
        (i 0)
        (in-string nil)
        (escape nil)
        (string-start nil)
        (token-start nil)
        (elements '())
        (element-stack '())
        (open-stack '()))
    (flet ((finish-token (end)
             (when token-start
               (push (cons token-start end) elements)
               (setf token-start nil))))
      (loop while (< i n) do
        (let ((c (char text i)))
          (cond
            (in-string
             (cond (escape (setf escape nil))
                   ((char= c #\\) (setf escape t))
                   ((char= c #\")
                    (setf in-string nil)
                    (push (cons string-start (1+ i)) elements)
                    (setf string-start nil)))
             (incf i))
            ((char= c #\;)
             (finish-token i)
             (loop while (and (< i n) (char/= (char text i) #\Newline)) do (incf i)))
            ((char= c #\")
             (finish-token i)
             (setf in-string t string-start i)
             (incf i))
            ((char= c #\()
             (finish-token i)
             (push elements element-stack)
             (push i open-stack)
             (setf elements '())
             (incf i))
            ((char= c #\))
             (finish-token i)
             (let ((start (pop open-stack)))
               (setf elements (or (pop element-stack) '()))
               (when start (push (cons start (1+ i)) elements)))
             (incf i))
            ((member c '(#\Space #\Tab #\Newline #\Return #\Page))
             (finish-token i)
             (incf i))
            (t
             (unless token-start (setf token-start i))
             (incf i))))))
    (values in-string string-start (reverse elements) token-start)))

(defun %text (text pair)
  (subseq text (car pair) (cdr pair)))

(defun %string-literal-p (token)
  (and (plusp (length token)) (char= (char token 0) #\")))

(defun %unquote (token)
  (string-trim "\"" token))

(defun %find-package-from (name package)
  "The package NAME denotes when read in PACKAGE.
A package-local nickname is only a name inside the package that declares it, so
looking one up globally answers NIL for code that is perfectly ordinary in its
own file."
  (let ((nicknames (ignore-errors (dotcl:package-local-nicknames package))))
    (or (cdr (assoc name nicknames :test #'string-equal))
        (find-package name))))

(defun %token-symbol (token package)
  "Resolve TOKEN to an existing symbol without interning anything."
  (let* ((colon (position #\: token))
         (last-colon (position #\: token :from-end t)))
    (if colon
        (let ((pkg (%find-package-from (string-upcase (subseq token 0 colon)) package)))
          (when pkg
            (find-symbol (string-upcase (subseq token (1+ last-colon))) pkg)))
        (find-symbol (string-upcase token) package))))

;;;; The package the text is read in
;;;
;;; A file says which package it is read in, and the answer depends on it: a
;;; bare INVOKE means dotnet:invoke in a package that uses DOTNET and nothing at
;;; all in CL-USER. The image's own *PACKAGE* is not that package -- it is
;;; wherever the server happens to sit -- so the text has to be asked.

(defparameter *in-package-forms* '("IN-PACKAGE" "CL:IN-PACKAGE" "COMMON-LISP:IN-PACKAGE"))

(defun %package-name-token (token)
  "The package name a designator token denotes: :foo #:foo \"foo\" 'foo foo."
  (let ((name (string-left-trim "'#:" token)))
    (if (and (> (length name) 1) (char= (char name 0) #\"))
        (string-trim "\"" name)
        (string-upcase (string-trim "\"" name)))))

(defun %text-package (text offset default)
  "The package established by the last (in-package ...) before OFFSET.
Matched on the text rather than read: the form may name a package this image
does not have, and a file being edited is not always readable anyway."
  (let ((result default)
        (limit (min offset (length text)))
        (position 0)
        (whitespace '(#\Space #\Tab #\Newline #\Return)))
    (loop
      (let ((open (position #\( text :start position :end limit)))
        (unless open (return result))
        (setf position (1+ open))
        (let* ((head-end (or (position-if (lambda (c) (member c whitespace)) text
                                          :start position :end limit)
                             limit))
               (head (string-upcase (subseq text position head-end))))
          (when (member head *in-package-forms* :test #'string=)
            (let* ((name-start (or (position-if-not (lambda (c) (member c whitespace)) text
                                                    :start head-end :end limit)
                                   limit))
                   (name-end (or (position-if (lambda (c) (or (member c whitespace)
                                                              (char= c #\))))
                                              text :start name-start :end limit)
                                 limit))
                   (found (and (< name-start name-end)
                               (find-package
                                (%package-name-token (subseq text name-start name-end))))))
              (when found (setf result found)))))))))

;;;; Receivers
;;;
;;; Completion runs on every keystroke, so a receiver is only resolved when that
;;; can be done without running the user's code: a literal type name, or a
;;; variable already bound in the image. Anything else -- a call that would
;;; construct or open something -- is left alone and no candidates are offered.

(defun %dotnet-symbol (name)
  (let ((pkg (find-package "DOTNET")))
    (and pkg (find-symbol name pkg))))

(defun %call-dotnet (name &rest args)
  (let ((sym (%dotnet-symbol name)))
    (when (and sym (fboundp sym))
      (apply (symbol-function sym) args))))

(defun %type-p (value)
  (and value (%call-dotnet "IS-INSTANCE-OF" value "System.Type")))

(defun %receiver-type (token package)
  (cond
    ((%string-literal-p token)
     (ignore-errors (%call-dotnet "RESOLVE-TYPE" (%unquote token))))
    (t
     (let ((sym (%token-symbol token package)))
       (when (and sym (boundp sym))
         (let ((value (symbol-value sym)))
           (if (%type-p value)
               value
               (ignore-errors (%call-dotnet "OBJECT-TYPE" value)))))))))

;;;; Items

(defun %member-items (type prefix &key (static :both))
  "Candidates for a member name, each with what .NET says it does.

The sentence comes from the XML documentation the framework ships, which is
read in the background: an early list carries signatures alone and the next
keystroke fills the prose in."
  (let ((members (ignore-errors
                  (%call-dotnet "MEMBERS" type :prefix prefix :static static
                                :documentation t))))
    (loop for m in members
          collect (list :label (getf m :name)
                        :kind (getf m :kind)
                        :detail (getf m :signature)
                        :documentation (getf m :documentation)))))

(defun %symbol-kind (symbol)
  (cond ((macro-function symbol) :macro)
        ((fboundp symbol) :function)
        ((find-class symbol nil) :class)
        ((boundp symbol) :variable)
        (t :symbol)))

(defparameter *symbol-limit* 200
  "Upper bound on symbol candidates, so a one-character prefix stays cheap.")

(defun %sorted-unique (items)
  (sort (remove-duplicates items :key (lambda (i) (getf i :label)) :test #'string=)
        #'string< :key (lambda (i) (getf i :label))))

(defun %matching-symbols (name-prefix package externals-only qualifier downcase-p)
  "Symbols of PACKAGE starting with NAME-PREFIX, labelled with QUALIFIER."
  (let ((items '())
        (count 0)
        (length (length name-prefix)))
    (flet ((consider (symbol)
             (let ((name (symbol-name symbol)))
               (when (and (>= (length name) length)
                          (string= name-prefix name :end2 length))
                 (push (list :label (concatenate 'string qualifier
                                                 (if downcase-p (string-downcase name) name))
                             :kind (%symbol-kind symbol)
                             :detail (package-name (symbol-package symbol)))
                       items)
                 (incf count)))))
      (block scan
        (if externals-only
            (do-external-symbols (s package) (consider s)
              (when (>= count *symbol-limit*) (return-from scan)))
            (do-symbols (s package) (consider s)
              (when (>= count *symbol-limit*) (return-from scan))))))
    items))

(defun %package-items (prefix package downcase-p)
  "Package names starting with PREFIX, offered with the colon that follows them.
A package qualifier is part of the token being typed, so the names have to be
candidates as much as the symbols are."
  (let ((upper (string-upcase prefix))
        (items '()))
    (flet ((consider (name)
             (when (and (>= (length name) (length upper))
                        (string= upper name :end2 (length upper)))
               (push (list :label (concatenate 'string
                                               (if downcase-p (string-downcase name) name)
                                               ":")
                           :kind :package
                           :detail "package")
                     items))))
      (dolist (p (list-all-packages))
        (consider (package-name p))
        (dolist (nickname (package-nicknames p)) (consider nickname)))
      (dolist (entry (ignore-errors (dotcl:package-local-nicknames package)))
        (consider (car entry))))
    items))

(defun %symbol-items (prefix package)
  "Candidates for a symbol token, which may carry a package qualifier.

`dotnet:inv' names one symbol and `dotn' may name a package, so both halves of
the token are completed: the text being replaced is the whole token, and the
label carries the qualifier back."
  (when (plusp (length prefix))
    (let* ((downcase-p (notany #'upper-case-p prefix))
           (colon (position #\: prefix)))
      (%sorted-unique
       (if colon
           (let* ((double (and (< (1+ colon) (length prefix))
                               (char= (char prefix (1+ colon)) #\:)))
                  (package-part (subseq prefix 0 colon))
                  (name-part (string-upcase (subseq prefix (+ colon (if double 2 1)))))
                  (target (%find-package-from (string-upcase package-part) package)))
             (when target
               (%matching-symbols name-part target (not double)
                                  (subseq prefix 0 (+ colon (if double 2 1)))
                                  downcase-p)))
           (append (%matching-symbols (string-upcase prefix) package nil "" downcase-p)
                   (%package-items prefix package downcase-p)))))))

;;;; Entry point

(defvar *type-index-incomplete* nil
  "Bound while building a result whose type names are not all in yet.")

(defun %type-name-items (prefix)
  "Type names starting with PREFIX, one namespace step at a time.
The first argument of these forms is a type, and unlike a member name it needs
no receiver to be resolved -- the set of types the image can name is enough.

Types that are merely available reach the index through a background scan, so
an early answer can be partial; that is recorded for the caller to pass on."
  (multiple-value-bind (entries complete)
      (ignore-errors (%call-dotnet "TYPE-NAMES" prefix))
    (unless complete (setf *type-index-incomplete* t))
    (loop for entry in (or entries '())
          collect (list :label (getf entry :name)
                        :kind (if (eq (getf entry :kind) :namespace) :namespace :class)
                        :detail (if (eq (getf entry :kind) :namespace)
                                    "namespace" ".NET type")))))

(defun %interop-role (text elements package)
  "What a string in this form denotes: :TYPE, :MEMBER, or NIL for neither.
Second value: the element holding the receiver, when the role has one. Third:
whether only static members apply.

Which argument of which form is a type name is decided here alone, so that
completion and documentation cannot come to disagree about it."
  (let* ((index (length elements))
         (operator (when elements (%token-symbol (%text text (first elements)) package))))
    (when operator
      (let ((invoke     (%dotnet-symbol "INVOKE"))
            (set-invoke (%dotnet-symbol "%SET-INVOKE"))
            (static     (%dotnet-symbol "STATIC"))
            (chain      (%dotnet-symbol "->")))
        (cond
          ;; (dotnet:invoke receiver "Member ...) -- the string is element 2.
          ((and (= index 2) (member operator (list invoke set-invoke chain)))
           (values :member (second elements) nil))
          ;; (dotnet:static "Type" "Member ...) -- statics of a literal type.
          ((and (= index 2) (eq operator static))
           (values :member (second elements) t))
          ;; The first argument of these forms is a type: (dotnet:new "Type ...,
          ;; (dotnet:static "Type" ..., and the literal-receiver spelling of a
          ;; call. No receiver has to be resolved to answer.
          ((and (= index 1)
                (member operator (list invoke set-invoke chain static
                                       (%dotnet-symbol "NEW")
                                       (%dotnet-symbol "RESOLVE-TYPE")
                                       (%dotnet-symbol "CLASS-FOR-TYPE")
                                       (%dotnet-symbol "MAKE-GENERIC-TYPE")
                                       (%dotnet-symbol "MAKE-ARRAY")
                                       (%dotnet-symbol "NEW-ARRAY")
                                       (%dotnet-symbol "MEMBERS"))))
           :type)
          ;; These take the value first and the type second.
          ((and (= index 2)
                (member operator (list (%dotnet-symbol "IS-INSTANCE-OF")
                                       (%dotnet-symbol "CAST")
                                       (%dotnet-symbol "BOX"))))
           :type))))))

(defun %interop-completions (text offset string-start elements package)
  (let ((prefix (subseq text (1+ string-start) offset))
        (result nil))
    (multiple-value-bind (role receiver static-only)
        (%interop-role text elements package)
      (case role
        (:member
         (let ((type (%receiver-type (%text text receiver) package)))
           (when type
             (setf result (%member-items type prefix
                                         :static (if static-only :static :both))))))
        (:type
         (setf result (%type-name-items prefix)))))
    (when result
      (append (list :start (1+ string-start) :end offset :items result)
              ;; The caller says so to its own client: an LSP one turns this into
              ;; isIncomplete, which is what makes it ask again as more of the
              ;; index lands.
              (when *type-index-incomplete* (list :incomplete t))))))

(defun completions (text offset &key package)
  "Completion candidates for the cursor at OFFSET in TEXT.

Returns (:start N :end M :items ((:label ... :kind ... :detail ...) ...)), where
START and END delimit the text the caller should replace, or NIL when there is
nothing to offer. Inside a string in .NET call position the candidates are that
type's members; elsewhere they are symbols visible in the package.

PACKAGE overrides the package the text is read in. By default it comes from the
text's own (in-package ...), because that is what decides whether a bare INVOKE
names dotnet:invoke -- the image's *PACKAGE* is wherever the server sits, which
is nobody's file."
  (let ((package (or package (%text-package text offset *package*)))
        (*type-index-incomplete* nil))
    (multiple-value-bind (in-string string-start elements token-start)
        (%scan text offset)
      (if in-string
          (%interop-completions text offset string-start elements package)
          (when token-start
            (let ((items (%symbol-items (subseq text token-start offset) package)))
              (when items
                (list :start token-start :end offset :items items))))))))

;;;; Documentation
;;;
;;; What can be offered for the name at point is a URL, because the reference
;;; for both halves of this language is on the web: the HyperSpec for CL,
;;; learn.microsoft.com for .NET. Neither can be shown in an editor pane -- both
;;; refuse to be framed -- so the answer is a URL, and the client opens it
;;; wherever it opens links.

(defparameter *clhs-url* "http://l1sp.org/cl/~A"
  "Where a COMMON-LISP symbol's page is, given its name.
A redirector rather than the HyperSpec itself, because HyperSpec filenames
(f_car_c.htm) do not follow from the symbol name and would need a table of about
a thousand entries. Rebind this to point somewhere that does not need one.")

(defparameter *dotnet-doc-url* "https://learn.microsoft.com/dotnet/api/~A"
  "Where a .NET type or member page is, given its documentation path.")

(defun %url-encode (name)
  "Percent-encode NAME for one path segment.
Symbol names are full of characters a URL reads as syntax: 1+ // char= and
*print-base* all need it. Only called for COMMON-LISP symbols, whose names are
ASCII, so a character is a byte here."
  (with-output-to-string (out)
    (loop for c across name
          do (if (or (find c "-._~")
                     (and (< (char-code c) 128) (alphanumericp c)))
                 (write-char c out)
                 (format out "%~2,'0X" (char-code c))))))

(defun %learn-path (type-name &optional member)
  "The documentation path for a CLR name.
Nesting is written with a dot rather than a plus, generic arity with a hyphen
rather than a backtick, and the whole path in lower case."
  (let ((path (substitute #\. #\+ (substitute #\- (code-char 96) type-name))))
    (string-downcase (if member (concatenate 'string path "." member) path))))

(defun %resolve-type-for-doc (name)
  "Resolve NAME to a type, including the arities a generic name is offered without.
A candidate reads List rather than List with its backtick arity, because that is
the spelling dotnet:make-generic-type takes, but the reference is filed under it."
  (or (ignore-errors (%call-dotnet "RESOLVE-TYPE" name))
      (loop for arity from 1 to 4
            thereis (ignore-errors
                     (%call-dotnet "RESOLVE-TYPE"
                                   (format nil "~A~C~D" name (code-char 96) arity))))))

(defun %type-full-name (type)
  (ignore-errors (%call-dotnet "INVOKE" type "FullName")))

(defun %declared-member (type name)
  "The type NAME is declared in, and its spelling there, or NIL.
An inherited member is filed under the type that declares it, so looking for
ToString on the receiver's own page asks for a page that is not there."
  (let ((match (find name (ignore-errors (%call-dotnet "MEMBERS" type :prefix name))
                     :key (lambda (m) (getf m :name))
                     :test #'string-equal)))
    (when match
      (values (getf match :declaring-type) (getf match :name)))))

;;;; What the cursor is on
;;;
;;; Both of the questions an editor asks about a name -- where its page is, and
;;; what it says -- start by working out what the name denotes. That is done
;;; once here so the two cannot disagree about it.

(defun %token-char-p (c)
  (not (member c '(#\Space #\Tab #\Newline #\Return #\Page #\( #\) #\" #\;))))

(defun %token-end (text offset)
  (let ((n (length text))
        (i (min offset (length text))))
    (loop while (and (< i n) (%token-char-p (char text i))) do (incf i))
    i))

(defun %string-end (text offset)
  "Where the string containing OFFSET ends, not counting the closing quote."
  (let ((n (length text))
        (i (min offset (length text))))
    (loop while (< i n) do
      (let ((c (char text i)))
        (cond ((char= c #\\) (incf i 2))
              ((char= c #\") (return))
              (t (incf i)))))
    (min i n)))

(defun %at-point (text offset package)
  "What the name at OFFSET denotes.

Returns (values :SYMBOL symbol), or (values :DOTNET type member-name) where the
member name is NIL when the name is the type's own, or NIL for a position that
denotes nothing. The name is taken whole rather than up to the cursor, as
completion takes it: the reader is pointing at something already written."
  (multiple-value-bind (in-string string-start elements token-start)
      (%scan text offset)
    (if in-string
        (let ((name (subseq text (1+ string-start) (%string-end text offset))))
          (when (plusp (length name))
            (multiple-value-bind (role receiver) (%interop-role text elements package)
              (case role
                (:type
                 (let ((type (%resolve-type-for-doc name)))
                   (when type (values :dotnet type nil))))
                (:member
                 (let ((type (%receiver-type (%text text receiver) package)))
                   (when type (values :dotnet type name))))))))
        (let ((start (or token-start
                         (when (and (< offset (length text))
                                    (%token-char-p (char text offset)))
                           offset))))
          (when start
            (let* ((token (subseq text start (%token-end text start)))
                   (name (string-left-trim "'#" token))
                   (symbol (and (plusp (length name)) (%token-symbol name package))))
              (when symbol (values :symbol symbol))))))))

;;;; Where the reference is

(defun %symbol-url (symbol)
  (when (eq (symbol-package symbol) (find-package "COMMON-LISP"))
    (format nil *clhs-url* (%url-encode (string-downcase (symbol-name symbol))))))

(defun %dotnet-url (type member)
  (if member
      (multiple-value-bind (declaring spelling) (%declared-member type member)
        (let ((full (or declaring (%type-full-name type))))
          (when full (format nil *dotnet-doc-url* (%learn-path full spelling)))))
      (let ((full (%type-full-name type)))
        (when full (format nil *dotnet-doc-url* (%learn-path full))))))

(defun documentation-url (text offset &key package)
  "The reference page for the name at OFFSET in TEXT, or NIL when there is none.

Returns (:url ... :name ... :kind :symbol|:type|:member).

Of the symbols an image holds only the COMMON-LISP ones have a page: the rest
are the reader's own code or a library, and neither is on the web at a place
that can be guessed."
  (let ((package (or package (%text-package text offset *package*))))
    (multiple-value-bind (kind object member) (%at-point text offset package)
      (case kind
        (:symbol
         (let ((url (%symbol-url object)))
           (when url
             (list :url url :name (string-downcase (symbol-name object))
                   :kind :symbol))))
        (:dotnet
         (let ((url (%dotnet-url object member)))
           (when url
             (multiple-value-bind (declaring spelling)
                 (when member (%declared-member object member))
               (let ((full (or declaring (%type-full-name object))))
                 (list :url url
                       :name (if member
                                 (concatenate 'string full "." (or spelling member))
                                 full)
                       :kind (if member :member :type)))))))))))

;;;; What the reference says
;;;
;;; The same name again, but answered from the image rather than pointed at: a
;;; lambda list and a docstring for a symbol, the summary and the overloads for
;;; a .NET member. This is what an editor shows without the reader going
;;; anywhere, and it is the part a popup can carry.

(defun %symbol-signature (symbol)
  "How the symbol is called, as it would be written, or NIL.

A function the image did not compile from Lisp -- a standard one, or one the
runtime registers -- keeps no lambda list, and (mapcar) as a signature says
something false rather than nothing."
  (when (fboundp symbol)
    (let ((lambda-list (ignore-errors (dotcl:function-lambda-list symbol))))
      (when lambda-list
        (string-downcase (princ-to-string (cons symbol lambda-list)))))))

(defun %symbol-description (symbol)
  (list :name (string-downcase (symbol-name symbol))
        :kind (%symbol-kind symbol)
        :package (package-name (symbol-package symbol))
        :signatures (let ((signature (%symbol-signature symbol)))
                      (when signature (list signature)))
        :documentation (or (documentation symbol 'function)
                           (documentation symbol 'variable)
                           (documentation symbol 'type))
        :url (%symbol-url symbol)))

(defun %member-signatures (type member)
  "Every way MEMBER can be called, as dotnet:members prints them."
  (loop for m in (ignore-errors (%call-dotnet "MEMBERS" type :prefix member))
        when (string-equal (getf m :name) member)
          collect (getf m :signature)))

(defun %dotnet-description (type member)
  (let ((doc (ignore-errors (%call-dotnet "DOCUMENTATION" type member)))
        (full (%type-full-name type)))
    (multiple-value-bind (declaring spelling) (when member (%declared-member type member))
      (list :name (if member
                      (concatenate 'string (or declaring full) "." (or spelling member))
                      full)
            :kind (if member :member :type)
            :signatures (when member (%member-signatures type member))
            :documentation (getf doc :summary)
            :parameters (getf doc :parameters)
            :returns (getf doc :returns)
            :url (%dotnet-url type member)))))

(defun describe-at (text offset &key package)
  "What the name at OFFSET is, for an editor to show where the cursor already is.

Returns (:name :kind :signatures :documentation :url), with :package for a
symbol and :parameters / :returns for a .NET member, or NIL.

Everything in it is optional but the name: a symbol with no docstring and a
member the framework documents nowhere still have a signature worth showing."
  (let ((package (or package (%text-package text offset *package*))))
    (multiple-value-bind (kind object member) (%at-point text offset package)
      (case kind
        (:symbol (%symbol-description object))
        (:dotnet (%dotnet-description object member))))))

(provide "dotcl-lsp-api")
