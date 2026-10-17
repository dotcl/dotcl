;;; DOTCL:STRING-TO-OCTETS, DOTCL:OCTETS-TO-STRING (as in sb-ext) and
;;; DOTCL:GETPID (as sb-posix:getpid). The SLIME / SLY / micros backends
;;; encoded UTF-8 through System.Text.Encoding a byte at a time, and returned
;;; 0 for the process id, for want of them.

(defun %sog-list (v) (coerce v 'list))

(deftest string-octets.utf-8-default
  (let ((v (dotcl:string-to-octets (coerce (list #\a (code-char 233) (code-char #x3042)) 'string))))
    (list (typep v '(simple-array (unsigned-byte 8) (*))) (%sog-list v)))
  (t (97 195 169 227 129 130)))

(deftest string-octets.round-trip
  ;; A character outside the BMP is a surrogate pair in a dotcl string; UTF-8
  ;; encodes the pair as the one four-byte sequence.
  (let* ((s (coerce (list #\x (code-char 233) (code-char #x3042)
                          (code-char #xD83D) (code-char #xDE00) #\y)
                    'string))
         (v (dotcl:string-to-octets s :external-format :utf-8)))
    (list (%sog-list (subseq v 6 10))
          (string= s (dotcl:octets-to-string v :external-format :utf-8))))
  ((240 159 152 128) t))

(deftest string-octets.bounds-and-null-terminate
  (list (%sog-list (dotcl:string-to-octets "abcd" :start 1 :end 3))
        (%sog-list (dotcl:string-to-octets "ab" :null-terminate t))
        (dotcl:octets-to-string (coerce '(104 101 108 108 111) '(vector (unsigned-byte 8)))
                                :start 1 :end 4))
  ((98 99) (97 98 0) "ell"))

(deftest string-octets.other-formats
  (list (%sog-list (dotcl:string-to-octets (string (code-char 233)) :external-format :latin-1))
        (%sog-list (dotcl:string-to-octets "A" :external-format :utf-16le))
        (dotcl:octets-to-string (coerce '(233) '(vector (unsigned-byte 8))) :external-format :latin-1))
  ((233) (65 0) #.(string (code-char 233))))

(deftest string-octets.general-vector
  ;; Any vector of octets, not only a specialized one.
  (dotcl:octets-to-string (vector 104 105))
  "hi")

(deftest string-octets.errors
  (list (handler-case (dotcl:string-to-octets 42) (type-error () :type-error))
        (handler-case (dotcl:octets-to-string (vector 1 300)) (type-error () :type-error))
        (handler-case (dotcl:string-to-octets "a" :bogus 1) (error () :error)))
  (:type-error :type-error :error))

(deftest getpid.matches-process
  (= (dotcl:getpid)
     (dotnet:invoke (dotnet:static "System.Diagnostics.Process" "GetCurrentProcess") "Id"))
  t)
