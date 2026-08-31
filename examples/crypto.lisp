;;;; crypto.lisp -- reaching for the .NET class library, with nothing to install.
;;;;
;;;; Run with:  dotcl examples/crypto.lisp [text]
;;;;
;;;; The companion example, http-json.lisp, pulls a package from NuGet. This one
;;;; needs no network at all: System.Security.Cryptography is part of the
;;;; runtime, so `dotnet:` reaches it the moment dotcl starts. Cryptography is
;;;; also a fair reason to leave Lisp -- these are the platform's audited,
;;;; hardware-accelerated implementations.
;;;;
;;;;   - a hash, checked against a value you can look up
;;;;   - a key derived from a password (PBKDF2)
;;;;   - an authenticated encrypt/decrypt round trip (AES-GCM)

(defparameter *text*
  (or (second (member "--" (dotcl:command-line-arguments) :test #'string=))
      "attack at dawn"))

(defvar *utf8* (dotnet:static "System.Text.Encoding" "UTF8"))

(defun bytes (string) (dotnet:invoke *utf8* "GetBytes" string))
(defun text  (bytes)  (dotnet:invoke *utf8* "GetString" bytes))
(defun hex   (bytes)  (dotnet:static "System.Convert" "ToHexString" bytes))

(defun random-bytes (n)
  (dotnet:static "System.Security.Cryptography.RandomNumberGenerator" "GetBytes" n))

;;; SHA-256. The digest of "abc" is a published test vector, so a run that
;;; prints anything else means the boundary is lying to you.
(defun sha256 (string)
  (dotnet:-> (dotnet:static "System.Security.Cryptography.SHA256" "Create")
             ("ComputeHash" (bytes string))))

;;; PBKDF2. A password is not a key; this is what turns one into the other.
(defun derive-key (password salt)
  (dotnet:-> (dotnet:new "System.Security.Cryptography.Rfc2898DeriveBytes"
                         password salt 100000
                         (dotnet:static "System.Security.Cryptography.HashAlgorithmName" "SHA256"))
             ("GetBytes" 32)))

;;; AES-GCM encrypts and authenticates in one pass: the tag is what makes a
;;; tampered ciphertext fail to decrypt rather than decrypt to garbage.
;;; dotnet:make-array gives .NET the output buffers it wants to write into.
(defun encrypt (key plaintext)
  (let* ((nonce      (random-bytes 12))
         (ciphertext (dotnet:make-array "System.Byte" (dotnet:invoke plaintext "Length")))
         (tag        (dotnet:make-array "System.Byte" 16))
         (gcm        (dotnet:new "System.Security.Cryptography.AesGcm" key 16)))
    (dotnet:invoke gcm "Encrypt" nonce plaintext ciphertext tag)
    (list nonce ciphertext tag)))

(defun decrypt (key nonce ciphertext tag)
  (let ((plaintext (dotnet:make-array "System.Byte" (dotnet:invoke ciphertext "Length")))
        (gcm       (dotnet:new "System.Security.Cryptography.AesGcm" key 16)))
    (dotnet:invoke gcm "Decrypt" nonce ciphertext tag plaintext)
    plaintext))

(format t "~&sha256(\"abc\")   ~a~%" (hex (sha256 "abc")))
(format t "  the published vector is BA7816BF...F20015AD~%~%")

(let* ((salt (random-bytes 16))
       (key  (derive-key "correct horse battery staple" salt)))
  (format t "key from password  ~a~%" (subseq (hex key) 0 32))
  (destructuring-bind (nonce ciphertext tag) (encrypt key (bytes *text*))
    (format t "ciphertext         ~a~%" (hex ciphertext))
    (format t "tag                ~a~%" (hex tag))
    (format t "decrypted          ~s~%" (text (decrypt key nonce ciphertext tag)))))

;;; Not everything in the class library is reachable this way. Members whose
;;; parameters are Span<T> exist only in that form -- CryptographicOperations
;;; .FixedTimeEquals is one -- and a reflected call cannot hand a Span across
;;; the boundary, so it reports the method as not found. Where the BCL kept an
;;; array overload, as everything above did, the call goes through as written.
