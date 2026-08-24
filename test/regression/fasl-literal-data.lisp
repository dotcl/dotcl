;;; A literal that cannot be rebuilt cons-by-cons — circular, or with shared
;;; structure whose EQ identity has to survive — is compiled into its printed
;;; representation plus a load-time read of it. That representation now travels
;;; in the fasl's data section (DefineInitializedData) instead of the module's
;;; #US string heap, and comes back through a UTF-8 decode.
;;;
;;; Two things can break in that move and in nothing else: the bytes (any
;;; character outside ASCII, including astral ones, has to survive the encode and
;;; decode intact) and the graph (a cycle must still be a cycle, and structure
;;; shared between occurrences must still be EQ). These pin both.

(defvar *fld-dir*
  (let ((dir (concatenate 'string
                          (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR")
                                                  (dotcl:getenv "TEMP")
                                                  "/tmp"))
                          "/dotcl-fld-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %fld-compile-and-load (source name)
  "Write SOURCE (a string of Lisp text) to a file, compile it, load the fasl."
  (let ((lisp (concatenate 'string *fld-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede
                            :external-format :utf-8)
      (write-string source s))
    (load (compile-file lisp))
    t))

;;; ---- a circular literal is still circular after the round trip ----

(deftest-compiled-only fld-circular-literal
  (progn
    (%fld-compile-and-load
     "(in-package :cl-user)
      (defparameter *fld-circ* '#1=(1 2 3 . #1#))
      (defun %fld-circ-take (n)
        (loop repeat n for x in *fld-circ* collect x))
      (defun %fld-circ-cycles-p ()
        (eq *fld-circ* (cdddr *fld-circ*)))"
     "fld-circ")
    (list (funcall (intern "%FLD-CIRC-TAKE") 7)
          (funcall (intern "%FLD-CIRC-CYCLES-P"))))
  ((1 2 3 1 2 3 1) t))

;;; ---- shared structure keeps EQ identity across occurrences ----

(deftest-compiled-only fld-shared-literal
  (progn
    (%fld-compile-and-load
     "(in-package :cl-user)
      (defparameter *fld-shared* '(#1=(:a :b) #1# (#1#)))
      (defun %fld-shared-eq-p ()
        (let ((a (first *fld-shared*))
              (b (second *fld-shared*))
              (c (car (third *fld-shared*))))
          (list (eq a b) (eq b c) (equal a '(:a :b)))))"
     "fld-shared")
    (funcall (intern "%FLD-SHARED-EQ-P")))
  (t t t))

;;; ---- the bytes survive: non-ASCII inside a circular literal ----
;;; The representation is UTF-8 in the data section, so anything that is not
;;; plain ASCII exercises the encode/decode pair. Astral characters (outside the
;;; BMP) are the case that separates a correct UTF-8 round trip from one that
;;; splits a surrogate pair.

(deftest-compiled-only fld-non-ascii-literal
  (progn
    (%fld-compile-and-load
     (concatenate 'string
                  "(in-package :cl-user)
      (defparameter *fld-uni* '#1=(\"" (string (code-char #x3042)) ; HIRAGANA A
                  (string (code-char #x1F600))                    ; astral
                  "\" " (string (code-char #x00E9))               ; e with acute
                  " . #1#))
      (defun %fld-uni-string () (first *fld-uni*))
      (defun %fld-uni-symbol-name () (symbol-name (second *fld-uni*)))
      (defun %fld-uni-cycles-p () (eq *fld-uni* (cddr *fld-uni*)))")
     "fld-uni")
    (list (funcall (intern "%FLD-UNI-STRING"))
          (funcall (intern "%FLD-UNI-SYMBOL-NAME"))
          (funcall (intern "%FLD-UNI-CYCLES-P"))))
  (#.(concatenate 'string (string (code-char #x3042)) (string (code-char #x1F600)))
   #.(string-upcase (string (code-char #x00E9)))
   t))

;;; ---- a long representation still reads back whole ----
;;; Guards the path where the representation is large enough to matter: it is
;;; emitted as data whatever its size, and above a limit it is split across
;;; several data fields and rejoined at load. This one only reaches the first
;;; case (splitting needs megabytes, too slow for the suite), but it does catch
;;; a truncation or an off-by-one in the byte count.

(deftest-compiled-only fld-long-literal
  (progn
    (%fld-compile-and-load
     (with-output-to-string (s)
       (write-string "(in-package :cl-user)
      (defparameter *fld-long* '#1=(" s)
       (dotimes (i 2000) (format s "(:k~d \"v~d\") " i i))
       (write-string " . #1#))
      (defun %fld-long-len () (length (%fld-long-prefix)))
      (defun %fld-long-prefix () (loop repeat 2000 for x in *fld-long* collect x))
      (defun %fld-long-last () (nth 1999 (%fld-long-prefix)))" s))
     "fld-long")
    (list (funcall (intern "%FLD-LONG-LEN"))
          (funcall (intern "%FLD-LONG-LAST"))))
  (2000 (:k1999 "v1999")))

;;; ---- float types survive a differing *read-default-float-format* ----
;;; Which floats print with an exponent marker depends on
;;; *read-default-float-format*, so a representation printed under one setting
;;; and read under another comes back as the right value with the wrong type,
;;; and nothing signals. Compiling under DOUBLE-FLOAT and loading under the
;;; standard default is exactly that case.

(defun %fld-compile-and-load-as-double (source name)
  (let ((lisp (concatenate 'string *fld-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (write-string source s))
    (let ((fasl (let ((*read-default-float-format* 'double-float))
                  (compile-file lisp))))
      (load fasl)
      t)))

(deftest-compiled-only fld-float-format-independence
  (progn
    (%fld-compile-and-load-as-double
     (with-output-to-string (s)
       (write-string "(in-package :cl-user)
      (defparameter *fld-floats* '(" s)
       ;; Long enough to take the reader path, not just the inline one.
       (dotimes (i 40) (format s "~d.5 ~d.25d0 " i i))
       (write-string "))
      (defun %fld-float-types ()
        (list (type-of (first *fld-floats*)) (type-of (second *fld-floats*))))
      (defun %fld-float-values () (list (first *fld-floats*) (second *fld-floats*)))" s))
     "fld-floats")
    (list (funcall (intern "%FLD-FLOAT-TYPES"))
          (funcall (intern "%FLD-FLOAT-VALUES"))))
  ;; Read as doubles at compile time, so doubles after the round trip.
  ((double-float double-float) (0.5d0 0.25d0)))

;;; The ordinary case is unchanged: read under the standard default, a bare
;;; literal is a single float and a d0 literal is a double.

(deftest-compiled-only fld-float-default-format
  (progn
    (%fld-compile-and-load
     (with-output-to-string (s)
       (write-string "(in-package :cl-user)
      (defparameter *fld-floats2* '(" s)
       (dotimes (i 40) (format s "~d.5 ~d.25d0 " i i))
       (write-string "))
      (defun %fld-float2-types ()
        (list (type-of (first *fld-floats2*)) (type-of (second *fld-floats2*))))" s))
     "fld-floats2")
    (funcall (intern "%FLD-FLOAT2-TYPES")))
  (single-float double-float))
