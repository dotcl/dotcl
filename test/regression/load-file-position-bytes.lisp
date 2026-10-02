;;; FILE-POSITION on the stream LOAD and COMPILE-FILE hand to reader macros
;;; counts bytes of the file, as a stream opened on the file does.
;;;
;;; Both read the source into a string first, and the stream over that string
;;; answered character offsets. A reader macro that records where a form starts
;;; and later finds it again in the file -- eclector does, and Coalton through it --
;;; got an offset that was off by the extra bytes of every non-ASCII character
;;; before the form. SBCL answers bytes here; so does WITH-OPEN-FILE in dotcl.

(defvar *flb-seen* '())

(defun flb-file (name text &key bom)
  "Write TEXT to NAME as UTF-8, after a byte order mark when BOM."
  (let ((path (concatenate 'string (namestring (regression-temp-dir)) "/" name)))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :external-format :utf-8)
      (when bom (write-char (code-char #xFEFF) s))
      (write-string text s))
    path))

;;; Three 3-byte characters in a comment (";; " and 9 bytes and a newline: the
;;; form starts at byte 13, character 7), then a form whose reader macro records
;;; FILE-POSITION. The text is built from code points so this file stays ASCII.
(defun flb-text ()
  (format nil ";; ~a~%(flb-mark)~%"
          (map 'string #'code-char '(#x3042 #x3044 #x3046))))

(defun flb-byte-offset-of-mark (path)
  "Where (flb-mark) begins, counted the way a file stream counts."
  (with-open-file (s path)
    (read-line s)
    (file-position s)))

(defmacro flb-mark ()
  nil)

(defun flb-install ()
  (set-macro-character
   #\( (let ((orig (get-macro-character #\( (copy-readtable nil))))
         (lambda (s c)
           (let ((p (file-position s)))
             (let ((form (funcall orig s c)))
               (when (and (consp form) (eq (car form) 'flb-mark))
                 (push (1- p) *flb-seen*))
               form))))))

(defun flb-run (fn path)
  (let ((*flb-seen* '())
        (*readtable* (copy-readtable)))
    (flb-install)
    (funcall fn path)
    (first *flb-seen*)))

(deftest load-file-position-bytes.load
  (let ((path (flb-file "flb-load.lisp" (flb-text))))
    (list (flb-run #'load path) (flb-byte-offset-of-mark path)))
  (13 13))

(deftest-emitting-only load-file-position-bytes.compile-file
  (let ((path (flb-file "flb-compile.lisp" (flb-text))))
    (list (flb-run (lambda (p)
                     (compile-file p :output-file
                                   (concatenate 'string (namestring (regression-temp-dir))
                                                "/flb-compile.fasl")))
                   path)
          (flb-byte-offset-of-mark path)))
  (13 13))

;;; A byte order mark is counted too, as the file stream counts it.
(deftest load-file-position-bytes.bom
  (let ((path (flb-file "flb-bom.lisp" (flb-text) :bom t)))
    (list (flb-run #'load path) (flb-byte-offset-of-mark path)))
  (16 16))
