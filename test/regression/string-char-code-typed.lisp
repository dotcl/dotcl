;;; (CHAR-CODE (SCHAR s i)) reads the code directly.
;;;
;;; Written the ordinary way, that composition cost a full generic call per
;;; character -- symbol lookup, virtual invoke, a boxed index going in and a
;;; character object coming out -- so a scan over 1 MB allocated 983,042 boxed
;;; indices and ran about a hundred times slower than the same loop in C#. The
;;; compiler now lowers the composition to one typed call.
;;;
;;; The tests are in two halves: that the typed call is what gets emitted (the
;;; SIL), and that every spelling still produces the value the generic path
;;; produces (the fast path must not change what CHAR-CODE answers, and the
;;; shapes it declines must still work).

(setf dotcl:*save-sil* t)

(defun %scc-typed-p (fn)
  "True when FN's SIL reads character codes through the typed entry."
  (and (search "CharCodeAtL" (princ-to-string (dotcl:function-sil fn))) t))

(defun %scc-generic-p (fn)
  "True when FN's SIL still calls SCHAR through the symbol."
  (and (search "SCHAR" (princ-to-string (dotcl:function-sil fn))) t))

;;; ---- what gets emitted ----

(defun %scc-sum (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0) (n (length s)))
    (declare (fixnum acc n))
    (dotimes (i n acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (char-code (schar s i))))))))

(deftest-compiled-only string-char-code-typed.schar-is-typed
  (list (%scc-typed-p #'%scc-sum) (%scc-generic-p #'%scc-sum))
  (t nil))

;; The CHAR spelling is the same composition and takes the same path.
(defun %scc-sum-char (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0) (n (length s)))
    (declare (fixnum acc n))
    (dotimes (i n acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (char-code (char s i))))))))

(deftest-compiled-only string-char-code-typed.char-is-typed
  (%scc-typed-p #'%scc-sum-char)
  t)

;; A computed index is not a simple expression: it goes to a temp first, and the
;; typed call is still what runs.
(defun %scc-sum-computed (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0) (n (length s)))
    (declare (fixnum acc n))
    (dotimes (i n acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (char-code (schar s (the fixnum (- n 1 i))))))))))

(deftest-compiled-only string-char-code-typed.computed-index-is-typed
  (%scc-typed-p #'%scc-sum-computed)
  t)

;; A local function shadowing SCHAR is not this composition at all: the shadow
;; has to be called, so the typed entry must not appear.
(defun %scc-shadowed (s)
  (flet ((schar (str i) (declare (ignore str i)) #\Z))
    (char-code (schar s 0))))

(deftest-compiled-only string-char-code-typed.local-shadow-not-typed
  (list (%scc-typed-p #'%scc-shadowed) (%scc-shadowed "abc"))
  (nil 90))

;;; ---- what it answers ----

;; A reference that cannot be fused: the character is bound first, so the value
;; comes back through the ordinary CHAR-CODE of a character object.
(defun %scc-sum-reference (s)
  (let ((acc 0))
    (dotimes (i (length s) acc)
      (let ((c (schar s i)))
        (setq acc (+ acc (funcall #'char-code c)))))))

(defparameter *scc-ascii* (coerce "Az0 ~" 'simple-string))
;; Three CJK ideographs: non-ASCII, still one UTF-16 unit each. Built from their
;; codes rather than written literally, so the file stays ASCII.
(defparameter *scc-japanese*
  (coerce (list (code-char 26085) (code-char 26412) (code-char 35486))
          'simple-string))
;; A character above the BMP is two UTF-16 units in a string, so a scan meets
;; each surrogate half on its own: SCHAR answers the half, and its code is what
;; the typed entry has to produce.
(defparameter *scc-surrogates*
  (coerce (list (code-char #xD83D) (code-char #xDE00) #\!) 'simple-string))

(deftest string-char-code-typed.ascii
  (list (%scc-sum *scc-ascii*) (%scc-sum-reference *scc-ascii*))
  (393 393))

(deftest string-char-code-typed.japanese
  (list (= (%scc-sum *scc-japanese*) (%scc-sum-reference *scc-japanese*))
        (%scc-sum *scc-japanese*))
  (t 87983))

(deftest string-char-code-typed.surrogate-halves
  (list (= (%scc-sum *scc-surrogates*) (%scc-sum-reference *scc-surrogates*))
        (char-code (schar *scc-surrogates* 0))
        (char-code (schar *scc-surrogates* 1)))
  (t 55357 56832))

(deftest string-char-code-typed.char-spelling-agrees
  (list (%scc-sum-char *scc-japanese*) (%scc-sum-reference *scc-japanese*))
  (87983 87983))

(deftest string-char-code-typed.computed-index-agrees
  (list (%scc-sum-computed *scc-ascii*) (%scc-sum-reference *scc-ascii*))
  (393 393))

;;; ---- the shapes the fast path declines ----

;; An adjustable string with a fill pointer is not the simple string the typed
;; entry reads directly; it has to fall through to CHAR and answer the same.
(deftest string-char-code-typed.adjustable-string
  (let ((s (make-array 5 :element-type 'character :adjustable t :fill-pointer 3
                         :initial-contents '(#\a #\b #\c #\d #\e))))
    (list (char-code (char s 0)) (char-code (char s 2)) (char-code (char s 4))))
  (97 99 101))

;; An index past the end is still a Lisp error, not a .NET cast escaping.
(deftest string-char-code-typed.index-out-of-range
  (handler-case (char-code (schar *scc-ascii* 99)) (error () :error))
  :error)

(setf dotcl:*save-sil* nil)
