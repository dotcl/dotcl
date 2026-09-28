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

;;; ---- both string backings ----
;;;
;;; A LispString holds either a System.String or a char[], never both, and a
;;; write is what converts the first into the second. The typed read asks which
;;; one is live and then reads its length and its element off that same object,
;;; so each backing has its own bound test and each needs its own coverage:
;;; getting one of the two wrong is invisible to every test above, which only
;;; ever sees strings that arrived by one route.
;;;
;;; MAKE-STRING produces the System.String backing and SETF SCHAR converts it,
;;; so the pair below is not a hope that the two routes differ -- it is the two
;;; routes.

(defun %scc-unwritten ()
  (make-string 4 :initial-element #\q))

(defun %scc-written ()
  (let ((s (make-string 4 :initial-element #\q)))
    (setf (schar s 1) #\Z)
    s))

(deftest string-char-code-typed.unwritten-backing
  (let ((s (%scc-unwritten)))
    (list (%scc-sum s) (%scc-sum-reference s) (char-code (schar s 3))))
  (452 452 113))

(deftest string-char-code-typed.written-backing
  (let ((s (%scc-written)))
    (list (%scc-sum s) (%scc-sum-reference s) (char-code (schar s 1))))
  (429 429 90))

;; The value spelling (the character object rather than its code) reads through
;; the same two-backing selection and has to agree on both.
(deftest string-char-code-typed.value-spelling-both-backings
  (list (schar (%scc-unwritten) 0) (schar (%scc-written) 1))
  (#\q #\Z))

;; Out of range on each backing separately: the bound is now read off whichever
;; object is live, so a backing whose check was dropped would read past the end
;; or throw a .NET exception instead of signalling.
(deftest string-char-code-typed.out-of-range-unwritten
  (let ((s (%scc-unwritten)))
    (list (handler-case (char-code (schar s 4)) (error () :error))
          (handler-case (char-code (schar s -1)) (error () :error))
          (handler-case (schar s 9) (error () :error))))
  (:error :error :error))

(deftest string-char-code-typed.out-of-range-written
  (let ((s (%scc-written)))
    (list (handler-case (char-code (schar s 4)) (error () :error))
          (handler-case (char-code (schar s -1)) (error () :error))
          (handler-case (schar s 9) (error () :error))))
  (:error :error :error))

;;; ---- the LispVector representation of a string ----
;;;
;;; A string has two runtime representations. MAKE-STRING and the reader build
;;; one; MAKE-ARRAY with :element-type CHARACTER builds the other, a vector
;;; whose character storage is a separate backing kind. They answer the same
;;; SIMPLE-STRING-P and the same TYPE-OF and both satisfy a (SIMPLE-STRING x)
;;; declaration, so no caller can tell them apart -- and the typed read used to
;;; recognise only the first, leaving the second on the boxed-index path at
;;; about fourteen times the cost per character.
;;;
;;; The tests below are one per route a string can arrive by, because a gate
;;; written against one representation is exactly the bug this fixes: it was
;;; invisible for as long as every test built its strings the same way.

(defun %scv-sum (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0) (n (length s)))
    (declare (fixnum acc n))
    (dotimes (i n acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (char-code (schar s i))))))))

(defun %scv-vector (n ch)
  (make-array n :element-type 'character :initial-element ch))

;; The vector representation reads the same values as the reference path.
(deftest string-char-code-typed.vector-backing
  (let ((s (%scv-vector 4 #\q)))
    (setf (schar s 1) #\Z)
    (list (%scv-sum s) (%scc-sum-reference s) (schar s 1) (schar s 3)))
  (429 429 #\Z #\q))

;; base-char is the same backing kind and must behave identically.
(deftest string-char-code-typed.vector-base-char
  (let ((s (make-array 3 :element-type 'base-char :initial-element #\a)))
    (list (%scv-sum s) (%scc-sum-reference s)))
  (291 291))

;; Non-ASCII and surrogate halves through the vector representation, matching
;; the coverage the other representation already has.
(deftest string-char-code-typed.vector-non-ascii
  (let ((s (%scv-vector 3 #\a)))
    (setf (schar s 0) (code-char 26085))
    (setf (schar s 1) (code-char #xD83D))
    (setf (schar s 2) (code-char #xDE00))
    (list (= (%scv-sum s) (%scc-sum-reference s))
          (char-code (schar s 1)) (char-code (schar s 2))))
  (t 55357 56832))

;; An adjustable character vector is the shape VECTOR-PUSH-EXTEND builds. It is
;; not a SIMPLE-STRING, but SIMPLE-STRING-P used to accept it, so declared
;; readers written against that still meet it and must read it correctly.
(deftest string-char-code-typed.vector-adjustable
  (let ((s (make-array 3 :element-type 'character :adjustable t
                         :initial-element #\b)))
    (list (%scv-sum s) (%scc-sum-reference s)))
  (294 294))

;; A displaced vector's elements are not its own: it must keep reading through
;; the displacement, which means staying off the direct-buffer path.
(deftest string-char-code-typed.vector-displaced
  (let* ((base (%scv-vector 6 #\y))
         (d (make-array 3 :element-type 'character :displaced-to base
                          :displaced-index-offset 2)))
    (setf (schar base 2) #\A)
    (list (schar d 0) (%scv-sum d) (%scc-sum-reference d)))
  (#\A 307 307))

;; A fill pointer does not make the vector unreadable by SCHAR here, and the
;; value must not change with the fast path.
(deftest string-char-code-typed.vector-fill-pointer
  (let ((s (make-array 4 :element-type 'character :fill-pointer 2
                         :initial-element #\c)))
    (list (schar s 0) (schar s 3) (length s)))
  (#\c #\c 2))

;; Out of range on the vector representation signals, as it does on the other.
(deftest string-char-code-typed.vector-out-of-range
  (let ((s (%scv-vector 4 #\q)))
    (list (handler-case (char-code (schar s 4)) (error () :error))
          (handler-case (char-code (schar s -1)) (error () :error))
          (handler-case (schar s 99) (error () :error))))
  (:error :error :error))

;; Both representations answer the same thing for the same content, which is
;; the property that makes the fast path safe to pick between them.
(deftest string-char-code-typed.representations-agree
  (let ((a (make-string 5 :initial-element #\m))
        (b (%scv-vector 5 #\m)))
    (list (= (%scv-sum a) (%scv-sum b))
          (equal (coerce a 'list) (coerce b 'list))
          (string= a b)))
  (t t t))

;;; ---- the hoisted char[] buffer ----
;;;
;;; A binding declared SIMPLE-STRING can have its char[] storage fetched once
;;; and read with a bare ldelem for the rest of the binding. What makes that
;;; sound is not the declaration -- adjustable and displaced character vectors
;;; still arrive under it in code written when SIMPLE-STRING-P accepted them,
;;; and a plain (MAKE-ARRAY n :element-type
;;; 'CHARACTER) is simple under CLHS while being a LispVector rather than a
;;; LispString. It is that LispString's _chars is write-once: assigned in the
;;; char[] constructor and in EnsureMutable under a null guard, and ADJUST-ARRAY
;;; cannot reach a LispString at all. So the array cannot be swapped underneath
;;; the binding, and a write through SCHAR mutates that same array in place
;;; where the hoisted reference sees it.
;;;
;;; Runtime.BackingChars answers NIL for everything it cannot pin, and every
;;; access carries the typed call as its other arm, so declining costs the speed
;;; and never the answer.

(defun %sbh-sum (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0) (n (length s)))
    (declare (fixnum acc n))
    (dotimes (i n acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (char-code (schar s i))))))))

;; The three ways a string can arrive, all satisfying the same declaration and
;; answering the same TYPE-OF. Only the first can be hoisted; the other two must
;; come back with identical values off the other arm.
(defun %sbh-written (n ch)
  (let ((s (make-string n :initial-element #\a)))
    (dotimes (i n s) (setf (schar s i) ch))))
(defun %sbh-unwritten (n ch) (make-string n :initial-element ch))
(defun %sbh-vector (n ch)
  (make-array n :element-type 'character :initial-element ch))

(deftest string-char-code-typed.hoist-agrees-across-representations
  (list (%sbh-sum (%sbh-written 4 #\q))
        (%sbh-sum (%sbh-unwritten 4 #\q))
        (%sbh-sum (%sbh-vector 4 #\q)))
  (452 452 452))

;; The buffer is fetched once and the bare load is what the body uses.
(deftest-emitting-only string-char-code-typed.hoist-is-emitted
  (let ((d (princ-to-string (dotcl:function-sil #'%sbh-sum))))
    (list (and (search "Runtime.BackingChars" d) t)
          (and (search "LDELEM-U2" d) t)
          ;; and the other arm is still there, for the strings it declines
          (and (search "CharCodeAtL" d) t)))
  (t t t))

;; A write followed by a read in the same binding must see the write. This
;; function reads outside any loop, so the backing hoist does not apply to it
;; and it checks values only; the hoisted version of the same aliasing check is
;; string-backing-hoist-gate.loop-write-is-visible.
(defun %sbh-write-then-read (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((before (schar s 1)))
    (setf (schar s 1) #\Z)
    (list before (schar s 1) (char-code (schar s 1)))))

(deftest string-char-code-typed.write-is-visible
  (list (%sbh-write-then-read (%sbh-written 4 #\q))
        (%sbh-write-then-read (%sbh-unwritten 4 #\q)))
  ((#\q #\Z 90) (#\q #\Z 90)))

;; A string that is not char[]-backed at binding time, written during the body:
;; the write materializes the backing, but the hoisted slot is NIL for it, so
;; every read takes the other arm and still answers correctly.
(deftest string-char-code-typed.unwritten-then-written-is-correct
  (let ((s (%sbh-unwritten 3 #\a)))
    (list (%sbh-write-then-read s) (%sbh-sum s)))
  ((#\a #\Z 90) 284))

;; The CHAR spelling takes the same path, and the value position (a character
;; object rather than its code) has to agree too.
(defun %sbh-char-spelling (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0) (n (length s)))
    (declare (fixnum acc n))
    (dotimes (i n (list acc (char s 0) (schar s 1)))
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (char-code (char s i))))))))

(deftest string-char-code-typed.char-spelling-and-value-position
  (list (%sbh-char-spelling (%sbh-written 3 #\m))
        (%sbh-char-spelling (%sbh-vector 3 #\m)))
  ((327 #\m #\m) (327 #\m #\m)))

;; Non-ASCII and surrogate halves through the hoisted read, matching the
;; coverage the typed call already has.
(deftest string-char-code-typed.hoist-non-ascii
  (let ((s (make-string 3 :initial-element #\a)))
    (setf (schar s 0) (code-char 26085))
    (setf (schar s 1) (code-char #xD83D))
    (setf (schar s 2) (code-char #xDE00))
    (list (%sbh-sum s) (char-code (schar s 1)) (char-code (schar s 2))))
  (138274 55357 56832))

;; Out of range still signals on a hoisted binding.
(deftest string-char-code-typed.hoist-out-of-range
  (let ((s (%sbh-written 4 #\q)))
    (list (handler-case (char-code (schar s 4)) (error () :error))
          (handler-case (char-code (schar s -1)) (error () :error))))
  (:error :error))

;; A local shadow of SCHAR is not this composition at all, so the hoist must not
;; be what runs.
(defun %sbh-shadowed (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (flet ((schar (str i) (declare (ignore str i)) #\Z))
    (char-code (schar s 0))))

(deftest string-char-code-typed.hoist-respects-local-shadow
  (%sbh-shadowed (%sbh-written 2 #\q))
  90)

(setf dotcl:*save-sil* nil)
