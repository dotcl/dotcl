;;; The char[] buffer of a SIMPLE-STRING binding is fetched only when the
;;; binding is read from inside a loop.
;;;
;;; The fetch runs where the variable is bound, every time it is bound, so it
;;; only pays for itself when the reads it serves happen more than once per
;;; binding. A function that reads its string once -- the common parser shape,
;;; SUBSEQ a fresh string and look at its first character -- used to pay the
;;; fetch plus a two-armed read for a single character.
;;;
;;; The integer array kinds are deliberately NOT gated this way: their fetch is
;;; also where a false (SIMPLE-ARRAY ...) declaration gets its TYPE-ERROR, and
;;; array-backing-hoist.lisp pins that a straight-line AREF still fetches. The
;;; char fetch never signals, so declining it loses speed and nothing else.
;;;
;;; The value tests run every representation a SIMPLE-STRING declaration can
;;; receive, because both arms of the read have to agree on all of them.

(setf dotcl:*save-sil* t)

(defun %sbg-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %sbg-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- shapes ----

;; One read, no loop.
(defun %sbg-once (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (char-code (schar s 0)))

;; One read in arithmetic context, which takes the typed code read.
(defun %sbg-once-arith (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (1- (char-code (schar s i))))

;; The parser shape: a fresh string bound by LET, read once.
(defun %sbg-subseq-once (line)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((s (subseq line 1)))
    (declare (simple-string s))
    (if (char= (schar s 0) #\#) :comment :other)))

;; Reads inside a loop.
(defun %sbg-loop (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0) (n (length s)))
    (declare (fixnum acc n))
    (dotimes (i n acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (char-code (schar s i))))))))

;; A loop read plus a read outside the loop: the binding is hoisted, so the
;; outside read uses the fetched buffer too.
(defun %sbg-loop-mixed (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc (char-code (schar s 0))) (n (length s)))
    (declare (fixnum acc n))
    (dotimes (i n acc)
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (char-code (schar s i))))))))

;; The loop is in an inner LET's scope, not the binding's: the fetch would run
;; once per iteration, so the inner binding must not be hoisted.
(defun %sbg-bound-per-iteration (lines)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((k 0))
    (dolist (line lines k)
      (let ((s (subseq line 0)))
        (declare (simple-string s))
        (when (char= (schar s 0) #\#) (setq k (1+ k)))))))

;; A write and a later read in the same hoisted binding, inside a loop: the
;; write goes through the ordinary store and mutates the array the hoist holds,
;; so the read off the hoisted buffer must see it. This is the aliasing the
;; hoist relies on, taken where the hoist now happens.
(defun %sbg-loop-write-then-read (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0) (n (length s)))
    (declare (fixnum acc n))
    (dotimes (i n acc)
      (declare (fixnum i))
      (setf (schar s i) #\Z)
      (setq acc (the fixnum (+ acc (char-code (schar s i))))))))

;;; ---- representations ----

(defun %sbg-written (n ch)
  (let ((s (make-string n :initial-element #\a)))
    (dotimes (i n s) (setf (schar s i) ch))))
(defun %sbg-unwritten (n ch) (make-string n :initial-element ch))
(defun %sbg-vector (n ch)
  (make-array n :element-type 'character :initial-element ch))

;;; ---- SIL shape ----
;;;
;;; DEFTEST-EMITTING-ONLY: an emit-free build stores no SIL, so a count of 0
;;; would pass for the wrong reason.

(deftest-emitting-only string-backing-hoist-gate.single-read-not-hoisted
  (list (let ((d (%sbg-sil #'%sbg-once)))
          (list (%sbg-count "Runtime.BackingChars" d) (%sbg-count "(LDELEM-U2)" d)
                (%sbg-count "Runtime.CharAtL" d)))
        (let ((d (%sbg-sil #'%sbg-once-arith)))
          (list (%sbg-count "Runtime.BackingChars" d) (%sbg-count "(LDELEM-U2)" d)
                (%sbg-count "Runtime.CharCodeAtL" d)))
        (let ((d (%sbg-sil #'%sbg-subseq-once)))
          (list (%sbg-count "Runtime.BackingChars" d) (%sbg-count "(LDELEM-U2)" d))))
  ((0 0 1) (0 0 1) (0 0)))

(deftest-emitting-only string-backing-hoist-gate.loop-read-hoisted
  (list (let ((d (%sbg-sil #'%sbg-loop)))
          (list (%sbg-count "Runtime.BackingChars" d) (%sbg-count "(LDELEM-U2)" d)))
        (let ((d (%sbg-sil #'%sbg-loop-mixed)))
          (list (%sbg-count "Runtime.BackingChars" d) (%sbg-count "(LDELEM-U2)" d)))
        (let ((d (%sbg-sil #'%sbg-loop-write-then-read)))
          (list (%sbg-count "Runtime.BackingChars" d) (%sbg-count "(LDELEM-U2)" d))))
  ((1 1) (1 2) (1 1)))

(deftest-emitting-only string-backing-hoist-gate.per-iteration-binding-not-hoisted
  (%sbg-count "Runtime.BackingChars" (%sbg-sil #'%sbg-bound-per-iteration))
  0)

;;; ---- values ----

(deftest string-backing-hoist-gate.single-read-values
  (list (%sbg-once (%sbg-written 3 #\q))
        (%sbg-once (%sbg-unwritten 3 #\q))
        (%sbg-once (%sbg-vector 3 #\q))
        (%sbg-once-arith (%sbg-written 3 #\q) 2)
        (%sbg-once-arith (%sbg-vector 3 (code-char 26085)) 1))
  (113 113 113 112 26084))

(deftest string-backing-hoist-gate.subseq-once-values
  (list (%sbg-subseq-once "x#rest") (%sbg-subseq-once "xyrest")
        (%sbg-bound-per-iteration '("#a" "b" "#c")))
  (:comment :other 2))

(deftest string-backing-hoist-gate.loop-values
  (list (%sbg-loop (%sbg-written 4 #\q))
        (%sbg-loop (%sbg-unwritten 4 #\q))
        (%sbg-loop (%sbg-vector 4 #\q))
        (%sbg-loop-mixed (%sbg-written 4 #\q))
        (%sbg-loop-mixed (%sbg-vector 4 #\q)))
  (452 452 452 565 565))

(deftest string-backing-hoist-gate.loop-write-is-visible
  (list (let ((s (%sbg-written 3 #\q))) (list (%sbg-loop-write-then-read s) s))
        (let ((s (%sbg-unwritten 3 #\q))) (list (%sbg-loop-write-then-read s) s))
        (let ((s (%sbg-vector 3 #\q))) (list (%sbg-loop-write-then-read s) s)))
  ((270 "ZZZ") (270 "ZZZ") (270 "ZZZ")))

;;; ---- which representations the fetch takes ----
;;;
;;; The SIL above is the same for every string; which arm runs is decided at
;;; binding time by the fetch. It takes the char[] of a LispString and of a
;;; simple rank-1 character vector (what MAKE-ARRAY :ELEMENT-TYPE 'CHARACTER
;;; builds), and declines a LispString still holding a System.String and every
;;; character array whose storage could be swapped or is not its own. The
;;; declining cases are the ones the per-element arm must keep handling, and
;;; the checked fetch (above (safety 0)) must still reject the non-simple ones.

(defun %sbg-representations ()
  (let ((base (make-array 6 :element-type 'character :initial-element #\z)))
    (list (%sbg-written 3 #\q)
          (%sbg-unwritten 3 #\q)
          (%sbg-vector 3 #\q)
          (make-array '(2 2) :element-type 'character :initial-element #\q)
          (make-array 3 :element-type 'character :adjustable t :initial-element #\q)
          (make-array 3 :element-type 'character :displaced-to base)
          (make-array 3 :element-type 'character :fill-pointer 3 :initial-element #\q)
          42)))

(deftest string-backing-hoist-gate.fetch-by-representation
  (mapcar (lambda (o) (not (null (dotnet:static "DotCL.Runtime" "BackingChars" o))))
          (%sbg-representations))
  (t nil t nil nil nil nil nil))

(deftest string-backing-hoist-gate.checked-fetch-by-representation
  (mapcar (lambda (o)
            (handler-case
                (not (null (dotnet:static "DotCL.Runtime" "BackingCharsChecked" o)))
              (type-error () :type-error)))
          (%sbg-representations))
  (t nil t nil :type-error :type-error :type-error nil))

;; A simple character vector now runs the hoisted arm: its reads, a non-ASCII
;; code included, and a write made through the vector between two calls.
(deftest string-backing-hoist-gate.vector-hoisted-values
  (let ((v (%sbg-vector 3 #\q)))
    (setf (char v 1) (code-char 26085))
    (let ((a (%sbg-loop v)))
      (setf (aref v 1) #\a)
      (list a (%sbg-loop v) (%sbg-loop-mixed v))))
  (26311 323 436))

(setf dotcl:*save-sil* nil)
