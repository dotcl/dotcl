;;; (CHAR-CODE (SCHAR S I)) carries its range into EXPR-INT-RANGE, not only
;;; into FIXNUM-TYPED-P.
;;;
;;; The third of the same omission, and the one with the largest effect per
;;; site, because what was unreachable was not an optimisation of the
;;; arithmetic but the character read itself. COMPILE-AS-LONG has had a
;;; Runtime.CharCodeAtL lowering for this composition for some time -- one call
;;; that answers the code directly -- but COMPILE-AS-LONG is only entered where
;;; a range proves, and FIXNUM-LEAF-RANGE had no clause, so for undeclared code
;;; the lowering could not be reached at all. A bare character scan went
;;; Runtime.CharAtL, Runtime.UnwrapMv, Runtime.CharCode: build the character
;;; object, unwrap it, take its code.
;;;
;;; The bound is (0 . 65535) and it is entirely an argument from the type.
;;; Every return of Runtime.CharCodeAtL is a C# char widened to long -- the two
;;; string backings, a LispVector's char[], and Runtime.CharCode of a LispChar
;;; whose Value is declared char -- and System.Char is 16 bits unsigned. Unlike
;;; the LENGTH bound, no step here holds only because of what fits in memory.
;;;
;;; WRITE THESE BARE. (THE FIXNUM ...) hands back the full int64 range, which
;;; is what does NOT prove 1-, so the declared spelling stays generic and is
;;; kept below as the control.

(setf dotcl:*save-sil* t)

(defun %flrc-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %flrc-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the shapes ----

(defun %flrc-dec (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (1- (char-code (schar s i))))

(defun %flrc-add (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (+ 1 (char-code (schar s i))))

;; The CHAR spelling reaches the same composition.
(defun %flrc-char-spelling (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (1- (char-code (char s i))))

;; The control: the declaration makes it WORSE, as with LENGTH.
(defun %flrc-dec-declared (s i)
  (declare (simple-string s) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (1- (the fixnum (char-code (schar s i)))))

;; A sum over a scan, which is the shape this is actually for.
(defun %flrc-scan (s)
  (declare (simple-string s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0))
    (dotimes (i (length s) acc)
      (setq acc (+ acc (char-code (schar s i)))))))

;;; ---- values ----

(deftest fixnum-leaf-range-char-code.dec
  (list (%flrc-dec "A" 0) (%flrc-dec "abc" 2))
  (64 98))

(deftest fixnum-leaf-range-char-code.add
  (list (%flrc-add "A" 0) (%flrc-add "abc" 2))
  (66 100))

(deftest fixnum-leaf-range-char-code.char-spelling
  (%flrc-char-spelling "A" 0)
  64)

(deftest fixnum-leaf-range-char-code.declared-form-still-answers
  (%flrc-dec-declared "A" 0)
  64)

(deftest fixnum-leaf-range-char-code.scan
  (list (%flrc-scan "AB") (%flrc-scan ""))
  (131 0))

;; The top of the declared range. A character at CHAR-CODE-LIMIT-1 is what the
;; bound claims is possible, so the arithmetic on it is checked rather than
;; assumed -- the range proof is what licenses the raw path, and if the bound
;; were wrong this is where it would show.
(deftest fixnum-leaf-range-char-code.at-the-upper-bound
  (let ((s (make-string 1 :initial-element (code-char 65535))))
    (list (char-code (schar s 0)) (%flrc-dec s 0) (%flrc-add s 0)))
  (65535 65534 65536))

;; A character above ASCII, to be sure nothing is narrowing to a byte.
(deftest fixnum-leaf-range-char-code.non-ascii
  (let ((s (make-string 1 :initial-element (code-char 12354))))
    (list (%flrc-dec s 0) (%flrc-add s 0)))
  (12353 12355))

;;; ---- emitted code ----
;;;
;;; Needles end at the closing paren: Runtime.CharCode is a prefix of
;;; Runtime.CharCodeAtL, and PRINC-TO-STRING prints neither the operand's
;;; quotes nor a keyword's colon, so (:SUB) appears as (SUB). Each test names
;;; what should be gone and what should have replaced it.

;; The whole three-call composition collapses to the one typed entry, and the
;; subtract is inline. This is the reachability point: the CharCodeAtL lowering
;; existed before and nothing undeclared could get to it.
(deftest-emitting-only fixnum-leaf-range-char-code.read-collapses-to-one-call
  (let ((s (%flrc-sil #'%flrc-dec)))
    (list (%flrc-count "Runtime.CharCodeAtL)" s)
          (%flrc-count "Runtime.CharAtL)" s)
          (%flrc-count "Runtime.CharCode)" s)
          (%flrc-count "Runtime.Decrement)" s)
          (%flrc-count "(SUB)" s)))
  (1 0 0 0 1))

(deftest-emitting-only fixnum-leaf-range-char-code.add-is-fixnum-typed
  (let ((s (%flrc-sil #'%flrc-add)))
    (list (%flrc-count "Runtime.CharCodeAtL)" s)
          (%flrc-count "Runtime.AddFixnum)" s)
          (%flrc-count "Runtime.Increment)" s)
          (%flrc-count "Runtime.CharAtL)" s)))
  (1 1 0 0))

(deftest-emitting-only fixnum-leaf-range-char-code.char-spelling-too
  (let ((s (%flrc-sil #'%flrc-char-spelling)))
    (list (%flrc-count "Runtime.CharCodeAtL)" s)
          (%flrc-count "Runtime.CharAtL)" s)))
  (1 0))

;; The control, and it goes the other way: THE gives the full int64 range, 1-
;; widens it out of int64, the proof fails, and the old three-call read comes
;; back with it. A dead needle would fail here rather than passing silently
;; above.
(deftest-emitting-only fixnum-leaf-range-char-code.declared-form-stays-generic
  (let ((s (%flrc-sil #'%flrc-dec-declared)))
    (list (plusp (%flrc-count "Runtime.Decrement)" s))
          (plusp (%flrc-count "Runtime.CharCode)" s))))
  (t t))

(setf dotcl:*save-sil* nil)
