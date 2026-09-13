;;; MISMATCH compares without copying either sequence.
;;;
;;; It used to collect both sequences into LispObject[] before comparing
;;; anything -- two arrays for a walk that reads each element once and usually
;;; stops at the first difference. That was 48 + 16n bytes for a call whose
;;; answer is an index or NIL, and it scaled with the inputs: 168 B for two
;;; 5-character strings, 264 B for two 11-character ones.
;;;
;;; Forward, each element is read once and in order, so a cursor is enough: an
;;; index for a string or vector, a cons pointer for a list. :FROM-END reads from
;;; the right, which a list cannot do, so that direction keeps the copy.
;;;
;;; The two-argument call also built an argument array just so the keyword parser
;;; could look at the two sequences; it now has a direct entry, the same one
;;; SEARCH already had.
;;;
;;; The bounding-index checks MISMATCH was missing when this file was written
;;; now live in MISMATCH-BOUNDING-INDICES; this file predates that fix and no
;;; longer describes a deviation.
;;;
;;; Values here are dotcl's own, and agree with SBCL.

(deftest mismatch-consing.strings
  (list (mismatch "hello" "hello")
        (mismatch "hello" "helXo")
        (mismatch "hello" "Xello")
        (mismatch "hel" "hello")
        (mismatch "hello" "hel")
        (mismatch "" "")
        (mismatch "" "a")
        (mismatch "a" ""))
  (nil 3 0 3 3 nil 0 0))

(deftest mismatch-consing.lists-and-vectors
  (list (mismatch '(1 2 3) '(1 2 3))
        (mismatch '(1 2 3) '(1 9 3))
        (mismatch '(1 2) '(1 2 3))
        (mismatch #(1 2 3) #(1 2 3))
        (mismatch #(1 2 3) #(1 9 3))
        (mismatch nil nil)
        (mismatch nil '(1)))
  (nil 1 2 nil 1 nil 0))

;;; Mixed sequence types go through the same cursors.
(deftest mismatch-consing.mixed-types
  (list (mismatch '(1 2 3) #(1 2 3))
        (mismatch "abc" '(#\a #\b #\c)))
  (nil nil))

(deftest mismatch-consing.bounding-indices
  (list (mismatch "xhello" "hello" :start1 1)
        (mismatch "hello" "xhello" :start2 1)
        (mismatch "helloZ" "hello" :end1 5)
        (mismatch "hello" "helloZ" :end2 5)
        (mismatch "xxhelloyy" "zhellow" :start1 2 :end1 7 :start2 1 :end2 6))
  (nil nil nil nil nil))

;;; :FROM-END was moved into the new core rather than rewritten, so one case pins
;;; that the split carried it across; the oracle diff settled the rest.
(deftest mismatch-consing.from-end
  (mismatch "hello" "hellX" :from-end t)
  5)

(deftest mismatch-consing.test-and-key
  (list (mismatch "HELLO" "hello" :test #'char-equal)
        (mismatch "abc" "abc" :test-not #'char/=)
        (mismatch "HELLO" "hello" :key #'char-upcase)
        (mismatch '(1 2 3) '(2 3 4) :key #'evenp))
  (nil nil nil 0))

;;; The type check still fires, and on the first argument first.
(deftest mismatch-consing.errors
  (list (handler-case (progn (mismatch 5 "hi") :no-error) (error () :error))
        (handler-case (progn (mismatch "a" "a" :zz 1) :no-error) (error () :error)))
  (:error :error))

;;; The point of the change.

(defparameter *mmc-s1* (copy-seq "hello world"))
(defparameter *mmc-s2* (copy-seq "hello world"))
(defparameter *mmc-l1* (list 1 2 3))
(defparameter *mmc-l2* (list 1 2 3))

(defun %mmc-strings (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (mismatch *mmc-s1* *mmc-s2*)))))

(defun %mmc-lists (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (mismatch *mmc-l1* *mmc-l2*)))))

;; The answer is NIL, so nothing is left to allocate. It was 264 B per call for
;; the strings. Compiled-only, like the other consing assertions.
(deftest-compiled-only mismatch-consing.strings-allocate-nothing
  (< (bytes-per-op #'%mmc-strings) 1)
  t)

(deftest-compiled-only mismatch-consing.lists-allocate-nothing
  (< (bytes-per-op #'%mmc-lists) 1)
  t)
