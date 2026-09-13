;;; SUBSTITUTE on a string or vector costs only its result.
;;;
;;; SUBSTITUTE replaces elements; it never adds or drops one, so the answer has
;;; exactly the input's length and can be written straight into a destination of
;;; that size. The string and vector paths did not: they materialised every
;;; element into a List<LispObject>, built a second List of the result, and
;;; handed that to the coercion that turned it back into a string or vector.
;;; Three intermediates for a sequence whose size was known before the walk
;;; started -- the shape REMOVE was taken out of, which SUBSTITUTE still had.
;;; (The list path already wrote its answer directly.)
;;;
;;; :FROM-END keeps the general path: it changes which elements are replaced only
;;; together with :COUNT, and that needs the right-to-left marking pass.
;;;
;;; Values here are dotcl's own, taken before the change and checked against SBCL
;;; (which differs only in the two TYPE-OF spellings and in signalling a
;;; TYPE-ERROR where dotcl signals a PROGRAM-ERROR -- all three pre-existing).

(deftest substitute-consing.string-basic
  (list (substitute #\a #\z "hello")
        (substitute #\a #\h "hello")
        (substitute #\a #\l "hello")
        (substitute #\a #\l ""))
  ("hello" "aello" "heaao" ""))

(deftest substitute-consing.string-bounds-and-count
  (list (substitute #\a #\l "hello" :count 1)
        (substitute #\a #\l "hello" :count 1 :from-end t)
        (substitute #\a #\l "hello" :count 0)
        (substitute #\a #\l "hello" :start 3)
        (substitute #\a #\l "hello" :end 3)
        (substitute #\a #\l "hello" :start 1 :end 4))
  ("healo" "helao" "hello" "helao" "healo" "heaao"))

(deftest substitute-consing.string-test-and-key
  (list (substitute #\a #\H "hello" :test #'char-equal)
        (substitute #\a #\L "hello" :key #'char-upcase)
        (substitute-if #\a #'alpha-char-p "he1lo")
        (substitute-if-not #\a #'alpha-char-p "he1lo"))
  ("aello" "heaao" "aa1aa" "healo"))

(deftest substitute-consing.input-is-not-modified
  (let ((s (copy-seq "hello")))
    (list (substitute #\a #\h s) s))
  ("aello" "hello"))

(deftest substitute-consing.vector
  (mapcar (lambda (v) (coerce v (quote list)))
          (list (substitute 9 7 (vector 1 2 3))
                (substitute 9 2 (vector 1 2 3))
                (substitute 9 2 (vector 1 2 2) :count 1)
                (substitute 9 2 (vector 1 2 2) :count 1 :from-end t)))
  ((1 2 3) (1 9 3) (1 9 2) (1 2 9)))

;;; The result keeps the input's element type, and takes its length from the fill
;;; pointer -- both decided by the coercion before, so both are checks that the
;;; direct write reproduces it.
(deftest substitute-consing.vector-element-type
  (let ((v (make-array 3 :element-type 'bit :initial-element 0)))
    (list (coerce (substitute 1 0 v) 'list)
          (typep (substitute 1 0 v) 'bit-vector)))
  ((1 1 1) t))

(deftest substitute-consing.vector-fill-pointer
  (let ((v (make-array 5 :fill-pointer 3 :initial-element 1)))
    (list (coerce (substitute 9 1 v) (quote list)) (length (substitute 9 1 v))))
  ((9 9 9) 3))

;;; A non-character NEWITEM into a string is an error -- but only when something
;;; actually matched, which is where the coercion used to notice it.
(deftest substitute-consing.non-character-into-string
  (list (handler-case (progn (substitute 1 #\h "hello") :no-error) (error () :error))
        (substitute 1 #\z "hello"))
  (:error "hello"))

;;; The point of the change: a string result costs what COPY-SEQ of it costs.

(defparameter *subc-s* (copy-seq "hello"))

(defun %subc-substitute (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (substitute #\a #\z *subc-s*)))))

(defun %subc-copy (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (copy-seq *subc-s*)))))

;; Measured against COPY-SEQ of the same string rather than against a constant:
;; that is exactly the result both loops build, so what is left is the overhead
;; this change removes. It used to be 456 B against COPY-SEQ's 104.
;; Compiled-only, like the other consing assertions.
(deftest-compiled-only substitute-consing.costs-only-the-result
  (< (- (bytes-per-op #'%subc-substitute) (bytes-per-op #'%subc-copy)) 40)
  t)
