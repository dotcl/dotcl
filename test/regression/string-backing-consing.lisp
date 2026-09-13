;;; Reading a string must not make every later read of it allocate.
;;;
;;; LispString is copy-on-write over two backings: the System.String it was made
;;; from, or a char[] once something writes to it. RAWCHARS is the write
;;; accessor -- it materializes the char[] permanently -- and VALUE has to build
;;; a fresh System.String on every read once that has happened.
;;;
;;; Two things went wrong with that.
;;;
;;; 1. SEARCH's string fast path reached for RAWCHARS to scan. It only reads, but
;;;    the scan flipped the backing, and from then on every STRING= / STRING< on
;;;    that string allocated a fresh copy of it -- for the rest of the image's
;;;    life, and only for the strings SEARCH had happened to touch. REPLACE did
;;;    the same to its source. Bulk readers now take a read-only view instead.
;;;
;;; 2. The comparison entries went through the string designator, i.e. VALUE. So
;;;    a string that really had been written to -- (SETF (CHAR S I) ...),
;;;    NSTRING-UPCASE, FILL, REPLACE -- paid that copy on every comparison
;;;    forever after. They now compare the characters directly.
;;;
;;; Every expected value here was taken from SBCL.

(defparameter *sbc-searched* (copy-seq "hello world"))
(defparameter *sbc-written*  (copy-seq "hello world"))

(defun %sbc-cmp-searched (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (string= *sbc-searched* "hello world")))))

(defun %sbc-cmp-written (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (string= *sbc-written* "hello world")))))

;;; Put both strings into the state that used to be expensive.
(search "wor" *sbc-searched*)
(setf (char *sbc-written* 0) #\h)

;; 11 characters is a 48-byte System.String, so 30000 extra comparisons would
;; show 1.4 MB. Compiled-only, like the other consing assertions.
(deftest-compiled-only string-backing-consing.compare-after-search
  (< (bytes-per-op #'%sbc-cmp-searched) 1)
  t)

(deftest-compiled-only string-backing-consing.compare-after-write
  (< (bytes-per-op #'%sbc-cmp-written) 1)
  t)

;;; SEARCH answers the same on both backings, and leaves its argument readable.

(deftest string-backing-consing.search-values
  (list (search "wor" "hello world")
        (search "zzz" "hello world")
        (search "" "abc")
        (search "l" "hello world" :from-end t)
        (search "l" "hello world" :start2 4 :end2 11)
        (search '(2 3) '(1 2 3 4)))
  (6 nil 0 9 9 1))

(deftest string-backing-consing.search-on-written-string
  (let ((s (copy-seq "hello world")))
    (setf (char s 0) #\H)
    (list s (search "ello" s)))
  ("Hello world" 1))

(deftest string-backing-consing.string-survives-search
  (let ((s (copy-seq "hello")))
    (search "ell" s)
    (list s (length s) (char s 1) (string= s "hello")))
  ("hello" 5 #\e t))

;;; REPLACE reads its source and writes its target. The source must come out
;;; unchanged, and the two must still work when they are the same object and the
;;; ranges overlap in either direction -- the case a naive copy gets wrong.

(deftest string-backing-consing.replace-basic
  (list (let ((a (copy-seq "abcdef"))) (replace a "XY") a)
        (let ((a (copy-seq "abcdef"))) (replace a "XY" :start1 2) a)
        (let ((a (copy-seq "abcdef"))) (replace a "XYZ" :start1 1 :end1 3) a))
  ("XYcdef" "abXYef" "aXYdef"))

(deftest string-backing-consing.replace-same-object-overlapping
  (list (let ((a (copy-seq "abcdef"))) (replace a a :start1 0 :start2 2) a)
        (let ((a (copy-seq "abcdef"))) (replace a a :start1 2 :start2 0) a))
  ("cdefef" "ababcd"))

(deftest string-backing-consing.replace-leaves-source-alone
  (let ((a (copy-seq "abcdef")) (b (copy-seq "XYZ")))
    (replace a b :end2 2)
    (list a b))
  ("XYcdef" "XYZ"))

;;; The comparisons keep answering for every string designator, and on a written
;;; string, where the characters and not the cached System.String are the truth.

(deftest string-backing-consing.designators
  (list (string= 'abc "ABC") (string= #\a "a") (string= nil "NIL") (string= t "T"))
  (t t t t))

(deftest string-backing-consing.orderings
  (list (string/= "abc" "abd") (string< "abc" "abd") (string> "abd" "abc")
        (string-equal "AbC" "aBc") (string= "hello" "xhellox" :start2 1 :end2 6))
  (2 2 2 t t))

(deftest string-backing-consing.compare-written-string
  (let ((s (copy-seq "abc")))
    (nstring-upcase s)
    (list s (string= s "ABC") (string= s "abc")))
  ("ABC" t nil))
