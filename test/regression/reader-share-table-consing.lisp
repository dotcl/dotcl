;;; A READ that uses no #n= / #n# does not build the tables for them.
;;;
;;; Reader held two Dictionary fields for the share labels and initialised both
;;; eagerly. An empty Dictionary is about 80 bytes, so every Reader paid 160 for a
;;; feature almost no input uses -- and READ-FROM-STRING builds a fresh Reader per
;;; call, as does the FASL load path when it reconstructs a shared constant.
;;; (read-from-string "1"), whose answer is a cached Fixnum, cost 584 B.
;;;
;;; The tables are now built on first use. The risk is the feature itself, so most
;;; of this file is #n= / #n#: expected values are SBCL's, and dotcl agrees on all
;;; of them except one pre-existing difference noted at the end.

;;; Sharing and circularity survive the change.

(deftest reader-share-table-consing.shared-sublist
  (let ((x (read-from-string "(#1=(a b) #1#)")))
    (list (equal (first x) (second x)) (eq (first x) (second x))))
  (t t))

(deftest reader-share-table-consing.circular
  (list (let ((x (read-from-string "#1=(a . #1#)")))
          (list (car x) (eq x (cdr x))))
        (let ((x (read-from-string "#1=#(a #1#)")))
          (eq x (aref x 1)))
        (let ((x (read-from-string "#1=#(#1#)")))
          (eq x (aref x 0))))
  ((a t) t t))

(deftest reader-share-table-consing.several-and-nested-labels
  (list (let ((x (read-from-string "(#1=(a) #2=(b) #1# #2#)")))
          (list (eq (nth 0 x) (nth 2 x)) (eq (nth 1 x) (nth 3 x)) (eq (nth 0 x) (nth 1 x))))
        (let ((x (read-from-string "#1=(#2=(a) #1# #2#)")))
          (list (eq x (nth 1 x)) (eq (nth 0 x) (nth 2 x)))))
  ((t t nil) (t t)))

;;; Labels belong to one outermost read (CLHS 2.4.8.15), so a second read starts
;;; with a fresh table -- the same table the lazy creation has to keep clearing.

(deftest reader-share-table-consing.labels-do-not-leak-between-reads
  (with-input-from-string (s "#1=(a) #1=(b)")
    (list (read s) (read s)))
  ((a) (b)))

(deftest reader-share-table-consing.plain-read-after-a-share-read
  (progn (read-from-string "#1=(a)")
         (nth-value 0 (read-from-string "(1 2 3)")))
  (1 2 3))

;;; READ on a stream takes the same path (it adopts the stream's tables, which
;;; must exist by then).

(deftest reader-share-table-consing.share-via-read-on-a-stream
  (with-input-from-string (s "#1=(a #1#)")
    (let ((x (read s))) (eq x (second x))))
  t)

;;; A circular object built by the reader still prints as one.

;;; The point of the change.

(defparameter *rstc-src* "1")

(defun %rstc-read (n)
  (declare (fixnum n))
  (let ((r nil))
    (do ((i 0 (1+ i))) ((= i n) r)
      (declare (fixnum i))
      (setq r (read-from-string *rstc-src*)))))

;; "1" reads to a cached Fixnum, so everything measured here is the machinery.
;; The bound sits between what it costs now (424 B) and what it cost before (584).
;; Compiled-only, like the other consing assertions.
(deftest-compiled-only reader-share-table-consing.read-from-string-is-cheaper
  (< (bytes-per-op #'%rstc-read) 500)
  t)
