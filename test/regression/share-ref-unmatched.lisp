;;; #n# with no #n= is an error, not an internal object.
;;;
;;; The reader answered an unmatched #n# with the SharePlaceholder it uses while
;;; resolving forward references inside a #n= body. That object reached the
;;; caller, and it is not a Lisp value: it printed as the thing it stood for
;;; while failing CONSP and erroring on CAR.
;;;
;;; Two shapes leaked it. "#1#" alone came back as the placeholder itself. And
;;; "(#1# #1=(a))" -- a reference before its label in the same read -- left the
;;; placeholder in the enclosing list, because the patch pass walks the object
;;; just read and that list was still being built. That one looked like it
;;; worked: ((A) (A)), with the halves not EQ and the first not a cons.
;;;
;;; CLHS 2.4.8.16 allows #n# only where #n= has ALREADY labelled an object, so
;;; the reference is now an error where the label is unknown. A placeholder is
;;; still handed out while its own #n= is mid-read, which is what makes a
;;; self-referential object readable.
;;;
;;; Expected values are SBCL's.

(defmacro %sru-try (form)
  `(handler-case ,form (error () :error)))

(deftest share-ref-unmatched.signals
  (list (%sru-try (nth-value 0 (read-from-string "#1#")))
        (%sru-try (nth-value 0 (read-from-string "(a #1#)")))
        (%sru-try (nth-value 0 (read-from-string "(#1=(a) #2#)")))
        (%sru-try (nth-value 0 (read-from-string "(#1# #1=(a))"))))
  (:error :error :error :error))

;;; A label still in progress resolves: this is the case the placeholder exists
;;; for, and it must keep working.

(deftest share-ref-unmatched.self-reference-still-reads
  (list (let ((x (read-from-string "#1=(a . #1#)"))) (eq x (cdr x)))
        (let ((x (read-from-string "#1=#(a #1#)"))) (eq x (aref x 1)))
        (let ((x (read-from-string "#1=#(#1#)"))) (eq x (aref x 0)))
        (let ((x (read-from-string "#1=(#2=(a) #1# #2#)")))
          (list (eq x (nth 1 x)) (eq (nth 0 x) (nth 2 x)))))
  (t t t (t t)))

;;; Ordinary sharing, where the label is complete before the reference.

(deftest share-ref-unmatched.backward-reference-still-shares
  (list (let ((x (read-from-string "(#1=(a b) #1#)"))) (eq (first x) (second x)))
        (let ((x (read-from-string "(#1=(a) #2=(b) #1# #2#)")))
          (list (eq (nth 0 x) (nth 2 x)) (eq (nth 1 x) (nth 3 x)))))
  (t (t t)))

;;; The label scope is one outermost read, so the same label can be reused, and a
;;; reference cannot reach across.

(deftest share-ref-unmatched.labels-are-per-read
  (list (with-input-from-string (s "#1=(a) #1=(b)") (list (read s) (read s)))
        (progn (read-from-string "#1=(a)")
               (%sru-try (nth-value 0 (read-from-string "#1#")))))
  (((a) (b)) :error))

;;; Under *read-suppress* nothing is built, so nothing is checked.

(deftest share-ref-unmatched.read-suppress
  (nth-value 0 (read-from-string "#+(or) #1# 7"))
  7)
