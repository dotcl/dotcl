;;; A reader macro that produces a value must say it produced one.
;;;
;;; GET-MACRO-CHARACTER hands back a Lisp function wrapping the built-in reader
;;; macro, which is how a library builds a readtable out of the standard one --
;;; named-readtables' (:merge :standard) does exactly this for every macro
;;; character. Those wrappers share the multiple-values channel: the ones that
;;; read something producing no value at all (a ";" comment) report it by
;;; publishing ZERO values.
;;;
;;; Publishing zero values is a global state, and it was never taken back. The
;;; next wrapper to return a real value returned it while the count still said
;;; "no values", so a caller that reads the count to decide whether anything was
;;; produced threw the value away -- whenever the value happened to be NIL.
;;; "(" returns NIL for the empty list, so in a readtable built this way EVERY
;;; () after a comment silently vanished from the form:
;;;
;;;   ;; comment
;;;   (defvar v (lambda () (error "x")))   read as (DEFVAR V (LAMBDA (ERROR "x")))
;;;
;;; which handed the compiler a LAMBDA whose lambda list was the body. Any
;;; library using named-readtables hit it; it is what stopped varjo (and cepl
;;; behind it) from loading.

(defun %mcr-readtable (chars)
  "A copy of the standard readtable with CHARS re-installed through the
   GET-MACRO-CHARACTER / SET-MACRO-CHARACTER round trip, the way a readtable
   merged from the standard one is built."
  (let ((rt (copy-readtable nil)))
    (dolist (c chars rt)
      (multiple-value-bind (fn non-terminating-p) (get-macro-character c nil)
        (set-macro-character c fn non-terminating-p rt)))))

(defun %mcr-read (chars string)
  (let ((*readtable* (%mcr-readtable chars)))
    (values (read-from-string string))))

;;; ---- the empty list survives ----

;; The reproducer. Both ( and ; have to be round-tripped: with only one of them
;; the other is still the built-in reader and never consults the count.
(deftest macro-char-roundtrip-values.empty-list-after-comment
  (%mcr-read '(#\( #\;) (format nil ";; comment~%(a () b)"))
  (a nil b))

(deftest macro-char-roundtrip-values.several-empty-lists
  (%mcr-read '(#\( #\;) (format nil ";; comment~%(a () b () c)"))
  (a nil b nil c))

;; The shape that made it a compiler failure rather than a reader curiosity.
(deftest macro-char-roundtrip-values.lambda-list-survives
  (%mcr-read '(#\( #\;) (format nil ";; comment~%(defvar v~%  (lambda ()~%    (error \"x\")))"))
  (defvar v (lambda nil (error "x"))))

;; A comment between two forms, and a comment at the end of a line.
(deftest macro-char-roundtrip-values.comment-positions
  (list (%mcr-read '(#\( #\;) (format nil "(a) ;; trailing~%"))
        (%mcr-read '(#\( #\;) (format nil ";; one~%;; two~%(())")))
  ((a) (nil)))

;; Round-tripping the whole standard set, which is what a merged readtable does.
(deftest macro-char-roundtrip-values.full-merge
  (%mcr-read '(#\( #\) #\; #\' #\" #\` #\,)
             (format nil ";; comment~%(a () \"s\" 'q)"))
  (a nil "s" (quote q)))

;;; ---- shapes that were already right, pinned ----

;; No comment: nothing publishes zero values, so this always worked.
(deftest macro-char-roundtrip-values.no-comment-unchanged
  (%mcr-read '(#\( #\;) (format nil "(a () b)"))
  (a nil b))

;; The standard readtable itself, where the built-in readers are used directly.
(deftest macro-char-roundtrip-values.standard-readtable-unchanged
  (let ((*readtable* (copy-readtable nil)))
    (values (read-from-string (format nil ";; comment~%(a () b)"))))
  (a nil b))

;; A comment still produces no value of its own: it does not become NIL.
(deftest macro-char-roundtrip-values.comment-produces-nothing
  (let ((*readtable* (%mcr-readtable '(#\( #\;))))
    (with-input-from-string (s (format nil ";; comment~%:first :second"))
      (list (read s) (read s))))
  (:first :second))

;; NIL written as NIL, and a quoted empty list, come through too.
(deftest macro-char-roundtrip-values.nil-spellings
  (%mcr-read '(#\( #\;) (format nil ";; c~%(nil () '() (quote ()))"))
  (nil nil (quote nil) (quote nil)))
