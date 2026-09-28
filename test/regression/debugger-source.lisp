;;; The debugger's :source, and the line :frame adds under the call form.
;;;
;;; A frame keeps only its function's name, so the debugger looks that name up
;;; among the definitions LOAD and COMPILE-FILE recorded (see
;;; frame-source-location.lisp). :frame adds one "source:" line only when the
;;; place is known for sure; :source prints the place and the lines around it,
;;; or the candidates, or why there is nothing.
;;;
;;; The debugger needs a terminal, so the text it prints is taken from
;;; DOTCL::%DEBUGGER-SOURCE-LINES, which both commands print from.

(defparameter *dsrc-dir* (regression-temp-dir))

(defparameter *dsrc-file*
  (format nil "~a/dotcl-dsrc-~d.lisp" *dsrc-dir* (random 1000000000)))

(with-open-file (s *dsrc-file* :direction :output
                               :if-exists :supersede :if-does-not-exist :create)
  (write-line "(defpackage :dsrc-a (:use :cl))" s)                      ; 1
  (write-line "(defpackage :dsrc-b (:use :cl))" s)                      ; 2
  (write-line "(in-package :dsrc-a)" s)                                 ; 3
  (write-line "(defun dsrc-inner (x) (error \"boom ~a\" x))" s)         ; 4
  (write-line "(defun dsrc-outer (y)" s)                                ; 5
  (write-line "  (dsrc-inner (1+ y)))" s)                               ; 6
  (write-line "(defmethod dsrc-gf ((x integer)) x)" s)                  ; 7
  (write-line "(defun dsrc-twice () :a)" s)                             ; 8
  (write-line "(defun dsrc-long () (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))" s) ; 9
  (write-line "(in-package :dsrc-b)" s)                                 ; 10
  (write-line "(defun dsrc-twice () :b)" s)                             ; 11
  (write-line "(in-package :cl-user)" s))                               ; 12

(load *dsrc-file*)

(defun dsrc-lines (name &optional brief (width 0))
  (dotcl::%debugger-source-lines name brief width))

(defun dsrc-ends-with (suffix string)
  (let ((n (- (length string) (length suffix))))
    (and (>= n 0) (string= suffix string :start2 n))))

;; :frame's line: the file and the line the definition starts on.
(deftest debugger-source-brief-names-file-and-line
  (let ((line (first (dsrc-lines "DSRC-OUTER" t))))
    (list (notnot (search ".lisp:5" line))
          (notnot (search "dotcl-dsrc-" line))))
  (t t))

;; :source: the place, then two lines before and four after, the definition's
;; first line marked.
(deftest debugger-source-full-listing
  (let ((lines (dsrc-lines "DSRC-OUTER")))
    (list (length lines)
          (notnot (search "; DSRC-OUTER: " (first lines)))
          (notnot (search ".lisp:5" (first lines)))
          (subseq lines 1)))
  (8 t t
   (";     3 | (in-package :dsrc-a)"
    ";     4 | (defun dsrc-inner (x) (error \"boom ~a\" x))"
    "; --> 5 | (defun dsrc-outer (y)"
    ";     6 |   (dsrc-inner (1+ y)))"
    ";     7 | (defmethod dsrc-gf ((x integer)) x)"
    ";     8 | (defun dsrc-twice () :a)"
    ";     9 | (defun dsrc-long () (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))")))

;; The listing stops at the start of the file.
(deftest debugger-source-listing-clipped-at-file-start
  (let ((lines (dotcl::%debugger-source-lines "DSRC-INNER")))
    (list (second lines) (fourth lines)))
  (";     2 | (defpackage :dsrc-b (:use :cl))"
   "; --> 4 | (defun dsrc-inner (x) (error \"boom ~a\" x))"))

;; Two-digit line numbers are padded so the bars line up.
(deftest debugger-source-line-number-padding
  (let ((lines (dsrc-lines "DSRC-LONG")))
    (list (second lines) (car (last lines))))
  (";      7 | (defmethod dsrc-gf ((x integer)) x)"
   ";     12 | (in-package :cl-user)"))

;; A line wider than the terminal is cut with "..." one column short of it.
(deftest debugger-source-width-cut
  (let* ((lines (dsrc-lines "DSRC-LONG" nil 40))
         (marked (find-if (lambda (l) (search "-->" l)) lines)))
    (list (length marked) (dsrc-ends-with "..." marked)
          (every (lambda (l) (<= (length l) 39)) (rest lines))))
  (39 t t))

;; A generic function's place is its last DEFMETHOD or DEFGENERIC, which need
;; not be the method in the frame; the line says so.
(deftest debugger-source-generic-function-note
  (notnot (search "(the last DEFMETHOD or DEFGENERIC read)"
                  (first (dsrc-lines "DSRC-GF" t))))
  t)

;; Nothing recorded (typed at the REPL, loaded from a fasl only): :frame adds
;; nothing, :source says why.
;; (A form LOAD reads is recorded, so define it through EVAL.)
(eval '(defun dsrc-typed-here () nil))

(deftest debugger-source-unknown-brief-says-nothing
  (dsrc-lines "DSRC-TYPED-HERE" t)
  nil)

(deftest debugger-source-unknown-full-says-why
  (dsrc-lines "DSRC-TYPED-HERE")
  ("; (no source location for DSRC-TYPED-HERE: locations are known for"
   ";  definitions LOAD or COMPILE-FILE read from a source file)"))

;; The same name defined in two packages: :frame cannot tell which, so it adds
;; nothing; :source lists both.
(deftest debugger-source-ambiguous-brief-says-nothing
  (dsrc-lines "DSRC-TWICE" t)
  nil)

(deftest debugger-source-ambiguous-full-lists-both
  (let ((lines (dsrc-lines "DSRC-TWICE")))
    (list (first lines)
          (length lines)
          (notnot (search "DSRC-A::DSRC-TWICE" (second lines)))
          (notnot (search ".lisp:8" (second lines)))
          (notnot (search "DSRC-B::DSRC-TWICE" (third lines)))
          (notnot (search ".lisp:11" (third lines)))))
  ("; DSRC-TWICE is defined in more than one package:" 3 t t t t))

;; ... unless one of them is the symbol the current package sees.
(deftest debugger-source-ambiguous-resolved-by-package
  (let ((*package* (find-package :dsrc-b)))
    (notnot (search ".lisp:11" (first (dsrc-lines "DSRC-TWICE" t)))))
  t)

;; A real frame: the name the backtrace keeps finds the definition.
(deftest debugger-source-from-backtrace-frame
  (block nil
    (handler-bind ((error (lambda (c)
                            (declare (ignore c))
                            (let ((names (dotcl:backtrace)))
                              (return
                                (list (notnot (member "DSRC-INNER" names :test #'string=))
                                      (notnot (search ".lisp:4"
                                                      (first (dsrc-lines "DSRC-INNER" t))))))))))
      (funcall (intern "DSRC-OUTER" :dsrc-a) 1)))
  (t t))

;; The file is gone: the place is still given, then why there are no lines.
(deftest debugger-source-file-gone
  (progn
    (delete-file *dsrc-file*)
    (let ((lines (dsrc-lines "DSRC-OUTER")))
      (list (length lines)
            (notnot (search ".lisp:5" (first lines)))
            (second lines))))
  (2 t ";  (the file cannot be read now)"))
