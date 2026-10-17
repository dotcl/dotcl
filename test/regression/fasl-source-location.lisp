;;; A fasl carries where its top level definitions are in the source, so
;;; DOTCL:FUNCTION-SOURCE-LOCATION answers for code loaded from the fasl in
;;; a process that never read the source (the Quicklisp case: libraries come
;;; from the fasl cache). COMPILE-FILE itself records the locations in the
;;; compiling process, so each test first points the record somewhere else
;;; and checks that loading the fasl brings it back. COMPILE-FILE needs the
;;; emitter, so not on the emit-free build.

(defparameter *fasl-sl-src* nil)
(defparameter *fasl-sl-fasl* nil)

(defun %fasl-sl-setup ()
  (unless *fasl-sl-fasl*
    (let* ((dir (regression-temp-dir))
           (src (merge-pathnames "fasl-sl-fixture.lisp" dir))
           (fasl (merge-pathnames "fasl-sl-fixture.fasl" dir)))
      (with-open-file (s src :direction :output :if-exists :supersede)
        (dolist (l '("(defpackage \"FASL-SL\" (:use \"CL\"))"          ; 1
                     "(in-package \"FASL-SL\")"                       ; 2
                     ""                                               ; 3
                     "(defun sl-fn (x) (* x 2))"                      ; 4
                     "(defmacro sl-mac (x) x)"                        ; 5
                     "(defgeneric sl-gf (x))"                         ; 6
                     "(defclass sl-class () ())"                      ; 7
                     "(defvar *sl-var* 1)"                            ; 8
                     "(defstruct (sl-struct (:constructor mk)) a)"   ; 9
                     "(progn (defun sl-in-progn () 1))"))             ; 10
          (write-line l s)))
      (let ((*error-output* (make-broadcast-stream)))
        (compile-file src :output-file fasl))
      (setf *fasl-sl-src* (namestring (truename src))
            *fasl-sl-fasl* fasl)))
  ;; Forget what COMPILE-FILE recorded, then load the fasl.
  (loop for (definer name) in '((defun "SL-FN") (defmacro "SL-MAC") (defgeneric "SL-GF")
                                 (defclass "SL-CLASS") (defvar "*SL-VAR*")
                                 (defstruct "SL-STRUCT") (defun "SL-IN-PROGN"))
        do (dotcl:record-definition-sources
            (list definer (intern name "FASL-SL")) "elsewhere.lisp" 999))
  (load *fasl-sl-fasl*)
  t)

(defun %fasl-sl-loc (name)
  (let ((loc (dotcl:function-source-location (find-symbol name "FASL-SL"))))
    (list (let ((file (probe-file (getf loc :file))))
            (and file (equal (namestring file) *fasl-sl-src*)))
          (getf loc :line))))

(deftest-emitting-only fasl-source-location.definers
  (progn
    (%fasl-sl-setup)
    (mapcar #'%fasl-sl-loc
            '("SL-FN" "SL-MAC" "SL-GF" "SL-CLASS" "*SL-VAR*" "SL-STRUCT" "SL-IN-PROGN")))
  ((t 4) (t 5) (t 6) (t 7) (t 8) (t 9) (t 10)))

;; A definition recorded after the fasl was loaded replaces the fasl's, even
;; though the fasl's table is only read at the first lookup.
(deftest-emitting-only fasl-source-location.later-definition-wins
  (progn
    (%fasl-sl-setup)
    (dotcl:record-definition-sources
     (list 'defun (find-symbol "SL-FN" "FASL-SL")) "later.lisp" 3)
    (list (dotcl:function-source-location (find-symbol "SL-FN" "FASL-SL"))
          (second (%fasl-sl-loc "SL-MAC"))))
  ((:file "later.lisp" :line 3) 5))
