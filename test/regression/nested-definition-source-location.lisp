;;; A definition nested in a top level PROGN, EVAL-WHEN, LOCALLY, MACROLET
;;; or SYMBOL-MACROLET keeps its own line, not the line of the outer form.
;;; So does one a top level macro form expands into (a macro call inside a
;;; PROGN is located at the call, not at the PROGN).

(defparameter *ndef-src* nil)

(defun %ndef-source ()
  (or *ndef-src*
      (let ((src (merge-pathnames "ndef-fixture.lisp" (regression-temp-dir))))
        (with-open-file (s src :direction :output :if-exists :supersede)
          (dolist (l '("(defpackage \"NDEF\" (:use \"CL\"))"                         ; 1
                       "(in-package \"NDEF\")"                                      ; 2
                       ""                                                           ; 3
                       "(defmacro ndef-defthing (v f)"                              ; 4
                       "  `(progn (defvar ,v 1) (defun ,f () 2)))"                  ; 5
                       "(defmacro ndef-defpt () '(progn (defstruct ndef-pt2 x)))"  ; 6
                       ""                                                           ; 7
                       "(progn"                                                     ; 8
                       "  (defun ndef-a () 1)"                                      ; 9
                       ""                                                           ; 10
                       "  (defstruct ndef-pt x y))"                                 ; 11
                       ""                                                           ; 12
                       "(eval-when (:compile-toplevel :load-toplevel :execute)"     ; 13
                       "  (defun ndef-eval-when () 2))"                             ; 14
                       ""                                                           ; 15
                       "(locally"                                                   ; 16
                       "  (declare (special *ndef-special*))"                       ; 17
                       "  (defmacro ndef-mac (x) x)"                                ; 18
                       "  (defgeneric ndef-gf (x))"                                 ; 19
                       "  (defmethod ndef-gf ((x integer)) x))"                     ; 20
                       ""                                                           ; 21
                       "(macrolet ((nothing () nil))"                               ; 22
                       "  (defclass ndef-class () ()))"                             ; 23
                       ""                                                           ; 24
                       "(symbol-macrolet ((nothing nil))"                           ; 25
                       "  (defvar *ndef-var* 1)"                                    ; 26
                       "  (defparameter *ndef-param* 2)"                            ; 27
                       "  (defconstant +ndef-const+ 3))"                            ; 28
                       ""                                                           ; 29
                       "(progn"                                                     ; 30
                       "  (deftype ndef-type () 'integer)"                          ; 31
                       "  (define-condition ndef-condition (error) ()))"            ; 32
                       ""                                                           ; 33
                       "(progn"                                                     ; 34
                       "  (ndef-defthing *ndef-x* ndef-x-fn)"                       ; 35
                       "  (ndef-defpt))"                                            ; 36
                       ""                                                           ; 37
                       "(defmacro ndef-read-definition ()"                          ; 38
                       "  (read-from-string \"(defun ndef-read-fn () 4)\"))"        ; 39
                       ""                                                           ; 40
                       "(ndef-read-definition)"))                                   ; 41
            (write-line l s)))
        (setf *ndef-src* (namestring (truename src))))))

(defun %ndef-results ()
  (mapcar (lambda (name)
            (mapcar (lambda (p)
                      (list (getf p :kind)
                            (getf p :line)
                            (and (equal (namestring (probe-file (getf p :file)))
                                        *ndef-src*)
                                 t)))
                    (dotcl:definition-sources (find-symbol name "NDEF"))))
          '("NDEF-A" "NDEF-PT" "NDEF-EVAL-WHEN" "NDEF-MAC" "NDEF-GF" "NDEF-CLASS"
            "*NDEF-VAR*" "*NDEF-PARAM*" "+NDEF-CONST+" "NDEF-TYPE"
            "NDEF-CONDITION" "*NDEF-X*" "NDEF-X-FN" "NDEF-PT2"
            "NDEF-READ-FN")))

(defparameter *ndef-expected*
  '(((:FUNCTION 9 t))
    ((:STRUCTURE 11 t))
    ((:FUNCTION 14 t))
    ((:MACRO 18 t))
    ((:GENERIC-FUNCTION 19 t) (:METHOD 20 t))
    ((:CLASS 23 t))
    ((:VARIABLE 26 t))
    ((:VARIABLE 27 t))
    ((:CONSTANT 28 t))
    ((:TYPE 31 t))
    ((:CLASS 32 t) (:CONDITION 32 t))
    ((:VARIABLE 35 t))
    ((:FUNCTION 35 t))
    ((:STRUCTURE 36 t))
    ((:FUNCTION 41 t))))

(defun %ndef-expected ()
  ;; Without a compiler DEFINE-CONDITION's expansion does not also report a class.
  (if (emitting-mode-p)
      *ndef-expected*
      (substitute '((:CONDITION 32 t)) '((:CLASS 32 t) (:CONDITION 32 t))
                  *ndef-expected* :test #'equal)))

(defun %ndef-matches (got)
  (let ((want (%ndef-expected)))
    (or (equal got want)
        (format nil "got ~S~%want ~S" got want))))

(deftest nested-definition-source-location.load-source
  (progn
    (when (find-package "NDEF") (delete-package "NDEF"))
    (load (%ndef-source))
    (%ndef-matches (%ndef-results)))
  t)

(deftest-emitting-only nested-definition-source-location.load-fasl
  (let ((fasl (merge-pathnames "ndef-fixture.fasl" (regression-temp-dir)))
        (src (%ndef-source)))
    (when (find-package "NDEF") (delete-package "NDEF"))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    (let ((*package* (find-package "NDEF")))
      (dolist (d '(("NDEF-A" . defun) ("NDEF-PT" . defstruct)
                   ("NDEF-EVAL-WHEN" . defun) ("NDEF-MAC" . defmacro)
                   ("NDEF-GF" . defgeneric) ("NDEF-CLASS" . defclass)
                   ("*NDEF-VAR*" . defvar) ("*NDEF-PARAM*" . defparameter)
                   ("+NDEF-CONST+" . defconstant) ("NDEF-TYPE" . deftype)
                   ("NDEF-CONDITION" . define-condition) ("*NDEF-X*" . defvar)
                   ("NDEF-X-FN" . defun) ("NDEF-PT2" . defstruct)
                   ("NDEF-READ-FN" . defun)))
        (dotcl:record-definition-sources
         (list (cdr d) (find-symbol (car d) "NDEF")) "elsewhere.lisp" 999)))
    (delete-package "NDEF")
    (load fasl)
    (%ndef-matches (%ndef-results)))
  t)
