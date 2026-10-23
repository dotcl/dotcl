;;; dotcl-lsp-coalton.lisp: Coalton type errors and types for dotcl-lsp.
;;;
;;;   (asdf:load-system "coalton")
;;;   (require "dotcl-lsp-coalton")
;;;
;;; A .coal document, or a .lisp document with coalton-toplevel forms, is type
;;; checked from the text the editor sent, without writing or loading it. The
;;; global Coalton environment is rebound around each check and put back. Hover
;;; shows the type of a global Coalton value, including those the document
;;; itself defines.
;;;
;;; Coalton is reached through FIND-SYMBOL, so this file compiles without it.
;;; Until Coalton is loaded the functions here decline every document.

(require "dotcl-lsp")

(defpackage :dotcl-lsp-coalton
  (:use :cl :dotcl-lsp)
  (:export #:check-document #:coalton-available-p))

(in-package :dotcl-lsp-coalton)

(defun coalton-available-p ()
  (and (find-package "COALTON-IMPL/ENTRY") t))

(defun %c (package name)
  (or (find-symbol name (concatenate 'string "COALTON-IMPL/" package))
      (error "Coalton has no ~A in COALTON-IMPL/~A" name package)))

(defun %call (package name &rest args)
  (apply (symbol-function (%c package name)) args))

(defun %coal-document-p (document)
  (or (equal (document-language-id document) "coalton")
      (let ((uri (document-uri document)))
        (and (> (length uri) 5) (string-equal ".coal" uri :start2 (- (length uri) 5))))))

(defun %lisp-document-p (document)
  (search "coalton-toplevel" (document-text document) :test #'char-equal))

;;; Conditions to diagnostics.

(defun %severity (condition)
  (case (ignore-errors (%call "SOURCE" "SEVERITY" condition))
    (:warn :warning)
    (:style-warning :information)
    (t :error)))

(defun %note-span (note source)
  (let ((location (%call "SOURCE" "LOCATION" note)))
    (when (eq (%call "SOURCE" "LOCATION-SOURCE" location) source)
      (%call "SOURCE" "LOCATION-SPAN" location))))

(defun %condition-diagnostic (condition source)
  (let* ((notes (%call "SOURCE" "NOTES" condition))
         (kind (lambda (n) (%call "SOURCE" "NOTE-TYPE" n)))
         (primary (or (find :primary notes :key kind) (first notes)))
         (span (and primary (%note-span primary source))))
    (when span
      (let ((note-message (%call "SOURCE" "MESSAGE" primary)))
        (list :start (car span) :end (cdr span)
              :severity (%severity condition)
              :source "coalton"
              :message (format nil "~A~@[: ~A~]~{~%help: ~A~}"
                               (%call "SOURCE" "MESSAGE" condition)
                               (and (plusp (length note-message)) note-message)
                               (loop for n in notes
                                     when (eq (funcall kind n) :help)
                                       collect (%call "SOURCE" "MESSAGE" n)))
              :related (loop for n in notes
                             for s = (and (eq (funcall kind n) :secondary)
                                          (%note-span n source))
                             when s collect (list (car s) (cdr s)
                                                  (%call "SOURCE" "MESSAGE" n))))))))

(defun %other-diagnostic (text start end error)
  (let ((start (or (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                                    text :start start :end end)
                   start)))
    (list :start start :end (max start end) :severity :error :source "coalton"
          :message (princ-to-string error))))

(defmacro %collecting-conditions ((list) &body body)
  "Run BODY, pushing each Coalton source condition signalled onto LIST and
muffling warnings."
  `(handler-bind ((condition
                    (lambda (c)
                      (when (typep c (%c "SOURCE" "SOURCE-CONDITION"))
                        (pushnew c ,list))
                      (when (and (typep c 'warning) (find-restart 'muffle-warning c))
                        (muffle-warning c)))))
     ,@body))

;;; Checking.

(defparameter *lisp-forms-to-evaluate* '("DEFPACKAGE" "IN-PACKAGE" "IN-READTABLE")
  "Forms of a .lisp document evaluated while it is read: the ones that decide
how the rest is read. Nothing else is evaluated.")

(defun %check-lisp (text source)
  "Read TEXT as a file would be, with Coalton checking each coalton-toplevel as
it is read. Returns (values conditions other-diagnostics environment)."
  (let ((conditions '()) (others '())
        (env-var (%c "ENTRY" "*GLOBAL-ENVIRONMENT*")))
    (progv (list env-var (%c "READER" "*SOURCE*") '*package* '*readtable*)
        (list (symbol-value env-var) source (find-package "CL-USER") (copy-readtable nil))
      (with-input-from-string (in text)
        (loop
          (let ((start (file-position in)))
            (multiple-value-bind (form error)
                (handler-case (%collecting-conditions (conditions)
                                (values (read in nil in) nil))
                  (error (e) (values nil e)))
              (cond
                (error
                 (unless (typep error (%c "SOURCE" "SOURCE-CONDITION"))
                   (push (%other-diagnostic text start (file-position in) error) others))
                 (file-position in start)
                 (let ((*read-suppress* t) (*readtable* (copy-readtable nil)))
                   (handler-case (read in nil in)
                     (error () (return)))))
                ((eq form in) (return))
                ((and (consp form) (symbolp (car form))
                      (member (symbol-name (car form)) *lisp-forms-to-evaluate*
                              :test #'string=))
                 (handler-case (%collecting-conditions (conditions) (eval form))
                   (error (e)
                     (push (%other-diagnostic text start (file-position in) e) others))))))))
        (values conditions others (symbol-value env-var))))))

(defvar *coal-checker* nil)

(defun %coal-checker ()
  "A function of a source that reads it as a .coal file and type checks it,
returning the environment it defines. Built once Coalton is there, since
the reading context is a macro of Coalton's."
  (or *coal-checker*
      (setf *coal-checker*
            (compile nil
                     `(lambda (source)
                        (with-open-stream (stream (,(%c "SOURCE" "SOURCE-STREAM") source))
                          (,(%c "PARSER" "WITH-READER-CONTEXT") stream
                           (nth-value 1 (,(%c "ENTRY" "ENTRY-POINT")
                                         (,(%c "PARSER" "READ-PROGRAM") stream source :file))))))))))

(defun %check-coal (source)
  (let ((conditions '()) (others '()) (env nil)
        (env-var (%c "ENTRY" "*GLOBAL-ENVIRONMENT*")))
    (progv (list env-var) (list (symbol-value env-var))
      (handler-case (%collecting-conditions (conditions)
                      (setf env (funcall (%coal-checker) source)))
        (error (e)
          (unless (typep e (%c "SOURCE" "SOURCE-CONDITION"))
            (push (list :start 0 :end 0 :severity :error :source "coalton"
                        :message (princ-to-string e))
                  others)))))
    (values conditions others env)))

(defun check-document (document)
  "The diagnostics of DOCUMENT, or NIL when it is not a Coalton document or
Coalton is not loaded. Remembers the environment the document defines, for
hover."
  (when (and (coalton-available-p)
             (or (%coal-document-p document) (%lisp-document-p document)))
    (let* ((text (document-text document))
           (source (%call "SOURCE" "MAKE-SOURCE-STRING" text :name (document-uri document))))
      (multiple-value-bind (conditions others env)
          (if (%coal-document-p document)
              (%check-coal source)
              (%check-lisp text source))
        (when env (setf (document-property document :coalton-environment) env))
        (let ((seen (make-hash-table :test 'equal)) (out '()))
          (dolist (d (append (remove nil (mapcar (lambda (c) (%condition-diagnostic c source))
                                                 (reverse conditions)))
                             (reverse others)))
            (let ((key (list (getf d :start) (getf d :end) (getf d :message))))
              (unless (gethash key seen)
                (setf (gethash key seen) t)
                (push d out))))
          (or (nreverse out) :none))))))

;;; Hover.

(defun %coal-package (text)
  (let ((start (search "(package" text :test #'char-equal)))
    (when start
      (let* ((name-start (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                                          text :start (+ start 8)))
             (name-end (and name-start
                            (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\))))
                                         text :start name-start))))
        (and name-end (find-package (string-upcase (subseq text name-start name-end))))))))

(defun hover (document offset)
  (when (and (coalton-available-p)
             (or (%coal-document-p document) (%lisp-document-p document)))
    (let* ((text (document-text document))
           (package (or (if (%coal-document-p document)
                            (%coal-package text)
                            (dotcl-code-info::%text-package text offset nil))
                        (find-package "CL-USER")))
           (env (or (document-property document :coalton-environment)
                    (symbol-value (%c "ENTRY" "*GLOBAL-ENVIRONMENT*")))))
      (multiple-value-bind (kind symbol) (dotcl-code-info::%at-point text offset package)
        (when (eq kind :symbol)
          (let ((type (%call "TYPECHECKER" "LOOKUP-VALUE-TYPE" env symbol :no-error t)))
            (when type
              (format nil "```coalton~%~(~A~) :: ~A~%```" (symbol-name symbol)
                      (%call "TYPECHECKER/TYPE-STRING" "TYPE-TO-STRING" type env)))))))))

(pushnew 'check-document dotcl-lsp:*diagnostic-functions*)
(pushnew 'hover dotcl-lsp:*hover-functions*)

(provide "dotcl-lsp-coalton")
