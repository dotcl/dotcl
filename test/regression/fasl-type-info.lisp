;;; A declaimed FTYPE and the body of a DEFUN proclaimed INLINE reach code
;;; compiled in another image that only LOADs the fasl.
;;;
;;; Both used to be recorded only in the image that compiled them. Code compiled
;;; after loading the fasl in a fresh image then made a generic call: the ftype
;;; caller boxed the result and did generic arithmetic on it (71.8 B per call
;;; against 24 B in the compiling image), and the INLINE function was not
;;; substituted at all (71.8 B against 0 B). A DEFSTRUCT's accessors were not
;;; inlined either (a fixnum slot read was 48 B against 0 B). The fasl now
;;; carries a PROCLAIM of each.
;;;
;;; One child compiles the library file, a second, fresh one loads only its
;;; fasl, compiles the user file and reports bytes per call. The library also
;;; has an inline function made by a macro (its parameters are gensyms, whose
;;; identity has to survive the fasl) and one whose body holds a hash table (no
;;; literal form, so it stays out of the fasl and is simply not inlined there).

(defvar *fti-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *fti-core* (regression-child-core))

(defparameter *fti-lib* "fti-lib-tmp.lisp")
(defparameter *fti-lib-fasl* "fti-lib-tmp.fasl")
(defparameter *fti-user* "fti-user-tmp.lisp")
(defparameter *fti-user-fasl* "fti-user-tmp.fasl")
(defparameter *fti-driver* "fti-driver-tmp.lisp")

(defun %fti-write (path text)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string text s)))

(defun %fti-child (&rest args)
  (dotcl:run-process *fti-exe* (list* "--core" *fti-core* args)))

(defun %fti-run ()
  "The second child's output read as one list, or the child's result when it
   did not exit 0."
  (unwind-protect
       (progn
         (%fti-write *fti-lib* "(defpackage :fti-lib (:use :cl))
(in-package :fti-lib)
(declaim (ftype (function (fixnum) (unsigned-byte 8)) fa))
(defun fa (x) (logand x 255))
(declaim (inline fi))
(defun fi (x) (declare (fixnum x)) (logand x 255))
(defmacro defsq (name)
  (let ((v (gensym \"V\")) (w (gensym \"W\")))
    `(progn (declaim (inline ,name))
            (defun ,name (,v) (let ((,w (* ,v ,v))) (+ ,w 1))))))
(defsq sq)
(declaim (inline with-table))
(defun with-table (x) (list x #.(make-hash-table)))
(defstruct sp (n 0 :type fixnum) (tag nil))
")
         (%fti-write *fti-user* "(in-package :fti-lib)
(defun loop-fa (v n)
  (declare (type (simple-array (unsigned-byte 32) (*)) v) (fixnum n))
  (dotimes (i n) (setf (aref v 0) (+ 100000 (fa i)))))
(defun loop-fi (v n)
  (declare (type (simple-array (unsigned-byte 32) (*)) v) (fixnum n))
  (dotimes (i n) (setf (aref v 0) (+ 100000 (fi i)))))
(defun use-others (a) (list (sq a) (car (with-table a))))
(defun loop-sp (s v n)
  (declare (type (simple-array (unsigned-byte 32) (*)) v) (fixnum n))
  (dotimes (i n) (setf (aref v 0) (+ 100000 (sp-n s)))))
(defun set-sp (s n)
  (declare (fixnum n))
  (dotimes (i n) (setf (sp-n s) i))
  (setf (sp-tag s) :done)
  (list (sp-n s) (sp-tag s)))
")
         (%fti-write *fti-driver* (format nil "(load ~s)
(load (compile-file ~s :output-file ~s))
(defun bpo (fn)
  (funcall fn 1000)
  (flet ((s (n) (let ((b (nth 4 (dotcl:gc-stats))))
                  (funcall fn n)
                  (- (nth 4 (dotcl:gc-stats)) b))))
    (/ (- (s 200000) (s 100000)) 100000.0)))
(let ((v (make-array 1 :element-type '(unsigned-byte 32)))
      (sp (funcall 'fti-lib::make-sp :n 3)))
  (format t \"~%FTI-RESULT \")
  (prin1 (list (bpo (lambda (n) (funcall 'fti-lib::loop-fa v n)))
               (bpo (lambda (n) (funcall 'fti-lib::loop-fi v n)))
               (funcall 'fti-lib::use-others 3)
               (bpo (lambda (n) (funcall 'fti-lib::loop-sp sp v n)))
               (funcall 'fti-lib::set-sp sp 5))))
" *fti-lib-fasl* *fti-user* *fti-user-fasl*))
         (let ((r1 (%fti-child "--eval"
                               (format nil "(compile-file ~s :output-file ~s)"
                                       *fti-lib* *fti-lib-fasl*))))
           (if (/= (first r1) 0)
               r1
               (let ((r2 (%fti-child *fti-driver*)))
                 (if (/= (first r2) 0)
                     r2
                     (let* ((out (second r2))
                            (at (search "FTI-RESULT " out)))
                       (if at
                           (read-from-string out nil out :start (+ at 11))
                           out)))))))
    (dolist (f (list *fti-lib* *fti-lib-fasl* *fti-user* *fti-user-fasl* *fti-driver*))
      (ignore-errors (delete-file f)))))

(defvar *fti-result* nil)
(defun %fti-result () (or *fti-result* (setq *fti-result* (%fti-run))))

;; A structure's accessor: a fixnum slot read at the call site, not through
;; the accessor function (48 B per read when it was called).
(deftest fasl-type-info.struct-read-after-load
  (let ((r (%fti-result)))
    (and (consp r) (numberp (fourth r)) (< (fourth r) 1)))
  t)

;; Stores through the SETF expansion recorded at load, raw and boxed slots.
(deftest fasl-type-info.struct-store-after-load
  (fifth (%fti-result))
  (4 :done))

;; 24 B is the boxed argument of the call itself; the generic path was 71.8.
(deftest fasl-type-info.ftype-after-load
  (let ((r (%fti-result)))
    (and (consp r) (numberp (first r)) (< (first r) 32)))
  t)

(deftest fasl-type-info.inline-after-load
  (let ((r (%fti-result)))
    (and (consp r) (numberp (second r)) (< (second r) 1)))
  t)

(deftest fasl-type-info.gensym-body-and-unrecorded-body
  (third (%fti-result))
  (10 3))
