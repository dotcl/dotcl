;;; Printer bugs found by a differential test against SBCL 2.6.8; expected strings are SBCL's.

;;; 1. *PRINT-READABLY* signals PRINT-NOT-READABLE for built-in #<...> objects.

(defun %pdf-pnr-object (thunk)
  "The object THUNK failed to print readably, or :NO-ERROR."
  (handler-case (progn (funcall thunk) :no-error)
    (print-not-readable (c) (print-not-readable-object c))))

(defclass %pdf-class () ())
(defstruct %pdf-struct a b)

(deftest printer-differential-fixes.readably-builtins
  (let ((h (make-hash-table))
        (p (find-package :cl))
        (f (symbol-function 'car))
        (s (make-string-output-stream))
        (c (make-condition 'simple-error :format-control "E"))
        (r *readtable*)
        (k (find-class '%pdf-class)))
    (list (eq h (%pdf-pnr-object (lambda () (write-to-string h :readably t))))
          (eq h (let ((*read-eval* nil))
                  (%pdf-pnr-object (lambda () (write-to-string h :readably t)))))
          (eq p (%pdf-pnr-object (lambda () (write-to-string p :readably t))))
          (eq f (%pdf-pnr-object (lambda () (write-to-string f :readably t))))
          (eq s (%pdf-pnr-object (lambda () (write-to-string s :readably t))))
          (eq c (%pdf-pnr-object (lambda () (write-to-string c :readably t))))
          (eq r (%pdf-pnr-object (lambda () (write-to-string r :readably t))))
          (eq k (%pdf-pnr-object (lambda () (write-to-string k :readably t))))))
  (t t t t t t t t))

(deftest printer-differential-fixes.readably-nested-struct-slot
  (let ((h (make-hash-table)))
    (eq h (%pdf-pnr-object
           (lambda ()
             (write-to-string (make-%pdf-struct :a h :b 2) :readably t)))))
  t)

;;; 2. :TYPE and :IDENTITY with no body forms get one space between them.

(defun %pdf-one-space-before-id-p (obj type-name)
  (let ((out (with-output-to-string (s)
               (print-unreadable-object (obj s :type t :identity t)))))
    (let ((space (char out (+ 2 (length type-name)))))
      (list (char= space #\Space)
            (not (char= (char out (1+ (+ 2 (length type-name)))) #\Space))))))

(deftest printer-differential-fixes.unreadable-object-one-space
  (let ((*package* (find-package :cl-user)))
    (list (%pdf-one-space-before-id-p (make-instance '%pdf-class) "%PDF-CLASS")
          (%pdf-one-space-before-id-p #\a "STANDARD-CHAR")
          (%pdf-one-space-before-id-p (cons 1 2) "CONS")))
  ((t t) (t t) (t t)))

;;; 3. A cycle a PRINT-OBJECT method writes is a back-reference under *PRINT-CIRCLE*.

(defclass %pdf-cycle () ((next :initarg :next :initform nil)))
(defmethod print-object ((o %pdf-cycle) s)
  (print-unreadable-object (o s :type t :identity nil)
    (write (slot-value o 'next) :stream s)))

(deftest printer-differential-fixes.print-circle-in-method
  (let ((x (make-instance '%pdf-cycle)))
    (setf (slot-value x 'next) x)
    (let ((*print-circle* t))
      (write-to-string x :circle t)))
  "#1=#<%PDF-CYCLE #1#>")

(deftest printer-differential-fixes.print-circle-mutual-in-method
  (let ((a (make-instance '%pdf-cycle))
        (b (make-instance '%pdf-cycle)))
    (setf (slot-value a 'next) b
          (slot-value b 'next) a)
    (let ((*print-circle* t))
      (write-to-string a :circle t)))
  "#1=#<%PDF-CYCLE #<%PDF-CYCLE #1#>>")

;;; 4. *PRINT-READABLY* does not force *PRINT-RADIX*.

(deftest printer-differential-fixes.readably-does-not-force-radix
  (list (write-to-string 255 :base 16 :radix nil :readably t)
        (write-to-string 1/255 :base 2 :radix nil :readably t)
        (write-to-string '(1 2 3) :base 2 :radix nil :readably t))
  ("FF" "1/11111111" "(1 10 11)"))

;;; 5. #S slot names are symbols, so *PRINT-CASE* applies to them.

(defstruct %pdf-case-struct a b)

(deftest printer-differential-fixes.struct-slot-print-case
  (list (write-to-string (make-%pdf-case-struct :a 1 :b 2) :case :downcase)
        (write-to-string (make-%pdf-case-struct :a 1 :b 2) :case :downcase
                         :readably t))
  ("#S(%pdf-case-struct :a 1 :b 2)"
   "#S(%pdf-case-struct :a 1 :b 2)"))

;;; 6. PRINT-NOT-READABLE's USE-VALUE replacement prints in the object's place.

(defclass %pdf-use-value () ())

(defun %pdf-use-value-string (obj replacement)
  (handler-bind ((print-not-readable
                   (lambda (c)
                     (let ((r (find-restart 'use-value c)))
                       (when r (invoke-restart r replacement))))))
    (write-to-string obj :readably t)))

(deftest printer-differential-fixes.print-not-readable-use-value
  (list (%pdf-use-value-string (make-instance '%pdf-use-value) "REPLACED")
        (%pdf-use-value-string (make-hash-table) 42)
        (%pdf-use-value-string (symbol-function 'car) :replacement))
  ("\"REPLACED\"" "42" ":REPLACEMENT"))
