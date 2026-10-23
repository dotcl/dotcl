;;;; Concurrent compile probes for dotcl: run COMPILE / COMPILE-FILE / EVAL from
;;;; several threads at once and check what comes out.
;;;;
;;;;   THR_TEST=cfile-distinct THR_N=4 THR_ITERS=20 dotcl threads.lisp
;;;;
;;;; Prints "RESULT <test> ok|BROKEN errors=N ..." and up to 8 distinct errors.
;;;; Run each test with THR_N=1 first: that is the control and must say ok.
;;;;
;;;; Environment:
;;;;   THR_TEST   lambda cfile-distinct cfile-samepkg defmacro-race intern
;;;;              make-package gensym clos defstruct ltv specials pload macrolet
;;;;              system
;;;;   THR_N      threads (default 2);  THR_ITERS  work per thread (default 50)
;;;;   THR_BIND   if set, each worker binds the compiler's file-level specials
;;;;              first (*compile-file-mode* and the like), which COMPILE-FILE
;;;;              otherwise assigns globally
;;;;   THR_SYS    for "system": the ASDF system (default alexandria), found
;;;;              through CL_SOURCE_REGISTRY or THR_ASD_DIR/<system>.asd;
;;;;              prints "PAR <system> n=N <ms>" for the parallel compile-file
;;;;   THR_WORK   scratch directory (default pcprobe-work/ under the current one)
(require "dotcl-thread") (require "asdf")
(defpackage :pcprobe (:use :cl))
(in-package :pcprobe)

(defvar *dir* (make-pathname :name nil :type nil :defaults *load-truename*))
(defvar *test* (or (dotcl:getenv "THR_TEST") "lambda"))
(defvar *iters* (parse-integer (or (dotcl:getenv "THR_ITERS") "50")))
(defvar *nthreads* (parse-integer (or (dotcl:getenv "THR_N") "2")))

(defvar *errors* nil)
(defvar *queue* nil)
(defvar *elock* (dotcl-thread:make-lock "errors"))

(defun note-error (who e)
  (dotcl-thread:with-lock-held (*elock*)
    (push (format nil "~A: ~A: ~A" who (type-of e)
                  (let ((s (ignore-errors (princ-to-string e))))
                    (subseq (or s "?") 0 (min 300 (length (or s ""))))))
          *errors*)))

(defmacro guarded (who &body body)
  `(handler-case (progn ,@body)
     (serious-condition (e) (note-error ,who e) nil)))

(defun run-threads (fn)
  "Run FN (thread-index) in *nthreads* threads, started together."
  (let* ((go nil)
         (threads (loop for i below *nthreads*
                        collect (let ((i i))
                                  (dotcl-thread:make-thread
                                   (lambda ()
                                     (loop until go do (dotcl-thread:thread-yield))
                                     (guarded (format nil "thread ~D top" i)
                                       (if (dotcl:getenv "THR_BIND")
                                           (let ((syms (loop for n in '("*COMPILE-FILE-MODE*" "*SAME-FILE-FUNCTIONS*" "*COMPILE-FILE-PATHNAME*" "*COMPILE-FILE-TRUENAME*" "*PACKAGE*" "*READTABLE*")
                                                             for s = (or (find-symbol n "DOTCL-INTERNAL") (find-symbol n "CL"))
                                                             when s collect s)))
                                             (progv syms (mapcar #'symbol-value syms) (funcall fn i)))
                                           (funcall fn i))))
                                   :name (format nil "w~D" i))))))
    (setf go t)
    (mapc #'dotcl-thread:thread-join threads)))

(defun report (name &optional extra)
  (format t "~&RESULT ~A ~A errors=~D ~@[~A~]~%" name
          (if *errors* "BROKEN" "ok") (length *errors*) extra)
  (loop for e in (remove-duplicates (reverse *errors*) :test #'string=)
        repeat 8 do (format t "  ~A~%" (substitute #\Space #\Newline e)))
  (finish-output))

(defun tmpfile (name type)
  (merge-pathnames (make-pathname :name name :type type) (uiop:ensure-directory-pathname (or (dotcl:getenv "THR_WORK") "pcprobe-work"))))

(defun write-source (path forms &optional (package "CL-USER"))
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (let ((*package* (find-package "CL-USER")) (*print-readably* nil) (*print-case* :downcase))
      (format s "(in-package ~S)~%" package)
      (dolist (f forms) (prin1 f s) (terpri s)))))

;;; 1. COMPILE of lambdas in parallel
(defun t-lambda ()
  (run-threads
   (lambda (i)
     (dotimes (j (* 4 *iters*))
       (guarded (format nil "w~D lambda ~D" i j)
         (let* ((k (+ (* 1000 i) j))
                (f (compile nil `(lambda (x)
                                   (flet ((g (y) (+ y ,k)))
                                     (macrolet ((twice (z) `(* 2 ,z)))
                                       (let ((acc 0))
                                         (dotimes (n x) (incf acc (twice (g n))))
                                         (handler-case (values acc (format nil "~A" ,k))
                                           (error () -1))))))))
                (want (* 2 (loop for n below 10 sum (+ n k)))))
           (unless (eql (funcall f 10) want)
             (error "wrong result ~A /= ~A" (funcall f 10) want)))))))
  (report "lambda"))

;;; 2. COMPILE-FILE of distinct files (own packages) in parallel, then load and check
(defun gen-file-forms (i j)
  (let* ((pkg (format nil "THR-P-~D-~D" i j))
         (sym (lambda (n) (intern (string-upcase n) pkg))))
    (values
     pkg
     `((defpackage ,pkg (:use :cl))
       (in-package ,pkg))
     (let ((*package* (or (find-package pkg) (make-package pkg :use '(:cl)))))
       (declare (ignorable *package*))
       `((defconstant ,(funcall sym "+k+") ,(+ (* 1000 i) j))
         (defmacro ,(funcall sym "with-acc") ((var) &body body)
           (let ((g (gensym "ACC")))
             `(let ((,g 0)) (flet ((,var (x) (incf ,g x))) ,@body) ,g)))
         (defstruct ,(funcall sym "pt") x y)
         (defclass ,(funcall sym "box") () ((v :initarg :v :accessor ,(funcall sym "box-v"))))
         (defgeneric ,(funcall sym "area") (o))
         (defmethod ,(funcall sym "area") ((o ,(funcall sym "box"))) (* 2 (,(funcall sym "box-v") o)))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defun ,(funcall sym "helper") (n) (* n 3)))
         (defmacro ,(funcall sym "tripled") (n) (,(funcall sym "helper") n))
         (defun ,(funcall sym "check") ()
           (+ (,(funcall sym "with-acc") (add) (dotimes (n 10) (add n)))
              (,(funcall sym "pt-x") (,(funcall sym "make-pt") :x 5 :y 6))
              (,(funcall sym "area") (make-instance ',(funcall sym "box") :v 7))
              (,(funcall sym "tripled") 4)
              ,(funcall sym "+k+")
              (loop for x in '(1 2 3) collect x into l finally (return (length l)))
              (handler-case (error "x") (error () 1)))))))))

(defun expected-check (i j) (+ 45 5 14 12 (+ (* 1000 i) j) 3 1))

(defun t-cfile-distinct ()
  (let ((fasls (make-array *nthreads* :initial-element nil)))
    (run-threads
     (lambda (i)
       (dotimes (j *iters*)
         (guarded (format nil "w~D cfile ~D" i j)
           (multiple-value-bind (pkg head body) (gen-file-forms i j)
             (declare (ignore pkg))
             (let ((src (tmpfile (format nil "d-~D-~D" i j) "lisp")))
               (write-source src (append head body))
               (multiple-value-bind (out warn fail)
                   (let ((*error-output* (make-broadcast-stream))) (compile-file src))
                 (declare (ignore warn))
                 (when (or (null out) fail) (error "compile-file failed ~A" (file-namestring src)))
                 (push (list i j out) (aref fasls i)))))))))
    ;; load sequentially and verify
    (let ((bad 0))
      (loop for i below *nthreads*
            do (dolist (e (reverse (aref fasls i)))
                 (destructuring-bind (ii jj out) e
                   (guarded (format nil "load ~D ~D" ii jj)
                     (load out)
                     (let ((miss (loop for n in '("MAKE-PT" "PT-X" "PT-P" "COPY-PT" "BOX-V" "AREA" "HELPER" "TRIPLED" "CHECK" "+K+" "WITH-ACC")
                                       for s = (find-symbol n (format nil "THR-P-~D-~D" ii jj))
                                       unless (and s (or (fboundp s) (boundp s))) collect n)))
                       (when miss (format t "~&MISSING ~D ~D ~S where=~S~%" ii jj miss
                                          (mapcar (lambda (x) (list x (package-name (symbol-package x)) (fboundp x))) (find-all-symbols "MAKE-PT")))))
                     (let ((got (funcall (find-symbol "CHECK" (format nil "THR-P-~D-~D" ii jj)))))
                       (unless (eql got (expected-check ii jj))
                         (incf bad)
                         (error "check ~D ~D: got ~A want ~A" ii jj got (expected-check ii jj))))))))
      (report "cfile-distinct" (format nil "fasls=~D bad=~D" (loop for v across fasls sum (length v)) bad)))))

;;; 3. Same as 2 but every file in one shared package
(defun t-cfile-samepkg ()
  (unless (find-package "THR-SHARED") (make-package "THR-SHARED" :use '(:cl)))
  (let ((fasls (make-array *nthreads* :initial-element nil)))
    (run-threads
     (lambda (i)
       (dotimes (j *iters*)
         (guarded (format nil "w~D cfile ~D" i j)
           (let* ((*package* (find-package "THR-SHARED"))
                  (s (lambda (n) (intern (format nil "~A-~D-~D" n i j) "THR-SHARED")))
                  (forms `((defmacro ,(funcall s "M") (x) `(list ',',(funcall s "M") ,x))
                           (defstruct ,(funcall s "S") a)
                           (defun ,(funcall s "F") () (,(funcall s "M") (,(intern (format nil "~A-A" (funcall s "S")) "THR-SHARED") (,(intern (format nil "MAKE-~A" (funcall s "S")) "THR-SHARED") :a ,j))))))
                  (src (tmpfile (format nil "s-~D-~D" i j) "lisp")))
             (write-source src forms "THR-SHARED")
             (let ((out (let ((*error-output* (make-broadcast-stream))) (compile-file src))))
               (unless out (error "compile-file failed"))
               (push (list i j out (funcall s "F") (funcall s "M")) (aref fasls i))))))))
    (loop for i below *nthreads*
          do (dolist (e (aref fasls i))
               (destructuring-bind (ii jj out f m) e
                 (guarded (format nil "load ~D ~D" ii jj)
                   (load out)
                   (unless (equal (funcall f) (list m jj)) (error "bad result ~S" (funcall f)))))))
    (report "cfile-samepkg")))

;;; 4. DEFMACRO (eval) in one thread while others compile using macros
(defun t-defmacro-race ()
  (run-threads
   (lambda (i)
     (dotimes (j (* 20 *iters*))
       (guarded (format nil "w~D defmacro ~D" i j)
         (let ((name (intern (format nil "RM-~D-~D" i j) :pcprobe)))
           (eval `(defmacro ,name (x) `(+ ,x ,,j)))
           (let ((f (compile nil `(lambda () (,name 1)))))
             (unless (eql (funcall f) (1+ j)) (error "bad ~A" (funcall f)))))))))
  (report "defmacro-race"))

;;; 5. intern / make-package
(defun t-intern ()
  (unless (find-package "THR-INTERN") (make-package "THR-INTERN" :use nil))
  (run-threads
   (lambda (i)
     (dotimes (j (* 2000 *iters*))
       (guarded (format nil "w~D intern" i)
         (intern (format nil "S~D-~D" i j) "THR-INTERN")))))
  (let ((n 0)) (do-symbols (s "THR-INTERN") (declare (ignore s)) (incf n))
    (let ((want (* *nthreads* 2000 *iters*)))
      (unless (= n want) (push (format nil "symbol count ~D want ~D" n want) *errors*))
      (loop for i below *nthreads*
            do (dotimes (j (* 2000 *iters*))
                 (unless (find-symbol (format nil "S~D-~D" i j) "THR-INTERN")
                   (push (format nil "lost S~D-~D" i j) *errors*) (return))))
      (report "intern" (format nil "symbols=~D want=~D" n want)))))

(defun t-make-package ()
  (run-threads
   (lambda (i)
     (dotimes (j (* 20 *iters*))
       (guarded (format nil "w~D make-package" i)
         (make-package (format nil "THR-MP-~D-~D" i j) :use '(:cl))))))
  (let ((lost (loop for i below *nthreads*
                    sum (loop for j below (* 20 *iters*)
                              count (null (find-package (format nil "THR-MP-~D-~D" i j)))))))
    (when (plusp lost) (push (format nil "lost packages ~D" lost) *errors*))
    (report "make-package" (format nil "lost=~D" lost))))

;;; 6. gensym counter
(defun t-gensym ()
  (let* ((start *gensym-counter*)
         (per (* 4000 *iters*))
         (names (make-array *nthreads*)))
    (run-threads
     (lambda (i)
       (setf (aref names i) (loop repeat per collect (symbol-name (gensym "G"))))))
    (let* ((all (loop for v across names append v))
           (h (make-hash-table :test 'equal)) (dups 0))
      (dolist (n all) (if (gethash n h) (incf dups) (setf (gethash n h) t)))
      (let ((advance (- *gensym-counter* start)))
        (when (plusp dups) (push (format nil "duplicate gensym names ~D" dups) *errors*))
        (unless (= advance (* per *nthreads*))
          (push (format nil "counter advanced ~D want ~D" advance (* per *nthreads*)) *errors*))
        (report "gensym" (format nil "dups=~D advance=~D want=~D" dups advance (* per *nthreads*)))))))

;;; 7. defclass/defmethod/defgeneric at the same time (CLOS tables)
(defun t-clos ()
  (run-threads
   (lambda (i)
     (dotimes (j (* 4 *iters*))
       (guarded (format nil "w~D clos ~D" i j)
         (let ((c (intern (format nil "C-~D-~D" i j) :pcprobe))
               (g (intern (format nil "GF-~D" (mod j 5)) :pcprobe)))
           (eval `(defclass ,c () ((a :initform ,j :accessor ,(intern (format nil "~A-A" c) :pcprobe)))))
           (eval `(defmethod ,g ((o ,c)) (list ',c (slot-value o 'a))))
           (let ((r (funcall g (make-instance c))))
             (unless (equal r (list c j)) (error "bad ~S" r))))))))
  (report "clos"))

;;; 8. defstruct concurrently (eval)
(defun t-defstruct ()
  (run-threads
   (lambda (i)
     (dotimes (j (* 4 *iters*))
       (guarded (format nil "w~D defstruct ~D" i j)
         (let ((s (intern (format nil "ST-~D-~D" i j) :pcprobe)))
           (eval `(defstruct ,s a b))
           (let ((o (funcall (intern (format nil "MAKE-~A" s) :pcprobe) :a i :b j)))
             (unless (eql (funcall (intern (format nil "~A-B" s) :pcprobe) o) j) (error "bad"))))))))
  (report "defstruct"))

;;; 9. A real system: load it, then compile-file its files again from N threads
;;;    (a shared work queue), and load every fasl produced
(defun t-system ()
  (let* ((sysname (or (dotcl:getenv "THR_SYS") "alexandria"))
         (root (dotcl:getenv "THR_ASD_DIR")))
    (let ((*error-output* (make-broadcast-stream)))
      (when (and root (null (dotcl:getenv "CL_SOURCE_REGISTRY")))
        (asdf:load-asd (merge-pathnames (format nil "~A.asd" sysname) root)))
      (asdf:load-system sysname))
    (let* ((sys (asdf:find-system sysname))
           (comps (remove-if-not (lambda (c) (and (typep c (find-symbol "CL-SOURCE-FILE" "ASDF"))
                                                  (string= (asdf:primary-system-name (asdf:component-system c)) (asdf:primary-system-name sys))))
                                 (funcall (find-symbol "REQUIRED-COMPONENTS" "ASDF") sys :other-systems t)))
           (paths (mapcar (find-symbol "COMPONENT-PATHNAME" "ASDF") comps))
           (outs (make-array *nthreads* :initial-element nil))
           (t0 (get-internal-real-time)))
      (setf *queue* (loop for r below (max 1 (floor *iters* 10)) append (loop for p in paths collect (cons r p))))
      (run-threads
       (lambda (i)
         (loop for task = (dotcl-thread:with-lock-held (*elock*) (pop *queue*))
               while task
               do (destructuring-bind (r . p) task
                   (guarded (format nil "w~D ~A" i (file-namestring p))
                        (let ((out (tmpfile (format nil "alx-~D-~D-~A" i r (pathname-name p)) "fasl")))
                          (ensure-directories-exist out)
                          (let ((errs (make-string-output-stream)))
                          (multiple-value-bind (o w f)
                              (let ((*error-output* errs)
                                    (*standard-output* (make-broadcast-stream)))
                                (compile-file p :output-file out))
                            (declare (ignore w))
                            (when (or (null o) f)
                              (let ((txt (get-output-stream-string errs)))
                                (error "failed ~A: ~A" (file-namestring p)
                                       (subseq txt 0 (min 250 (length txt))))))
                            (push o (aref outs i))))))))))
      (format t "~&PAR ~A n=~D ~D ms~%" sysname *nthreads* (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second))
      ;; reload every produced fasl
      (loop for v across outs
            do (dolist (o v) (guarded (format nil "load ~A" (file-namestring o)) (load o))))
      (when (string= sysname "alexandria")
       (guarded "after"
        (unless (equal (funcall (find-symbol "FLATTEN" "ALEXANDRIA") '(1 (2 (3)))) '(1 2 3)) (error "flatten"))
        (unless (eql (funcall (find-symbol "CLAMP" "ALEXANDRIA") 5 0 3) 3) (error "clamp"))))
      (report sysname (format nil "files=~D fasls=~D" (length paths) (loop for v across outs sum (length v)))))))

;;; 10. LOAD-TIME-VALUE under COMPILE in parallel
(defun t-ltv ()
  (run-threads
   (lambda (i)
     (dotimes (j (* 10 *iters*))
       (guarded (format nil "w~D ltv ~D" i j)
         (let* ((k (+ (* 100000 i) j))
                (f (compile nil `(lambda () (load-time-value (list ,k))))))
           (unless (equal (funcall f) (list k)) (error "ltv got ~S want ~S" (funcall f) (list k))))))))
  (report "ltv"))

;;; 11. special proclamations from two threads
(defun t-specials ()
  (let ((names (make-array *nthreads* :initial-element nil)))
    (run-threads
     (lambda (i)
       (dotimes (j (* 20 *iters*))
         (guarded (format nil "w~D defvar" i)
           (let ((v (intern (format nil "*SV-~D-~D*" i j) :pcprobe)))
             (eval `(defvar ,v 0))
             (push v (aref names i)))))))
    (let ((lost 0))
      (loop for v across names
            do (dolist (s v)
                 (let ((f (compile nil `(lambda () (let ((,s 1)) (declare (ignorable ,s)) (symbol-value ',s))))))
                   (unless (eql (funcall f) 1) (incf lost)))))
      (when (plusp lost) (push (format nil "lost special proclamations ~D" lost) *errors*))
      (report "specials" (format nil "lost=~D" lost)))))

;;; 12. parallel compile-file + LOAD of files adding methods to shared generic functions
(defgeneric shared-gf (o))
(defun t-pload ()
  (let ((made (make-array *nthreads* :initial-element nil)))
    (run-threads
     (lambda (i)
       (dotimes (j (* 2 *iters*))
         (guarded (format nil "w~D pload ~D" i j)
           (let* ((c (intern (format nil "PC-~D-~D" i j) :pcprobe))
                  (src (tmpfile (format nil "pl-~D-~D" i j) "lisp")))
             (write-source src `((defclass ,c () ((a :initform ,j)))
                                 (defmethod shared-gf ((o ,c)) (list ',c (slot-value o 'a)))
                                 (defmethod print-object ((o ,c) s) (format s "#<~A>" ',c)))
                           "PCPROBE")
             (let ((out (let ((*error-output* (make-broadcast-stream))) (compile-file src))))
               (load out)
               (push (cons c j) (aref made i))))))))
    (let ((lost 0) (lostp 0))
      (loop for v across made
            do (loop for (c . j) in v
                     do (let ((o (make-instance c)))
                          (unless (ignore-errors (equal (shared-gf o) (list c j))) (incf lost))
                          (unless (equal (prin1-to-string o) (format nil "#<~A>" c)) (incf lostp)))))
      (when (plusp (+ lost lostp)) (push (format nil "lost methods shared-gf=~D print-object=~D" lost lostp) *errors*))
      (report "pload" (format nil "lost=~D lost-print-object=~D" lost lostp)))))

;;; 13. same-named MACROLETs with different expansions in parallel COMPILEs
(defun t-macrolet ()
  (run-threads
   (lambda (i)
     (dotimes (j (* 4 *iters*))
       (guarded (format nil "w~D macrolet ~D" i j)
         (let* ((k (+ (* 1000 i) j))
                (f (compile nil `(lambda ()
                                   (macrolet ((local-m (x) `(+ ,x ,,k)))
                                     (let ((acc 0))
                                       (dotimes (n 3) (incf acc (local-m n)))
                                       acc))))))
           (unless (eql (funcall f) (+ 3 (* 3 k)))
             (error "macrolet got ~A want ~A" (funcall f) (+ 3 (* 3 k)))))))))
  (report "macrolet"))

(let ((fn (intern (string-upcase (format nil "T-~A" *test*)) :pcprobe)))
  (format t "~&TEST ~A n=~D iters=~D~%" *test* *nthreads* *iters*)
  (let ((t0 (get-internal-real-time)))
    (funcall fn)
    (format t "~&TIME ~A ~D ms~%" *test* (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second))))
