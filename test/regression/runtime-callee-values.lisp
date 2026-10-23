;;; A built-in function that calls Lisp code (a :TEST, a :KEY, a predicate, a
;;; handler, a method) and then returns a value of its own must return that one
;;; value, not the values the Lisp code it called left behind.
(require "dotcl-gray")

(defun %rcv-nil (&rest a) (declare (ignore a)) (values nil :leak))
(defun %rcv-t (&rest a) (declare (ignore a)) (values t :leak))
(defun %rcv-id (x &rest a) (declare (ignore a)) (values x :leak))

(defmacro %rcv-mvl (&rest forms)
  `(list ,@(mapcar (lambda (f) `(multiple-value-list ,f)) forms)))

(deftest runtime-callee-values.sequences
  (%rcv-mvl (find-if #'%rcv-nil '(1)) (find-if #'%rcv-nil #(1)) (find-if #'%rcv-nil "a")
            (find-if #'%rcv-id '(1)) (find-if-not #'%rcv-t '(1))
            (find 1 '(2) :test #'%rcv-nil) (find 1 '(1) :key #'%rcv-nil)
            (position-if #'%rcv-nil '(1)) (position 1 #(2) :test #'%rcv-nil)
            (count-if #'%rcv-id '(1)) (count 1 '(1) :test #'%rcv-id)
            (member-if #'%rcv-nil '(1)) (member 1 '(2) :test #'%rcv-nil)
            (assoc-if #'%rcv-nil '((1))) (assoc 1 '((2)) :test #'%rcv-nil)
            (rassoc-if #'%rcv-nil '((1 . 2))) (rassoc 1 '((2 . 3)) :test #'%rcv-nil)
            (search '(1) '(2) :test #'%rcv-nil) (mismatch '(1) '(1) :test-not #'%rcv-nil)
            (remove 1 (list 1) :test-not #'%rcv-nil) (delete-if #'%rcv-t (list 1))
            (reduce #'+ '(1) :key #'%rcv-id) (sort (list 1) #'%rcv-nil))
  ((nil) (nil) (nil) (1) (nil) (nil) (nil) (nil) (nil) (1) (1) (nil) (nil)
   (nil) (nil) (nil) (nil) (nil) (nil) (nil) (nil) (1) ((1))))

(deftest runtime-callee-values.signal-and-hash
  (%rcv-mvl (handler-bind ((condition #'%rcv-nil)) (signal 'simple-condition))
            (handler-bind ((warning #'%rcv-nil))
              (let ((*error-output* (make-broadcast-stream))) (warn "x")))
            (let ((h (make-hash-table))) (setf (gethash 1 h) 2) (maphash #'%rcv-nil h))
            (typep 1 '(satisfies %rcv-nil))
            (typep 1 '(and integer (satisfies %rcv-nil))))
  ((nil) (nil) (nil) (nil) (nil)))

(defun %rcv-rst-handler (&rest a) (declare (ignore a)) :handled)

(deftest runtime-callee-values.restarts
  (%rcv-mvl
   ;; applicable: the :TEST-FUNCTION reported (t :leak), the restart itself won
   (restart-bind ((%rcv-r1 #'%rcv-rst-handler :test-function #'%rcv-t))
     (and (find-restart '%rcv-r1) t))
   ;; not applicable: the :TEST-FUNCTION reported (nil :leak) and FIND-RESTART
   ;; must answer its own NIL, not the leaked second value or the restart
   (restart-bind ((%rcv-r2 #'%rcv-rst-handler :test-function #'%rcv-nil))
     (find-restart '%rcv-r2))
   ;; the leak shape: an unrelated name after :TEST-FUNCTION published (t :leak)
   (restart-bind ((%rcv-r3 #'%rcv-rst-handler :test-function #'%rcv-t))
     (find-restart '%rcv-unbound-name))
   (restart-bind ((%rcv-r4 #'%rcv-rst-handler :test-function #'%rcv-nil))
     (find-restart '%rcv-unbound-name)))
  ((t) (nil) (nil) (nil)))

(defclass %rcv-po () ())
(defmethod print-object ((o %rcv-po) s) (write-string "#<po>" s) (values o :leak))

(deftest runtime-callee-values.printer-and-format
  (let ((o (make-instance '%rcv-po))
        (null (make-broadcast-stream)))
    (mapcar (lambda (r) (if (eq (car r) o) (cons :o (cdr r)) r))
            (%rcv-mvl (prin1 o null) (princ o null) (write o :stream null)
                      (format null "~/cl-user::%rcv-nil/" 1)
                      (format null #'%rcv-nil))))
  ((:o) (:o) (:o) (nil) (nil)))

(defclass %rcv-gs (dotcl-gray:fundamental-character-input-stream
                   dotcl-gray:fundamental-character-output-stream) ())
(defmethod dotcl-gray:stream-read-char ((s %rcv-gs)) (values #\a :leak))
(defmethod dotcl-gray:stream-peek-char ((s %rcv-gs)) (values #\a :leak))
(defmethod dotcl-gray:stream-listen ((s %rcv-gs)) (values nil :leak))
(defmethod dotcl-gray:stream-write-char ((s %rcv-gs) c) (values c :leak))
(defmethod dotcl-gray:stream-finish-output ((s %rcv-gs)) (values nil :leak))
(defmethod dotcl-gray:stream-terpri ((s %rcv-gs)) (values nil :leak))
(defmethod dotcl-gray:stream-fresh-line ((s %rcv-gs)) (values nil :leak))

(deftest runtime-callee-values.gray-stream
  (let ((s (make-instance '%rcv-gs)))
    (%rcv-mvl (read-char s) (peek-char nil s) (listen s) (write-char #\a s)
              (finish-output s) (terpri s) (fresh-line s)))
  ((#\a) (#\a) (nil) (#\a) (nil) (nil) (nil)))

(defclass %rcv-c1 () ((a :initarg :a :initform 1)))
(defclass %rcv-c2 () ((a :initarg :a :initform 1)))
(defmethod initialize-instance :around ((o %rcv-c1) &key) (call-next-method) (values o :leak))
(defmethod update-instance-for-different-class :around ((o %rcv-c1) (n %rcv-c2) &key)
  (call-next-method) (values n :leak))
(defmethod dotcl-mop:slot-value-using-class :around (cl (o %rcv-c2) s)
  (declare (ignore cl s))
  (values (call-next-method) :leak))
(defmethod dotcl-mop:slot-boundp-using-class :around (cl (o %rcv-c2) s)
  (declare (ignore cl s))
  (values (call-next-method) :leak))

(deftest runtime-callee-values.clos
  (let* ((c1 (make-instance '%rcv-c1))
         (c2 (make-instance '%rcv-c2)))
    (list (length (multiple-value-list (make-instance '%rcv-c1)))
          (length (multiple-value-list (change-class c1 '%rcv-c2)))
          (multiple-value-list (slot-value c2 'a))
          (multiple-value-list (slot-boundp c2 'a))))
  (1 1 (1) (t)))

;; A method-group pattern naming a function: only its primary value counts.
(defun %rcv-mgpred-leak (quals) (declare (ignore quals)) (values nil :leak))
(defun %rcv-mgpred-t (quals) (declare (ignore quals)) (values t :leak))

(define-method-combination %rcv-mgcomb-nil ()
  ((main %rcv-mgpred-leak) (all *))
  (:generic-function gf)
  `(progn ,@(mapcar (lambda (m) `(list 'all (call-method ,m))) all)
          ,@(mapcar (lambda (m) `(list 'main (call-method ,m))) main)))
(define-method-combination %rcv-mgcomb-t ()
  ((main %rcv-mgpred-t) (all *))
  (:generic-function gf)
  `(progn ,@(mapcar (lambda (m) `(list 'all (call-method ,m))) all)
          ,@(mapcar (lambda (m) `(list 'main (call-method ,m))) main)))

(defgeneric %rcv-mgf-nil (x) (:method-combination %rcv-mgcomb-nil))
(defgeneric %rcv-mgf-t (x) (:method-combination %rcv-mgcomb-t))
(defmethod %rcv-mgf-nil :quux ((x integer)) :hit)
(defmethod %rcv-mgf-t :quux ((x integer)) :hit)

(deftest runtime-callee-values.method-group
  (%rcv-mvl (%rcv-mgf-nil 3) (%rcv-mgf-t 3))
  (((all :hit)) ((main :hit))))

