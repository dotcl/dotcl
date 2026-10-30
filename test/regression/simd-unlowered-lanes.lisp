;;; Lane kinds without a backing type, and the float-only operations: the
;;; Simd.Load/Store/Broadcast/Div/Sqrt entries are never emitted for these.

(require "dotcl-simd")

(defparameter *sul-kinds*
  '(((signed-byte 16) "S16.8" "Int16")
    ((signed-byte 8) "S8.16" "SByte")
    ((unsigned-byte 32) "U32.4" "UInt32")))

(defun %sul-sym (kind name)
  (find-symbol (concatenate 'string kind name) "DOTCL-SIMD"))

(defun %sul-names (fn)
  (let* ((text (%flat-text (dotcl:function-sil fn)))
         (names '())
         (start 0))
    (loop
      (let ((at (search "Simd." text :start2 start)))
        (unless at (return (sort names #'string<)))
        (let ((end (position-if (lambda (c) (member c '(#\Space #\" #\))))
                                text :start at)))
          (pushnew (subseq text at end) names :test #'string=)
          (setq start (1+ at)))))))

(defun %sul-def (name lambda-list body)
  (let ((dotcl:*save-sil* t))
    (eval `(defun ,name ,lambda-list ,@body)))
  (symbol-function name))

(defun %sul-read (name type kind)
  (%sul-def name '(a i)
    `((declare (type (simple-array ,type (*)) a) (fixnum i)
               (optimize (safety 0)))
      (loop repeat 1 do (aref a 0))
      (load-time-value 0)
      (,(%sul-sym kind "-AREF") a i))))

(defun %sul-write (name type kind)
  (%sul-def name '(a i v)
    `((declare (type (simple-array ,type (*)) a) (fixnum i)
               (optimize (safety 0)))
      (loop repeat 1 do (aref a 0))
      (load-time-value 0)
      (setf (,(%sul-sym kind "-AREF") a i) v))))

(defun %sul-constant (name kind)
  (%sul-def name '(a)
    `((declare (type ,(%sul-sym kind "") a))
      (,(%sul-sym kind "+") a 1))))

(defun %sul-method-names (lane)
  (loop for op in '("Load" "LoadAt" "LoadNoCheck" "LoadRaw"
                    "Store" "StoreAt" "StoreNoCheck" "StoreRaw")
        collect (format nil "Simd.~A.~A" op lane)))

(defun %sul-lane-count (lane)
  (cdr (assoc lane '(("Int16" . 8) ("SByte" . 16) ("UInt32" . 4))
              :test #'string=)))

(deftest-compiled-only simd-unlowered-lanes.array-methods-absent
  (let ((out '()) (i 0))
    (dolist (entry *sul-kinds* (nreverse out))
      (destructuring-bind (type kind lane) entry
        (let* ((read (%sul-read (intern (format nil "%SUL-READ-~D" i)) type kind))
               (write (%sul-write (intern (format nil "%SUL-WRITE-~D" i)) type kind))
               (allowed (%sul-method-names lane))
               (seen (append (%sul-names read) (%sul-names write))))
          (unless (and (search (format nil "~A-AREF" kind)
                               (%flat-text (dotcl:function-sil read)))
                       (search (format nil "~A-AREF" kind)
                               (%flat-text (dotcl:function-sil write))))
            (push (list kind :not-compiled) out))
          (dolist (name allowed)
            (when (member name seen :test #'string=)
              (push name out))))
        (incf i))))
  nil)

(deftest-compiled-only simd-unlowered-lanes.broadcast-absent
  (let ((out '()) (i 0))
    (dolist (entry *sul-kinds* (nreverse out))
      (destructuring-bind (type kind lane) entry
        (declare (ignore type))
        (let* ((fn (%sul-constant (intern (format nil "%SUL-CONST-~D" i)) kind))
               (broadcast (format nil "Simd.Broadcast.~A" lane))
               (add (format nil "Simd.Add.~A" lane))
               (seen (%sul-names fn)))
          (unless (member add seen :test #'string=)
            (push (list kind :add-not-lowered) out))
          (when (member broadcast seen :test #'string=)
            (push broadcast out)))
        (incf i))))
  nil)

(deftest simd-unlowered-lanes.float-only-operations
  (let ((out '()))
    (dolist (entry *sul-kinds* out)
      (destructuring-bind (type kind lane) entry
        (declare (ignore type lane))
        (dolist (suffix '("/" "-SQRT"))
          (let ((name (%sul-sym kind suffix)))
            (when (fboundp name) (push (list kind suffix) out)))))))
  nil)

(deftest simd-unlowered-lanes.values
  (let ((out '()))
    (dolist (entry *sul-kinds* (nreverse out))
      (destructuring-bind (type kind lane) entry
        (let* ((lanes (%sul-lane-count lane))
               (len (* 2 lanes))
               (a (make-array len :element-type type :initial-element 0))
               (read (%sul-sym kind "-AREF"))
               (write (fdefinition (list 'setf read)))
               (values (%sul-sym kind "-VALUES"))
               (make (find-symbol (concatenate 'string "MAKE-" kind) "DOTCL-SIMD")))
          (dotimes (j len) (setf (aref a j) (coerce j type)))
          (let ((got (multiple-value-list (funcall values (funcall read a 0)))))
            (funcall write
                     (apply (symbol-function make)
                            (loop for j below lanes collect (+ 10 j)))
                     a lanes)
            (push (list got
                        (coerce a 'list)
                        (handler-case (progn (funcall read a (1+ (- len lanes))) :no-error)
                          (error () :error)))
                  out))))))
  (((0 1 2 3 4 5 6 7) (0 1 2 3 4 5 6 7 10 11 12 13 14 15 16 17) :error)
   ((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
    (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)
    :error)
   ((0 1 2 3) (0 1 2 3 10 11 12 13) :error)))
