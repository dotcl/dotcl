;;; asdf-extra-deps.lisp -- add the dependencies a system forgot to declare,
;;; from a file kept outside the system, without editing its .asd.
;;;
;;;   (require "asdf-extra-deps")                ; dotcl
;;;   (load "asdf-extra-deps.lisp")              ; any other Lisp, after ASDF
;;;   (asdf-extra-deps:load-supplements "supplements.sexp")
;;;   (asdf:load-system "serapeum")
;;;
;;; A supplement file holds one form per system:
;;;
;;;   (:system "serapeum"
;;;    :asd-sha256 ("3f2a...")         ; optional: the .asd must hash to one of these
;;;    :version ("0.0.1")              ; optional: the system's version must be one of these
;;;    :depends-on ((("level1" "vectors") ("level0" "sequences"))
;;;                 ("iter" "macro-tools" (:system "alexandria"))))
;;;
;;; Each :DEPENDS-ON entry is a component followed by what it depends on. A
;;; component is named by its path from the system: a string for a child of
;;; the system, a list of strings below that. A dependency is such a path, or
;;; (:SYSTEM name). The component's PREPARE-OP then also depends on the LOAD-OP
;;; of each, so it is compiled and loaded after them.
;;;
;;; When the system's .asd or version no longer matches the guards, the entry
;;; does nothing: a new release of the system, which may declare the
;;; dependency itself, is left alone until the file is updated. A path that
;;; names no component is skipped and listed in *SKIPPED*.

(require "asdf")

(defpackage :asdf-extra-deps
  (:use :cl)
  (:export #:load-supplements
           #:add-supplement
           #:clear-supplements
           #:*supplements*
           #:extra-dependencies
           #:*skipped*
           #:file-sha256))

(in-package :asdf-extra-deps)

(defvar *supplements* '()
  "The supplement forms in effect, the latest first.")

(defvar *skipped* '()
  "Dependencies that named no component: (system component dependency).")

(defvar *by-system* (make-hash-table :test 'eq)
  "System object -> hash table from component to the components it gets, or
   :NONE.")

(defun clear-supplements ()
  (setf *supplements* '() *skipped* '())
  (clrhash *by-system*)
  (values))

(defun add-supplement (form)
  "Put one supplement form, as described at the top of this file, in effect."
  (unless (and (consp form) (eq (first form) :system) (stringp (second form)))
    (error "asdf-extra-deps: not a supplement form: ~S" form))
  (push form *supplements*)
  (clrhash *by-system*)
  form)

(defun load-supplements (file)
  "Read FILE's supplement forms and put them in effect. Returns how many."
  (let ((n 0))
    (with-open-file (s file :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*read-eval* nil)
              (*package* (find-package :asdf-extra-deps)))
          (loop for form = (read s nil s)
                until (eq form s)
                do (add-supplement form) (incf n)))))
    n))

;;; ------------------------------------------------------------------
;;; SHA-256, for the :ASD-SHA256 guard.

(defparameter +k+
  (coerce
   '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
     #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
     #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
     #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
     #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
     #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
     #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
     #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)
   'vector))

(defun sha256-hex (octets)
  (labels ((u32 (x) (logand x #xffffffff))
           (rotr (x n) (u32 (logior (ash x (- n)) (ash x (- 32 n))))))
    (let* ((len (length octets))
           (total (* 64 (ceiling (+ len 9) 64)))
           (msg (make-array total :element-type '(unsigned-byte 8) :initial-element 0))
           (h (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                      #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
           (w (make-array 64)))
      (replace msg octets)
      (setf (aref msg len) #x80)
      (loop for i below 8
            do (setf (aref msg (- total 1 i)) (ldb (byte 8 (* 8 i)) (* 8 len))))
      (loop for base from 0 below total by 64 do
        (dotimes (i 16)
          (setf (aref w i)
                (logior (ash (aref msg (+ base (* 4 i))) 24)
                        (ash (aref msg (+ base (* 4 i) 1)) 16)
                        (ash (aref msg (+ base (* 4 i) 2)) 8)
                        (aref msg (+ base (* 4 i) 3)))))
        (loop for i from 16 below 64
              do (let* ((x (aref w (- i 15))) (y (aref w (- i 2)))
                        (s0 (logxor (rotr x 7) (rotr x 18) (ash x -3)))
                        (s1 (logxor (rotr y 17) (rotr y 19) (ash y -10))))
                   (setf (aref w i) (u32 (+ (aref w (- i 16)) s0 (aref w (- i 7)) s1)))))
        (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
              (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
          (dotimes (i 64)
            (let* ((t1 (u32 (+ hh
                               (logxor (rotr e 6) (rotr e 11) (rotr e 25))
                               (logxor (logand e f) (logand (logxor e #xffffffff) g))
                               (aref +k+ i) (aref w i))))
                   (t2 (u32 (+ (logxor (rotr a 2) (rotr a 13) (rotr a 22))
                               (logxor (logand a b) (logand a c) (logand b c))))))
              (setf hh g g f f e e (u32 (+ d t1)) d c c b b a a (u32 (+ t1 t2)))))
          (loop for x in (list a b c d e f g hh) for i from 0
                do (setf (aref h i) (u32 (+ (aref h i) x))))))
      (format nil "~(~{~8,'0x~}~)" (coerce h 'list)))))

(defun file-sha256 (file)
  "The SHA-256 of FILE's bytes, as lowercase hex."
  (let ((octets (with-open-file (s file :element-type '(unsigned-byte 8))
                  (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
                    (read-sequence v s)
                    v))))
    (sha256-hex octets)))

;;; ------------------------------------------------------------------
;;; Matching supplements to systems.

(defun listify (x) (if (listp x) x (list x)))

(defun component-path (x) (if (listp x) x (list x)))

(defun guard-matches-p (form system)
  (let ((hashes (getf (cddr form) :asd-sha256))
        (versions (getf (cddr form) :version)))
    (and (or (null hashes)
             (let ((asd (asdf:system-source-file system)))
               (and asd (probe-file asd)
                    (member (file-sha256 asd) (listify hashes) :test #'string-equal))))
         (or (null versions)
             (member (asdf:component-version system) (listify versions) :test #'equal)))))

(defun resolve (system dep)
  (if (and (consp dep) (eq (first dep) :system))
      (asdf:find-system (second dep) nil)
      (asdf:find-component system (component-path dep))))

(defun system-table (system)
  (multiple-value-bind (table hit) (gethash system *by-system*)
    (if hit
        table
        (setf (gethash system *by-system*)
              (let ((name (asdf:component-name system))
                    (table nil))
                (dolist (form *supplements*)
                  (when (and (string= (second form) name) (guard-matches-p form system))
                    (unless table (setf table (make-hash-table :test 'eq)))
                    (dolist (entry (getf (cddr form) :depends-on))
                      (let ((c (asdf:find-component system (component-path (first entry)))))
                        (dolist (dep (rest entry))
                          (let ((d (and c (resolve system dep))))
                            (if d
                                (pushnew d (gethash c table))
                                (push (list name (first entry) dep) *skipped*))))))))
                (or table :none))))))

(defun extra-dependencies (component)
  "The components the supplements in effect add to COMPONENT's dependencies."
  (let ((system (asdf:component-system component)))
    (when system
      (let ((table (system-table system)))
        (unless (eq table :none)
          (reverse (gethash component table)))))))

(defmethod asdf:component-depends-on :around ((o asdf:prepare-op) (c asdf:component))
  (let ((extra (and *supplements* (extra-dependencies c))))
    (if extra
        (append (call-next-method) (list (cons 'asdf:load-op extra)))
        (call-next-method))))
