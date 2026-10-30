;;; The check of (THE <array type> X) keeps the last array that passed in a local
;;; the check takes by address. In a body with enough locals for slot sharing
;;; that local must stay declared and keep a slot of its own. The first shape is
;;; flexi-streams' STRING-TO-OCTETS* for the CRLF UTF-16 formats: a local
;;; function called twice, holding an inlined one with several THE sites.

(defparameter *tacsl-encoder*
  '(lambda (string start end)
     (declare (fixnum start end) (string string))
     (let ((octets (make-array (* 8 (- end start)) :element-type '(unsigned-byte 8)))
           (j 0))
       (declare (fixnum j))
       (loop for i of-type fixnum from start below end do
         (macrolet ((ow (form)
                      `(progn (setf (aref (the (array (unsigned-byte 8) *) octets) j) ,form)
                              (incf j))))
           (flet ((wc (c)
                    (flet ((write-word (word)
                             (ow (logand #x00ff word))
                             (ow (ash (logand #xff00 word) -8))))
                      (declare (inline write-word))
                      (let ((code (char-code c)))
                        (cond ((< code #x10000) (write-word code))
                              (t (decf code #x10000)
                                 (write-word (logior #xd800 (ash code -10)))
                                 (write-word (logior #xdc00 (logand #x03ff code)))))))))
             (let ((c (char string i)))
               (cond ((char= c #\Newline) (wc #\Return) (wc #\Newline))
                     (t (wc c)))))))
       (coerce (subseq octets 0 j) 'list))))

;; Straight-line code with many THE sites, then one whose type the array is
;; not: the check there must not take the slot of a local that still holds it.
(defparameter *tacsl-straight*
  `(lambda (a b)
     (list ,@(loop for k below 20
                   collect `(aref (the (simple-array (unsigned-byte 8) (*)) a) ,(mod k 2)))
           (aref (the (simple-array fixnum (*)) b) 0))))

(defun %tacsl-load-fasl ()
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames "the-array-check-shared-locals.lisp" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*package* (find-package :cl-user)))
          (prin1 `(defun %tacsl-encode-fasl ,@(cdr *tacsl-encoder*)) s)
          (prin1 `(defun %tacsl-straight-fasl ,@(cdr *tacsl-straight*)) s))))
    (load (compile-file src :output-file
                        (merge-pathnames "the-array-check-shared-locals.fasl" dir)))))

(when (emitting-mode-p) (%tacsl-load-fasl))

(deftest-emitting-only the-array-check-shared-locals.compile-file
  (funcall '%tacsl-encode-fasl (format nil "a~%b") 0 3)
  (97 0 13 0 10 0 98 0))

(deftest-emitting-only the-array-check-shared-locals.compile
  (funcall (compile nil *tacsl-encoder*) (format nil "a~%b") 0 3)
  (97 0 13 0 10 0 98 0))

(defun %tacsl-straight-outcome (fn)
  (let ((u8 (make-array 2 :element-type '(unsigned-byte 8) :initial-element 7))
        (fx (make-array 1 :element-type 'fixnum :initial-element 9)))
    (list (length (funcall fn u8 fx))
          (handler-case (funcall fn u8 u8) (type-error () :type-error)))))

(deftest-emitting-only the-array-check-shared-locals.straight-compile-file
  (%tacsl-straight-outcome '%tacsl-straight-fasl)
  (21 :type-error))

(deftest-emitting-only the-array-check-shared-locals.straight-compile
  (%tacsl-straight-outcome (compile nil *tacsl-straight*))
  (21 :type-error))
