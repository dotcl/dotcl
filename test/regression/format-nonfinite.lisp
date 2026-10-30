;;; ~F, ~E, ~G and ~$ print infinities and NaN as their readable constants
;;; instead of cutting up .NET's "Infinity" and "NaN" strings.
;;;
;;; SBCL 2.6.8 prints #.DOUBLE-FLOAT-POSITIVE-INFINITY for infinities and
;;; #<DOUBLE-FLOAT quiet NaN> for NaN, and switches to #<...> under *READ-EVAL*
;;; NIL. dotcl has readable constants for NaN as well, so FORMAT uses the text
;;; PRIN1 already produces. Unlike SBCL, a field narrower than that text uses
;;; overflowchar when one is given; the text itself is never split.

(defvar *format-nonfinite-zero* 0.0d0)
(defvar *format-nonfinite-zero-single* 0.0f0)

(defun %format-nonfinite-values ()
  (let ((z *format-nonfinite-zero*)
        (zs *format-nonfinite-zero-single*))
    (list (/ 1.0d0 z) (/ -1.0d0 z) (/ z z)
          (/ 1.0f0 zs) (/ -1.0f0 zs) (/ zs zs))))

(deftest format-nonfinite.fixed
  (let ((v (%format-nonfinite-values)))
    (list (format nil "~F" (nth 0 v))
          (format nil "~,2F" (nth 1 v))
          (format nil "~@F" (nth 0 v))
          (format nil "~0F" (nth 0 v))
          (format nil "~10,2F" (nth 2 v))
          (format nil "~F" (nth 3 v))))
  ("#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-NEGATIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-NAN"
   "#.DOTCL:SINGLE-FLOAT-POSITIVE-INFINITY"))

(deftest format-nonfinite.exponential
  (let ((v (%format-nonfinite-values)))
    (list (format nil "~E" (nth 0 v))
          (format nil "~,2E" (nth 1 v))
          (format nil "~@E" (nth 0 v))
          (format nil "~10E" (nth 2 v))
          (format nil "~E" (nth 3 v))
          (format nil "~E" (nth 5 v))))
  ("#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-NEGATIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-NAN"
   "#.DOTCL:SINGLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:SINGLE-FLOAT-NAN"))

(deftest format-nonfinite.general
  (let ((v (%format-nonfinite-values)))
    (list (format nil "~G" (nth 0 v))
          (format nil "~,2G" (nth 1 v))
          (format nil "~@G" (nth 0 v))
          (format nil "~10G" (nth 2 v))
          (format nil "~G" (nth 4 v))))
  ("#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-NEGATIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-NAN"
   "#.DOTCL:SINGLE-FLOAT-NEGATIVE-INFINITY"))

(deftest format-nonfinite.dollar
  (let ((v (%format-nonfinite-values)))
    (list (format nil "~$" (nth 0 v))
          (format nil "~,0$" (nth 1 v))
          (format nil "~5,2,0$" (nth 0 v))
          (format nil "~2,1,40,'?$" (nth 0 v))
          (format nil "~$" (nth 2 v))
          (format nil "~5,2:$" (nth 0 v))
          (format nil "~5,2@$" (nth 0 v))))
  ("#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-NEGATIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "??#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-NAN"
   "#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"))

(deftest format-nonfinite.width-and-overflow
  (let ((v (%format-nonfinite-values)))
    (list (format nil "~10,2,0,'*F" (nth 0 v))
          (format nil "~40,2,0,'*,'?F" (nth 0 v))
          (format nil "~30,2,0,'*F" (nth 2 v))
          (format nil "~3F" (nth 0 v))
          (format nil "~10,2,2,1,'*E" (nth 0 v))
          (format nil "~10,2,2,1,'*G" (nth 0 v))))
  ("**********"
   "??#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "      #.DOTCL:DOUBLE-FLOAT-NAN"
   "#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "**********"
   "**********"))

(deftest format-nonfinite.read-eval-nil
  (let ((v (%format-nonfinite-values)))
    (let ((*read-eval* nil))
      (list (format nil "~F" (nth 0 v))
            (format nil "~E" (nth 1 v))
            (format nil "~G" (nth 2 v))
            (format nil "~$" (nth 0 v))
            (format nil "~F" (nth 2 v)))))
  ("#<DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY>"
   "#<DOTCL:DOUBLE-FLOAT-NEGATIVE-INFINITY>"
   "#<DOTCL:DOUBLE-FLOAT-NAN>"
   "#<DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY>"
   "#<DOTCL:DOUBLE-FLOAT-NAN>"))

(defun %format-nonfinite-readably (control value)
  (handler-case (progn (format nil control value) :no-error)
    (print-not-readable () :print-not-readable)
    (error (e) (list :other (type-of e)))))

(deftest format-nonfinite.print-readably-signals
  (let ((v (%format-nonfinite-values)))
    (let ((*read-eval* nil) (*print-readably* t))
      (list (%format-nonfinite-readably "~F" (nth 0 v))
            (%format-nonfinite-readably "~E" (nth 0 v))
            (%format-nonfinite-readably "~G" (nth 0 v))
            (%format-nonfinite-readably "~$" (nth 0 v)))))
  (:print-not-readable :print-not-readable :print-not-readable :print-not-readable))

(deftest format-nonfinite.printer-unchanged
  (let ((v (%format-nonfinite-values)))
    (list (prin1-to-string (nth 0 v))
          (princ-to-string (nth 2 v))
          (format nil "~A" (nth 0 v))
          (format nil "~S" (nth 3 v))))
  ("#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:DOUBLE-FLOAT-NAN"
   "#.DOTCL:DOUBLE-FLOAT-POSITIVE-INFINITY"
   "#.DOTCL:SINGLE-FLOAT-POSITIVE-INFINITY"))
