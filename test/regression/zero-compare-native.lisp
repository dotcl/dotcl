;;; (= X 0) as a branch test compares X raw when X is statically a fixnum or a
;;; float. The zero test had a clause of its own ahead of the typed comparison
;;; paths, and that clause called the zerop entry, which takes an object: an X
;;; living in an Int64 or double slot was boxed only to be compared with zero.
;;; In the typed body of a function with a declaimed FTYPE every step of a
;;; recursion on N tested (= N 0) this way, one Fixnum per step once N left the
;;; small-integer cache.
;;;
;;; What the tests pin: the shape (no boxing, no zerop call for a fixnum or a
;;; double X, the zerop call kept for an X of unknown type) and the answers,
;;; including the float zeros of both signs and a NaN.

(setf dotcl:*save-sil* t)

(defun %zcn-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %zcn-has (needle fn) (and (search needle (%zcn-sil fn)) t))

(defun %zcn-fx (n) (declare (fixnum n)) (if (= n 0) :zero :other))
(defun %zcn-fx-rev (n) (declare (fixnum n)) (if (= 0 n) :zero :other))
(defun %zcn-df (x) (declare (double-float x)) (if (= x 0) :zero :other))
(defun %zcn-sf (x) (declare (single-float x)) (if (= 0 x) :zero :other))
(defun %zcn-any (x) (if (= x 0) :zero :other))

(declaim (ftype (function (fixnum fixnum) fixnum) %zcn-ev %zcn-od))
(defun %zcn-ev (n acc) (if (= n 0) acc (%zcn-od (1- n) (+ acc 1))))
(defun %zcn-od (n acc) (if (= n 0) acc (%zcn-ev (1- n) (+ acc 2))))

(deftest-emitting-only zero-compare-native.shape
  (list (%zcn-has "IsTrueZerop" #'%zcn-fx)
        (%zcn-has "Fixnum.Make" #'%zcn-fx)
        (%zcn-has "IsTrueZerop" #'%zcn-fx-rev)
        (%zcn-has "IsTrueZerop" #'%zcn-df)
        (%zcn-has "IsTrueZerop" #'%zcn-sf)
        (%zcn-has "IsTrueZerop" #'%zcn-any)
        (%zcn-has "IsTrueZerop" #'%zcn-ev)
        (%zcn-has "Fixnum.Make" #'%zcn-ev))
  (nil nil nil nil nil t nil nil))

(deftest zero-compare-native.values
  (list (%zcn-fx 0) (%zcn-fx 7) (%zcn-fx most-negative-fixnum)
        (%zcn-fx-rev 0) (%zcn-fx-rev -1)
        (%zcn-df 0d0) (%zcn-df -0d0) (%zcn-df 1d-300)
        (%zcn-sf 0f0) (%zcn-sf -0f0) (%zcn-sf 1f0)
        (%zcn-any 0) (%zcn-any 0d0) (%zcn-any 1/2)
        (%zcn-ev 1001 0))
  (:zero :other :other :zero :other :zero :zero :other :zero :zero :other
   :zero :zero :other 1501))

(deftest zero-compare-native.nan
  (let ((nan (- (dotnet:static "System.Double" "PositiveInfinity")
                (dotnet:static "System.Double" "PositiveInfinity"))))
    (list (%zcn-df nan) (%zcn-any nan)))
  (:other :other))
