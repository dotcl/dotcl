;;; ASINH and ASIN of a complex number with a huge magnitude no longer overflow (the formula
;;; squared the argument): (asinh #C(1d300 1d300)) is about #C(691.8 0.785). Ordinary arguments
;;; are unchanged.

(defun %cal-near (z re im)
  (and (< (abs (- (realpart z) re)) 1d-6) (< (abs (- (imagpart z) im)) 1d-6)))

(deftest complex-asinh-large.huge
  (list (%cal-near (asinh (complex 1d300 1d300)) 691.8152486690535d0 0.7853981633974483d0)
        (%cal-near (asin (complex 1d300 1d300)) 0.7853981633974483d0 691.8152486690535d0)
        (%cal-near (asinh (complex -1d300 1d300)) -691.8152486690535d0 0.7853981633974483d0)
        (%cal-near (asinh (complex 1d300 -1d300)) 691.8152486690535d0 -0.7853981633974483d0))
  (t t t t))

(deftest complex-asinh-large.ordinary
  (list (%cal-near (asinh (complex 1d0 1d0)) 1.0612750619050357d0 0.6662394324925153d0)
        (%cal-near (asin (complex 2d0 3d0)) 0.5706527843210976d0 1.9833870299165366d0)
        (%cal-near (asinh (complex 0d0 2d0)) 1.3169578969248166d0 1.5707963267948966d0))
  (t t t))
