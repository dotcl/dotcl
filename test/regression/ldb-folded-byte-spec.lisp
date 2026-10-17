;;; (LDB (BYTE S P) X) / (DPB V (BYTE S P) X) whose S and P are constant
;;; expressions, (byte 37 (- 64 37)), are open-coded like literal ones.

(setf dotcl:*save-sil* t)

(defconstant +lfbs-width+ 5)

(defun %lfbs-ldb (x) (list (ldb (byte 37 (- 64 37)) x) (ldb (byte +lfbs-width+ (- 64 +lfbs-width+)) x)))
(defun %lfbs-dpb (v x) (dpb v (byte +lfbs-width+ (- 64 +lfbs-width+)) x))

(deftest-compiled-only ldb-folded-byte-spec.open-coded
  (list (search "\"LDB\"" (%flat-text (dotcl:function-sil #'%lfbs-ldb)))
        (search "\"DPB\"" (%flat-text (dotcl:function-sil #'%lfbs-dpb)))
        (search "\"BYTE\"" (%flat-text (dotcl:function-sil #'%lfbs-ldb))))
  (nil nil nil))

(deftest ldb-folded-byte-spec.values
  (list (%lfbs-ldb (1- (expt 2 64)))
        (%lfbs-ldb #xDEADBEEFCAFEBABE)
        (%lfbs-ldb 12345)
        (%lfbs-ldb -1)
        (%lfbs-dpb 31 0)
        (%lfbs-dpb 0 (1- (expt 2 64)))
        (%lfbs-dpb 7 -1)
        (handler-case (%lfbs-ldb 1.5) (type-error () :type-error)))
  ((137438953471 31) (119549713913 27) (0 0) (137438953471 31)
   17870283321406128128 576460752303423487 -13835058055282163713 :type-error))
