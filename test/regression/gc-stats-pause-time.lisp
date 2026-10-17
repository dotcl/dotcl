;;; Element 5 of DOTCL:GC-STATS is the total time the GC has paused the
;;; process, in internal time units, as sb-ext:*gc-run-time*. Libraries
;;; (metatilities' gc-time) called System.GC.GetTotalPauseDuration through
;;; dotnet: for want of it.

(deftest gc-stats-pause-time.shape
  (let ((stats (dotcl:gc-stats)))
    (list (length stats) (typep (nth 5 stats) '(integer 0))))
  (6 t))

(deftest gc-stats-pause-time.does-not-go-back
  (let ((before (nth 5 (dotcl:gc-stats))))
    (dotcl:gc)
    (dotcl:gc)
    (<= before (nth 5 (dotcl:gc-stats))))
  t)
