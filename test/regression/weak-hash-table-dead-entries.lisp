;;; A weak hash table drops its dead entries even when nothing ever walks it.
;;; A collected key is never looked up again, and the entries were removed only
;;; by COUNT, MAPHASH or a lookup of that key, so a cache that only ever had new
;;; keys added (the interpreter's lambda-list shape cache, keyed by the lambda
;;; lists RESTART-CASE builds afresh on every expansion) kept every dead entry.

;;; Entries the table holds, dead ones included. HASH-TABLE-COUNT cannot answer
;;; this: it sweeps first.
(defun %whtde-stored (table) (dotnet:static "DotCL.LispHashTable" "StoredCount" table))

;;; Insert N entries whose key (or value) nothing else holds, collecting every
;;; 5000 so the entries are dead by the time the table could sweep them; without
;;; that, how many are still uncollected depends on the collector's budget.
(defun %whtde-fill (table n weak-key)
  (dotimes (i n)
    (when (zerop (mod i 5000)) (dotnet:static "System.GC" "Collect"))
    (if weak-key
        (setf (gethash (list i) table) (vector i i i))
        (setf (gethash i table) (list i)))))

;;; At most about twice the entries alive at the last sweep (here at most 5000
;;; between collections), not one per insert. Before, all 60,000 stayed. The
;;; emit-free build interprets the loop, so it inserts fewer; the subject is
;;; the table.
(defun %whtde-stored-after-fill (weakness weak-key)
  (let ((table (make-hash-table :test 'eql :weakness weakness))
        (n (emit-free-scale 60000 30000)))
    (%whtde-fill table n weak-key)
    (let ((stored (%whtde-stored table)))
      ;; T, or the number held so a failure shows it.
      (or (< stored 15000) stored))))

(deftest weak-hash-table-dead-entries.key
  (%whtde-stored-after-fill :key t)
  t)

(deftest weak-hash-table-dead-entries.value
  (%whtde-stored-after-fill :value nil)
  t)

;;; Live entries are all still there after the sweeps.
(deftest weak-hash-table-dead-entries.live-kept
  (let* ((table (make-hash-table :test 'eq :weakness :key))
         (keep (loop for i below 500 collect (list i))))
    (dolist (k keep) (setf (gethash k table) (car k)))
    (%whtde-fill table 20000 t)
    (list (every (lambda (k) (eql (gethash k table) (car k))) keep)
          (>= (hash-table-count table) 500)))
  (t t))
