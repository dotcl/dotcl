;;; Threads that did not come from DOTCL:MAKE-THREAD (a host's own .NET threads)
;;; that ask for their CURRENT-THREAD are entered in the thread registry. Their
;;; entries must go once they have ended, as a Lisp-made thread's do: they used
;;; to stay for the life of the process, one per host thread.

(defun host-thread-registry-count ()
  (dotcl:all-threads)                   ; drops the entries of ended threads
  (dotcl-internal::%thread-registry-count))

(defun run-host-thread (fn)
  "Run FN on a fresh .NET thread, not one MAKE-THREAD made, and wait for it."
  (let ((th (dotnet:new "System.Threading.Thread" fn)))
    (dotnet:invoke th "Start")
    (dotnet:invoke th "Join")))

(deftest host-thread-registry.ended-host-threads-leave
  (let ((before (host-thread-registry-count)))
    (dotimes (i 200)
      (run-host-thread (lambda () (dotcl:current-thread))))
    (<= (host-thread-registry-count) before))
  t)

;;; While it runs, a host thread keeps one identity and is listed.
(deftest host-thread-registry.live-host-thread-keeps-its-identity
  (let ((result nil))
    (run-host-thread
     (lambda ()
       ;; other host threads come and go, and are pruned, meanwhile
       (let ((me (dotcl:current-thread)))
         (dotimes (i 20)
           (run-host-thread (lambda () (dotcl:current-thread))))
         (setf result (list (eq me (dotcl:current-thread))
                            (and (member me (dotcl:all-threads)) t))))))
    result)
  (t t))
