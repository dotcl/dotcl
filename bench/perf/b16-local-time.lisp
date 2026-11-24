(load "bench/perf/pcommon.lisp")
(setf *bench-group* "16-local-time")
;; local-time: parse-timestring of an ISO-8601 string, encode-timestamp, and format-timestring.
;; Libraries to install on the runner: local-time  (perf.yml: libs=local-time)
(load-ql-system :local-time)
(defvar *iso* "2020-01-02T03:04:05.123456+09:00")
(defvar *ts* (local-time:parse-timestring *iso*))
(defvar *sink* nil)
(bench "local-time:parse-timestring" 20000 (setq *sink* (local-time:parse-timestring *iso*)))
(bench "local-time:encode-timestamp" 20000
       (setq *sink* (local-time:encode-timestamp 123456000 5 4 3 2 1 2020 :timezone local-time:+utc-zone+)))
(bench "local-time:format-timestring, iso-8601" 20000
       (setq *sink* (local-time:format-timestring nil *ts* :format local-time:+iso-8601-format+)))
