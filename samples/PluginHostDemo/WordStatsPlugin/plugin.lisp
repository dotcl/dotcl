;;; The face the host sees. word-stats.asd names this file as its
;;; (:dotnet-library ...) component: loading the system leaves it alone, and
;;; building the system loads it after the rest. SUMMARY, the functions it
;;; calls, and the text-util functions they call are compiled into the DLL.

(in-package :word-stats)

(dotnet:library ("WordStatsPlugin" :version "1.0.0.0")
  (:class "WordStatsPlugin.StatsPlugin" ()
    (:implements "PluginHost.IPlugin")
    (:properties ("Name" String))
    (:events ("Completed" "System.Action`1[System.String]"))
    (:ctor ()
      (dotnet:%set-invoke self "Name" "word stats (Common Lisp, ASDF system)"))
    (:methods
      (run ((input String) (progress "System.Action`1[System.String]")) :returns String
        (let* ((words (split-words input))
               (result (summary words)))
          (funcall progress (format nil "~D words" (length words)))
          (dotnet:invoke self "OnCompleted" result)
          result)))))
