;;; The command-line runtime caps the generation 0 allocation budget at 64 MB
;;; (System.GC.Gen0MaxBudget in the runtime configuration). Without it an
;;; allocation-heavy program runs a young-generation collection every few
;;; megabytes; with it the same program collects about a tenth as often.

(deftest gc-gen0-budget.configured
  (let ((v (dotnet:static "System.AppContext" "GetData" "System.GC.Gen0MaxBudget")))
    (and v (<= 16777216 (parse-integer (princ-to-string v)) 134217728)))
  t)
