;;; dotcl regression test runner
;;; Usage: dotnet run ... -- --asm compiler/cil-out.sil test/regression/run.lisp

(load "test/framework.lisp")

(load "test/regression/closures.lisp")
(load "test/regression/special-vars.lisp")
(load "test/regression/conditions.lisp")
(load "test/regression/clos.lisp")
(load "test/regression/sequences.lisp")
(load "test/regression/analysis.lisp")
(load "test/regression/recent-fixes.lisp")
(load "test/regression/threading.lisp")
(load "test/regression/mop.lisp")
(load "test/regression/net-class.lisp")
(load "test/regression/delegate-marshal.lisp")
(load "test/regression/call-out.lisp")
(load "test/regression/static-generic.lisp")

(do-tests-summary)
(if (= *fail-count* 0)
    (dotcl:quit 0)
    (dotcl:quit 1))
