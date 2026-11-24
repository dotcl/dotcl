;;; Tests of the ansi-test beyond-ansi suite that dotcl is known to fail (read in
;;; package CL-TEST by RT:DO-TESTS). 34 of them SBCL fails as well; TYPE.8,
;;; FTYPE.2 and DOTIMES.5 are dotcl only. A test that fails and is not
;;; listed here, or that is listed and passes, makes `make test-ansi-beyond` report it.
ba-test::type.8
ba-test::ftype.2
ba-test::ftype.4
ba-test::optimize.1
ba-test::optimize.3
ba-test::not.type.3
ba-test::and.type.2
ba-test::and.type.3
ba-test::or.type.2
ba-test::or.type.3
ba-test::values.type.3
ba-test::values.type.4
ba-test::values.type.5
ba-test::function.type.1
ba-test::function.type.2
ba-test::function.type.3
ba-test::function.type.4
ba-test::function.type.5
ba-test::function.type.6
ba-test::function.type.7
ba-test::function.type.8
ba-test::function.type.9
ba-test::function.type.10
ba-test::defun.6
ba-test::defun.7
ba-test::flet.31
ba-test::labels.31
ba-test::defconstant.8
ba-test::defparameter.8
ba-test::defvar.8
ba-test::psetq.8
ba-test::catch.4
ba-test::case.4
ba-test::define-modify-macro.6
ba-test::defsetf.14
ba-test::defsetf.15
ba-test::dotimes.5
