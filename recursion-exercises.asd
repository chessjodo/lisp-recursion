;;;; recursion-exercises.asd
;;;;
;;;; Copyright (c) 2022 Breanndán Ó Nualláin <o@uva.nl>

(defsystem :recursion-exercises
  :description "Recursion exercises in Common Lisp"
  :author "Breanndán Ó Nualláin <o@uva.nl>"
  :license "Lisp Lesser General Public License"
  :serial t
  :components ((:file "package")
               (:file "exercises"))
  :in-order-to ((test-op (test-op :recursion-exercises/test))))

(defsystem :recursion-exercises/test
  :description "Tests for the RECURSION-EXERCISES packages"
  :author "Breanndán Ó Nualláin <o@uva.nl>"
  :license "Lisp Lesser General Public License"
  :depends-on (:fiveam :recursion-exercises)
  :components ((:file "tests"))
  :perform (test-op (op c) (symbol-call :fiveam :run! :tests)))
