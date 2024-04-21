;;;; tests.lisp
;;;;
;;;; Copyright (c) 2022 Breanndán Ó Nualláin <o@uva.nl>

(defpackage :recursion-exercises/test
  (:use :cl :recursion-exercises :fiveam))

(in-package :recursion-exercises/test)

(def-suite* :tests)

(test test-fact
  (is (= 1 (fact 0)))
  (is (= 2 (fact 2)))
  (is (= 24 (fact 4)))
  (is (= 3628800 (fact 10))))
