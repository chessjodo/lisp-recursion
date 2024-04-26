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

(test test-sum-list
      (is (= 0 (sum-list '())))
      (is (= 6 (sum-list '(1 2 3))))
      (is (= 2 (sum-list '(1 1))))
      (is (= 4 (sum-list '(3 1))))
      (is (= 10 (sum-list '(1 1 1 1 4 2)))))
