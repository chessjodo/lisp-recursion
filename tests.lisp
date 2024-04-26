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

(test test-double-list
      (is (equalp '() (double-list '())))
      (is (equalp '(0) (double-list '(0))))
      (is (equalp '(2 4 6) (double-list '(1 2 3))))
      (is (equalp '(2 2) (double-list '(1 1))))
      (is (equalp '(6 2) (double-list '(3 1))))
      (is (equalp '(2 2 2 2 8 4) (double-list '(1 1 1 1 4 2)))))

(test test-largest-list
      (is (= 0 (largest-list '(0))))
      (is (= 3 (largest-list '(1 2 3))))
      (is (= 3 (largest-list '(3 2 1))))
      (is (= 3 (largest-list '(2 3 3))))
      (is (= 5 (largest-list '(2 3 4 4 5)))))

(test test-pos-list
      (is (equalp '() (pos-list '())))
      (is (equalp '(1 2 3) (pos-list '(1 2 3))))
      (is (equalp '(1 2 3) (pos-list '(-1 1 2 3))))
      (is (equalp '(1 2 3) (pos-list '(1 -1 2 3))))
      (is (equalp '(1 2 3) (pos-list '(1 2 -3 3 -2))))
      (is (equalp '() (pos-list '(-1))))
      (is (equalp '(0 1) (pos-list '(0 1))))
      )
