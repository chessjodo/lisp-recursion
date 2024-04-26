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
;;;; COUNTING NUMBERS TESTS
(test test-rep-ntimes
      (is (equalp '() (rep-ntimes "ho" 0)))
      (is (equalp '("ho" "ho" "ho") (rep-ntimes "ho" 3)))
      (is (equalp '("hello" "hello") (rep-ntimes "hello" 2)))
      (is (equalp '("hi" "hi" "hi" "hi" "hi") (rep-ntimes "hi" 5)))
      )

(test test-nsquares-from
      (is (equalp '() (nsquares-from 0 4)))
      (is (equalp '(1) (nsquares-from 1 1)))
      (is (equalp '(1 4 9 16) (nsquares-from 4 1)))
      (is (equalp '(9 16 25 36 49) (nsquares-from 5 3)))
      )
;;;; LIST TESTS
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
(test test-merge-list
      (is (equalp '() (merge-list '() '())))
      (is (equalp '(1 2) (merge-list '(1) '(2))))
      (is (equalp '(1 2 3) (merge-list '(1 2) '(3))))
      (is (equalp '(1 2 2 3 3 4) (merge-list '(1 2 3) '(2 3 4))))
      )
