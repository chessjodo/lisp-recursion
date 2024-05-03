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
;;;; BST TESTS
(test test-make-bst
      (is (equalp nil (make-bst '())))
      (is (equalp (make-node :value 1
                             :right (make-node :value 2
                                               :right (make-node :value 3)))
                  (make-bst '(1 2 3))))
      (is (equalp (make-node :value 2
                             :left (make-node :value 1)
                             :right (make-node :value 3))
                  (make-bst '(2 1 3))))
      (is (equalp (make-node :value 2
                             :left (make-node :value 1)
                             :right (make-node :value 4
                                               :left (make-node :value 3)
                                               :right (make-node :value 6
                                                                 :left (make-node :value 5))))
                  (make-bst '(2 4 6 3 5 1))))
      (is (equalp (make-node :value 1
                             :right (make-node :value 2
                                               :right (make-node :value 3
                                                                 :right (make-node :value 4)))) (make-bst '(1 2 3 4))))
      )

(test test-list-smaller
      (is (equalp '(3 2 1) (list-smaller '(3 4 5 2 1) 4)))
      (is (equalp '() (list-smaller '(3 4 5 2 1) 1)))
      (is (equalp '(1) (list-smaller '(1 3) 2))))

(test test-list-bigger
      (is (equalp '(5) (list-bigger '(3 4 5 2 1) 4)))
      (is (equalp '() (list-bigger '(3 4 5 2 1) 5)))
      (is (equalp '(3) (list-bigger '(1 3) 2))))

(test test-search-bt
      (is (equalp nil (search-bt (make-bst '()) 2)))
      (is (equalp t (search-bt (make-bst '(5 2 3)) 2)))
      (is (equalp nil (search-bt (make-bst '(2 1 4 3 5)) 6)))
      (is (equalp t (search-bt (make-bst '(2 1 4 3 5)) 3)))
      )

(test test-search-bst
      (is (equalp nil (search-bst (make-bst '()) 2)))
      (is (equalp t (search-bst (make-bst '(1 2 3)) 2)))
      (is (equalp nil (search-bst (make-bst '(2 1 4 3 5)) 6)))
      (is (equalp t (search-bst (make-bst '(2 1 4 3 5)) 3))))

(test test-path-ton
      (is (equalp '() (path-ton (make-bst '()) 2)))
      (is (equalp '(right t) (path-ton (make-bst '(1 2 3)) 2)))
      (is (equalp '(right right t) (path-ton (make-bst '(2 1 4 3 5)) 5)))
      (is (equalp '(right left t) (path-ton (make-bst '(2 1 4 3 5)) 3))))

(test test-bt-path-ton
      (is (equalp '() (bt-path-ton (make-bst '()) 2)))
      (is (equalp '(right) (bt-path-ton (make-node :value 1
                                                   :right (make-node :value 3)
                                                   :left (make-node :value 2)) 3)))
      (is (equalp '(right left) (bt-path-ton (make-node :value 3
                                                        :right (make-node :value 1
                                                                          :right (make-node :value 5)
                                                                          :left (make-node :value 6))
                                                        :left (make-node :value 4)) 6)))
      )
(test test-cons-to-end
      (is (equalp '(right) (cons-to-end '() 'right)))
      (is (equalp '(right left) (cons-to-end '(right) 'left))))

(test test-follow-path
      (is (= 5 (follow-path (make-bst '(2 1 3 4 5)) (path-ton (make-bst '(2 1 3 4 5)) 5))))
      (is (= 3 (follow-path (make-bst '(2 1 3)) (path-ton (make-bst '(2 1 3)) 3))))
      (is (equalp nil (follow-path (make-bst '(2 1 3)) (path-ton (make-bst '(2 1 3)) 4))))
      (is (= 3 (follow-path (make-bst '(2 4 1 5 3)) (path-ton (make-bst '(2 4 1 5 3)) 3)))))

(test test-bt-ltor
      (is (equalp '(1 2 3) (bt-ltor (make-bst '(2 1 3)))))
      (is (equalp '() (bt-ltor '())))
      )

(test test-combine-lists
      (is (equalp '(1 2 3 4 5) (combine-lists '(1 2 3) '(4 5))))
      (is (equalp '(1 2 3) (combine-lists '(1) '(2 3)))))

(test test-bt-depth-first
      (is (equalp '(2 1 3) (bt-depth-first (make-bst '(2 1 3)))))
      (is (equalp '() (bt-depth-first '())))
      (is (equalp '(4 2 1 3 6 5 7) (bt-depth-first (make-bst '(4 2 1 3 6 5 7)))))
      )

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

(test test-firstn
      (is (equalp '() (firstn '() 0)))
      (is (equalp '() (firstn '() 3)))
      (is (equalp '(1) (firstn '(1 2 3) 1)))
      (is (equalp '(3 2 4 5) (firstn '(3 2 4 5 7 6) 4)))
      )

(test test-dropn
      (is (equalp '() (dropn '() 0)))
      (is (equalp '() (dropn '() 3)))
      (is (equalp '(2 3) (dropn '(1 2 3) 1)))
      (is (equalp '(7 6) (dropn '(3 2 4 5 7 6) 4)))
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
