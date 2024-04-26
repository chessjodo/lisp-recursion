;;;; recursion-exercises.lisp
;;;;
;;;; Copyright (c) 2022 Breanndán Ó Nualláin <o@uva.nl>

(in-package :recursion-exercises)

(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (1- n)))))

;;;; LISTS

;;;; 1. Find the sum of all numbers in the list.
(defun sum-list (ns)
  (cond ((null ns) 0)
        ((consp ns) (+ (first ns) (sum-list (rest ns))))))

;;;; 2. Return the list with elements which are double those of the input list.
(defun double-list (ns)
  (cond ((null ns) '())
        ((consp ns) (cons (* 2 (first ns)) (double-list (rest ns))))))

;;;; 3. Find the largest number in the list.
;;;; 4. Return the sub-list of the input list which contains only positive numbers.
;;;; 5. Design a merge function which consumes two lists of numbers, both sorted in ascending order. It produces a single sorted list of numbers that contains all the numbers on both inputs lists. A number occurs in the output as many times as it occurs on the two input lists together.
