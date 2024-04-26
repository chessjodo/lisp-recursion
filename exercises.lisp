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
(defun largest-list (ns)
  (cond ((null ns) 0)
        ((consp ns)
         (let ((result (largest-list (rest ns))))
           (if (< result (first ns)) (first ns) result)))))

;;;; 4. Return the sub-list of the input list which contains only positive numbers.
(defun pos-list (ns)
  (cond ((null ns) '())
        ((> 0 (first ns)) (pos-list (rest ns)))
        ((<= 0 (first ns)) (cons (first ns) (pos-list (rest ns)))))
  )

;;;; 5. Design a merge function which consumes two lists of numbers,
;;;;    both sorted in ascending order. It produces a single sorted list of
;;;;    numbers that contains all the numbers on both inputs lists.
;;;;    A number occurs in the output as many times as it occurs on the two
;;;;    input lists together.
(defun merge-list (ans bns)
  (cond ((null ans) bns)
        ((null bns) ans)
        ((< (first ans) (first bns)) (cons (first ans) (merge-list (rest ans) bns)))
        ((>= (first ans) (first bns)) (cons (first bns) (merge-list ans (rest bns))))
        )
  )

;;;; COUNTING NUMBERS
;;;; 1. Generalise hos to a function that can produce a given number of ha's or hi's or indeed any other symbol desired.
;;;; 2. Design a function that returns a list of squares of a given length starting from a given number.
;;;; 3. Design take which takes a list and a counting number, n and returns the list consisting of the first n elements of the input list
;;;; 4. Design drop which takes a list and a counting number, n and returns the list gotten by dropping the first n elements of the input list.

;;;; BINARY TREES
;;;; 1. Design a function which takes a list of integers and returns a BST containing those integers.
;;;; 2. Design a function which takes a number and a BT and determines whether the number is present in the BT.
;;;; 3. Design a function which takes a number and a BST and determines whether the number is present in the BST.
;;;; 4. Design a function which takes a number and a BST and returns a path
;;;;    describing the route from the root of the BST to the number if the number is present in the BST, or nil if it is not.
;;;; 5. Design a function which takes a number and a BT and returns a path
;;;;    describing the route from the root of the BT to the number if the number is present in the BT, or nil if it is not.
;;;; 6. Design a function which takes a path and a BT and returns the number
;;;;    found by following the path through the BT, or nil if the path is not present in the BT.
;;;; 7. Design three functions which takes a BT and returns a list of the numbers occurring in the BT
;;;; 7a. from left to right
;;;; 7b. in depth-first search order
;;;; 7c. in breadth-first search order

;;;; S-Expressions
;;;; 1. Design a function which takes an s-expression and returns a list of all of the strings it contains.
;;;; 2. Design a function which takes an s-expression and returns the sum of all the numbers occurring in it.
;;;; 3. Design a function which takes an s-expression and simplifies any arithmetic expression occurring in it.
