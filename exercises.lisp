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
(defun rep-ntimes (symbol n)
  (cond ((= 0 n) '())
        ((< 0 n) (cons symbol (rep-ntimes symbol (- n 1))))))

;;;; 2. Design a function that returns a list of squares of a given length starting from a given number.
(defun nsquares-from (length start)
  (cond ((= 0 length) '())
        ((< 0 length) (cons (* start start) (nsquares-from (- length 1) (+ start 1))))))

;;;; 3. Design take which takes a list and a counting number, n and returns the list consisting of the first n elements of the input list
(defun firstn (ns n)
  (cond ((= 0 n) '())
        ((null ns) '())
        ((< 0 n) (cons (first ns) (firstn (rest ns) (- n 1)))))
  )

;;;; 4. Design drop which takes a list and a counting number, n and returns the list gotten by dropping the first n elements of the input list.
(defun dropn (ns n)
  (cond ((= 0 n) ns)
        ((null ns) ns)
        ((< 0 n) (dropn (rest ns) (- n 1))))
  )

;;;; BINARY TREES
(deftype bt () '(or node null))

(defstruct node
  (value  nil :type fixnum)
  (left   nil :type bt)
  (right  nil :type bt))
;;;; A path is a list containing only the symbols left and right, and ending with the symbol t

;;;; 1. Design a function which takes a list of integers and returns a BST containing those integers.
(defun make-bst (ns)
  (cond ((null ns) nil)
        ((consp ns) (make-node :value (first ns)
                               :left (make-bst (list-smaller (rest ns) (first ns)))
                               :right (make-bst (list-bigger (rest ns) (first ns)))))
        ))

;;;; Takes a list and a number and returns a list with all the numbers in the list that are bigger than the number
(defun list-bigger (ns n)
  (cond ((null ns) '())
        ((consp ns)
         (if (< n (first ns)) (cons (first ns) (list-bigger (rest ns) n)) (list-bigger (rest ns) n)))))

;;;; Takes a list and a number and returns a list with all the numbers in the list that are smaller than the number
(defun list-smaller (ns n)
  (cond ((null ns) '())
        ((consp ns)
         (if (> n (first ns)) (cons (first ns) (list-smaller (rest ns) n)) (list-smaller (rest ns) n)))))

;;;; 2. Design a function which takes a number and a BT and determines whether the number is present in the BT.
(defun search-bt (bt n)
  (cond ((null bt) nil)
        ((node-p bt)
         (cond ((= (node-value bt) n) t)
               (t (or (search-bt (node-right bt) n) (search-bt (node-left bt) n))))))
  )

;;;; 3. Design a function which takes a number and a BST and determines whether the number is present in the BST.
(defun search-bst (bst n)
  (cond ((null bst) nil)
        ((node-p bst)
         (cond ((= (node-value bst) n) t)
               ((< (node-value bst) n) (search-bst (node-right bst) n))
               ((> (node-value bst) n) (search-bst (node-left bst) n)))
         )))

;;;; 4. Design a function which takes a number and a BST and returns a path
;;;;    describing the route from the root of the BST to the number if the number is present in the BST, or nil if it is not.
(defun path-ton (bst n)
  (cond ((null bst) '())
        ((node-p bst)
         (cond ((= (node-value bst) n) '(t))
               ((< (node-value bst) n) (cons 'right (path-ton (node-right bst) n)))
               ((> (node-value bst) n) (cons 'left (path-ton (node-left bst) n)))
               ))
        ))

;;;; 5. Design a function which takes a number and a BT and returns a path
;;;;    describing the route from the root of the BT to the number if the number
;;;;    is present in the BT, or nil if it is not.
(defun bt-path-ton (bt n &optional (path '()))
  (cond ((null bt) '())
        ((node-p bt)
         (cond ((= (node-value bt) n) path)

               (t (or (bt-path-ton (node-right bt) n (cons-to-end path 'right)) (bt-path-ton (node-left bt) n (cons-to-end path 'left)))))))
  )

;;;; Takes a list and an element and returns a list where the element is added to the end of that list
(defun cons-to-end (list n)
  (cond ((null list) (cons n '()))
        ((consp list) (cons (first list) (cons-to-end (rest list) n)))))

;;;; 6. Design a function which takes a path and a BT and returns the number
;;;;    found by following the path through the BT, or nil if the path is not present in the BT.
(defun follow-path (bt path)
  (cond ((null bt) nil)
        ((equalp path (cons t '())) (node-value bt))
        ((consp path)
         (cond ((equalp (first path) 'right) (follow-path (node-right bt) (rest path)))
               ((equalp (first path) 'left) (follow-path (node-left bt) (rest path)))))))

;;;; 7. Design three functions which takes a BT and returns a list of the numbers occurring in the BT
;;;; 7a. from left to right
(defun bt-ltor (bt)
  (cond ((null bt) '())
        ((node-p bt) (combine-lists (bt-ltor (node-left bt)) (cons (node-value bt) (bt-ltor (node-right bt))))))
  )

;;;; Takes two lists and combines the two into one big list
(defun combine-lists (l1 l2)
  (cond ((null l1) l2)
        ((consp l1) (cons (first l1) (combine-lists (rest l1) l2))))
  )

;;;; 7b. in depth-first search order
(defun bt-depth-first (bt)
  (cond ((null bt) '())
        ((node-p bt) (cons (node-value bt) (combine-lists (bt-depth-first (node-left bt)) (bt-depth-first (node-right bt)) ))))
  )

;;;; 7c. in breadth-first search order

;;;; S-Expressions
;;;; An atom is either a number, a symbol, a string or a Boolean.
;;;; An s-expression is either an atom or a list of atoms.
(deftype matom () '(or number symbol string boolean))
(deftype msxp () '(or matom (list matom)))

;;;; 1. Design a function which takes an s-expression and returns a list of all of the strings it contains.
(defun get-strings (sx)
  (cond ((typep sx 'matom)
         (cond ((typep sx 'string) (cons sx '()))
               (t '())))
        ((list sx) (combine-lists (get-strings (first sx)) (get-strings (rest sx))))))

;;;; 2. Design a function which takes an s-expression and returns the sum of all the numbers occurring in it.
(defun get-sum-sx (sx)
  (cond ((typep sx 'matom)
         (cond ((typep sx 'number) sx)
               (t 0)))
        ((list sx) (+ (get-sum-sx (first sx)) (get-sum-sx (rest sx)))))
  )
;;;; 3. Design a function which takes an s-expression and simplifies any arithmetic expression occurring in it.
(defun simplify-arithmetic (sx)
  (cond ((typep sx 'matom) sx)
        ((list sx)
         (let ((result (check-arithmetic sx)))
           (cond ((equalp (rest result) '(t)) (simplify-arithmetic (cons (first result) (rest (rest (rest sx))))))
                 ((equalp result sx) (cons (first sx) (simplify-arithmetic (rest sx))))))))
  )

(defun check-arithmetic (sx)
  (cond ((typep (first sx) 'number)
         (cond ((typep (eval (first (rest sx))) 'symbol)
                (cond ((check-symbol (eval (first (rest sx))))
                       (cond ((typep (first (rest (rest sx))) 'number)
                              (evaluate-arithmetic (first sx) (eval (first (rest sx))) (first (rest (rest sx)))))
                             (t sx)))
                      (t sx)))
               (t sx)))
        (t sx)))

(defun check-symbol (symb)
  (cond ((equalp '+ symb) t)
        ((equalp '- symb) t)
        ((equalp '* symb) t)
        ((equalp '/ symb) t)
        (t nil)))

(defun evaluate-arithmetic (n1 symb n2)
  (cond ((equalp '+ symb) (cons (+ n1 n2) '(t)))
        ((equalp '- symb) (cons (- n1 n2) '(t)))
        ((equalp '* symb) (cons (* n1 n2) '(t)))
        ((equalp '/ symb) (cons (/ n1 n2) '(t)))))
