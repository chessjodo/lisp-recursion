;;;; recursion-exercises.lisp
;;;;
;;;; Copyright (c) 2022 Breanndán Ó Nualláin <o@uva.nl>

(in-package :recursion-exercises)

(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (1- n)))))
