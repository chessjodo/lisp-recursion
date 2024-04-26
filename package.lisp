;;;; package.lisp
;;;;
;;;; Copyright (c) 2022 Breanndán Ó Nualláin <o@uva.nl>

(defpackage :recursion-exercises
  (:use :cl)
  (:export :fact :sum-list :double-list :largest-list :pos-list :merge-list
           :rep-ntimes :nsquares-from :firstn :dropn :make-bst :list-smaller
           :list-bigger :node :bt :make-node :node-value :node-right :node-left
           :search-bt :search-bst :path-ton))
