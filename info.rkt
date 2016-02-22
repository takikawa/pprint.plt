#lang info
(define name "pprint")
(define blurb
  (list "A universal pretty-printing library."))
(define primary-file "main.rkt")
(define scribblings '(("pprint.scrbl" ())))
(define categories '(datastructures io))
(define repositories '("4.x"))
(define required-core-version "4.0.0.0")
(define release-notes
  (list '(ul
          (li "Page width may now also be " (tt "#f") ", indicating unbounded page width.")
          (li "Made the contract of " (tt "current-page-width") " more precise."))))
(define version "2")

(define deps '("base" "dherman-struct"))
(define build-deps '("rackunit-lib"))
