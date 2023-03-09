#lang info
(define name "pprint")
(define blurb
  (list "A universal pretty-printing library."))
(define scribblings '(("pprint.scrbl" ())))
(define categories '(datastructures io))
(define release-notes
  (list '(ul
          (li "Page width may now also be " (tt "#f") ", indicating unbounded page width.")
          (li "Made the contract of " (tt "current-page-width") " more precise."))))
(define version "2.0")

(define deps '("base" "dherman-struct" "rackunit-lib"))
(define build-deps '("racket-doc" "scribble-lib"))
