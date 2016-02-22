;; =============================================================================
;;
;;  main.ss - a pretty-printer for PLT Scheme
;;  Copyright (C) 2005 - 2008 David Herman
;;
;;  Portions based on PPrint.hs - a pretty-printer for Haskell
;;  Copyright 2000, Daan Leijen. All rights reserved.
;;  See COPYING.HASKELL for accompanying license.
;;
;;  Portions based on pprint.m - a pretty-printer for Mercury
;;  Copyright (C) 2000-2002 The University of Melbourne
;;  Written by Ralph Becket
;;  See COPYING for accompanying license.
;;
;;  This library is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU Lesser General Public License as published by
;;  the Free Software Foundation; either version 2.1 of the License, or (at
;;  your option) any later version.
;;
;;  This library is distributed in the hope that it will be useful, but WITHOUT
;;  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;  License for more details.
;;
;;  You should have received a copy of the GNU Lesser General Public License
;;  along with this library; if not, write to the Free Software Foundation,
;;  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;; =============================================================================

#lang racket

(require dherman-struct/datatype
         racket/contract
         (except-in racket/list empty flatten))

  ;; ===========================================================================
  ;; Primitives
  ;; ===========================================================================

(define (write-doc v port write?)
  (fprintf port "#<struct:doc>"))

(define-datatype (doc ([prop:custom-write write-doc]))
  [NIL     ()]
  [CAT     (left right)]
  [NEST    (depth doc)]
  [LABEL   (label doc)]
  [MARKUP  (f doc)]
  [TEXT    (text)]
  [LINE    (break?)]
  [GROUP   (doc)]
  [COLUMN  (f)]
  [NESTING (f)])

(define (doc->string doc)
  (match doc
    [(struct NIL ()) "NIL"]
    [(struct CAT (x y)) (format "(CAT ~a ~a)" (doc->string x) (doc->string y))]
    [(struct NEST (n x)) (format "(NEST ~a ~a)" n (doc->string x))]
    [(struct LABEL (l x)) (format "(LABEL ~v ~a)" l (doc->string x))]
    [(struct MARKUP (f x)) (format "(MARKUP ~a ~a)" f (doc->string x))]
    [(struct LINE (break?)) (format "(LINE ~a)" break?)]
    [(struct GROUP (x)) (format "(GROUP ~a)" (doc->string x))]
    [(struct TEXT (t)) (format "~v" t)]
    [(struct COLUMN (f)) (format "(COLUMN ~a)" f)]
    [(struct NESTING (f)) (format "(NESTING ~a)" f)]))

(define-datatype simple-doc
  [SEMPTY ()]
  [STEXT (text rest)]
  [SPUSH (f rest)]
  [SPOP (rest)]
  [SLINE (is rest)])

(define empty         (make-NIL))
(define (nest i x)    (make-NEST i x))
(define (text s)      (make-TEXT s))
(define (label l d)   (make-LABEL l d))
(define (markup f d)  (make-MARKUP f d))
(define (column f)    (make-COLUMN f))
(define (nesting f)   (make-NESTING f))
(define (group x)     (make-GROUP x))
(define (char c)      (if (char=? c #\newline) line (text (string c))))

(define line          (make-LINE #f))
(define break         (make-LINE #t))
(define soft-line     (group line))
(define soft-break    (group break))

;; ===========================================================================
;; Semi-primitives
;; ===========================================================================

(define (fill/break f x)
  (width x (lambda (w)
             (if (> w f)
                 (nest f break)
                 (text (spaces (- f w)))))))
(define (fill f d)
  (width d (lambda (w)
             (if (>= w f)
                 empty
                 (text (spaces (- f w)))))))
(define (width d f)
  (column (lambda (k1)
            (h-append d (column (lambda (k2)
                                  (f (- k2 k1))))))))
(define (indent i d)
  (hang i (h-append (text (spaces i)) d)))
(define (hang i d)
  (align (nest i d)))
(define (align d)
  (column (lambda (k)
            (nesting (lambda (i)
                       (nest (- k i) d))))))

;; ===========================================================================
;; High-level combinators
;; ===========================================================================

(define comma     (char #\,))
(define semi      (char #\;))
(define colon     (char #\:))
(define lparen    (char #\())
(define rparen    (char #\)))
(define lbracket  (char #\[))
(define rbracket  (char #\]))
(define langle    (char #\<))
(define rangle    (char #\>))
(define lbrace    (char #\{))
(define rbrace    (char #\}))
(define space     (char #\space))
(define ellipsis  (text "..."))
(define squote    (char #\'))
(define dquote    (char #\"))
(define dot       (char #\.))
(define backslash (char #\\))
(define equals    (char #\=))

(define (foldr1 f xs)
  (match xs
    [(list x) x]
    [(list x xs ...) (f x (foldr1 f xs))]))

(define (fold f ds)
  (if (null? ds)
      empty
      (foldr1 f ds)))

(define (cat-with sep)
  (letrec ([f (match-lambda
                [(list) empty]
                [(list x) x]
                [(list x y) (h-append x sep y)]
                [(list d ds ...) (h-append d sep (f ds))])])
    (lambda ds
      (f ds))))

(define h-append
  (letrec ([f (match-lambda
                [(list) empty]
                [(list x) x]
                [(list x y) (make-CAT x y)]
                [(list d ds ...) (make-CAT d (f ds))])])
    (lambda ds
      (f ds))))

(define hs-append     (cat-with space))
(define v-append      (cat-with line))
(define vs-append     (cat-with soft-line))
(define vb-append     (cat-with break))
(define vsb-append    (cat-with soft-break))

(define hs-concat     (lambda (ds) (fold hs-append ds)))
(define v-concat      (lambda (ds) (fold v-append ds)))
(define vs-concat     (lambda (ds) (fold vs-append ds)))
(define v-concat/s    (compose group v-concat))

(define h-concat      (lambda (ds) (fold h-append ds)))
(define vb-concat     (lambda (ds) (fold vb-append ds)))
(define vsb-concat    (lambda (ds) (fold vsb-append ds)))
(define vb-concat/s   (compose group vb-concat))

(define (next-newline s i)
  (if (or (>= i (string-length s))
          (char=? (string-ref s i) #\newline))
      i
      (next-newline s (add1 i))))

(define (split-newlines s)
  (let ([len (string-length s)])
    (let f ([start 0])
      (cond
        [(>= start len)
         null]
        [(char=? (string-ref s start) #\newline)
         (cons "\n" (f (add1 start)))]
        [else (let ([end (next-newline s start)])
                (if (= end len)
                    (list (substring s start))
                    (cons (substring s start end)
                          (cons "\n" (f (add1 end))))))]))))

(define (string->doc s)
  (foldr h-append
         empty
         (map (lambda (s)
                (if (string=? "\n" s) line (text s)))
              (split-newlines s))))

(define (value->doc x)
  (string->doc (format "~a" x)))

;; (punctuate p (list d1 d2 ... dn)) => (list (<> d1 p) (<> d2 p) ... dn)

(define (apply-infix p ds)
  (match ds
    [(list) null]
    [(list d) (list d)]
    [(list d ds ...) (cons (h-append d p) (apply-infix p ds))]))

(define (spaces n)
  (build-string n (lambda (i) #\space)))

(define (extend s n)
  (string-append s (spaces n)))

;; flatten : doc -> doc
(define (flatten doc)
  (match doc
    [(struct CAT (x y)) (make-CAT (flatten x) (flatten y))]
    [(struct NEST (n x)) (flatten x)]
    [(struct LABEL (l x)) (flatten x)]
    [(struct MARKUP (f x)) (make-MARKUP f (flatten x))]
    [(struct LINE (#t)) (make-NIL)]
    [(struct LINE (#f)) (make-TEXT " ")]
    [(struct GROUP (x)) (flatten x)]
    [(struct COLUMN (f)) (make-COLUMN (compose flatten f))]
    [(struct NESTING (f)) (make-NESTING (compose flatten f))]
    [_ doc]))

;; NOTE: If you want to add ribbon-width, you'll need an extra accumulator
;;       is0, which represents the current line's indentation (as opposed
;;       to the nesting level, which represents the indentation of future
;;       lines). I don't think I want to bother, though, because it seems
;;       like a pretty esoteric feature, and doesn't really make sense in
;;       the context of the `label' combinator.

(define-values (backtrack! backtrack?)
  (let ()
    (define-struct backtrack ())
    (values (make-backtrack) backtrack?)))

;(define backtrack!
;  (let-struct backtrack ()
;    (make-backtrack)))
;
;(define (backtrack? x)
;  (eq? x backtrack!))

(define (too-big? text col width)
  (> (+ col (string-length text)) width))

;; layout : (or nat #f) * doc -> simple-doc
(define (layout width doc)
  ;; best : nat * (listof (cons string doc)) * boolean -> simple-doc
  (let best ([col 0] [docs (list (cons "" doc))] [alternate? #f])
    (match docs
      [(list) (make-SEMPTY)]
      [(list #f docs* ...)
       (make-SPOP (best col docs* alternate?))]
      [(list (cons is (struct NIL ())) docs* ...)
       (best col docs* alternate?)]
      [(list (cons is (struct CAT (x y))) docs* ...)
       (best col (cons (cons is x)
                       (cons (cons is y) docs*)) alternate?)]
      [(list (cons is (struct NEST (n x))) docs* ...)
       (best col (cons (cons (extend is n) x) docs*) alternate?)]
      [(list (cons is (struct LABEL (l x))) docs* ...)
       (best col (cons (cons (string-append is l) x) docs*) alternate?)]
      [(list (cons is (struct MARKUP (f x))) docs* ...)
       (make-SPUSH f (best col (cons (cons is x) (cons #f docs*)) alternate?))]
      [(list (cons is (struct LINE (_))) docs* ...)
       (make-SLINE is (best (string-length is) docs* alternate?))]
      [(list (cons is (struct GROUP (x))) docs* ...)
       (with-handlers ([backtrack? (lambda (exn)
                                     (best col (cons (cons is x) docs*) alternate?))])
         (best col (cons (cons is (flatten x)) docs*) #t))]
      [(list (cons is (struct TEXT (t))) docs* ...)
       (if (and width alternate? (too-big? t col width))
           (raise backtrack!)
           (make-STEXT t (best (+ col (string-length t)) docs* alternate?)))]
      [(list (cons is (struct COLUMN (f))) docs* ...)
       (best col (cons (cons is (f col)) docs*) alternate?)]
      [(list (cons is (struct NESTING (f))) docs* ...)
       (best col (cons (cons is (f (string-length is))) docs*) alternate?)])))

;; ===========================================================================
;; Front-end and utilities
;; ===========================================================================

(define current-page-width (make-parameter 80))

(define (pretty-print doc [port (current-output-port)] [width (current-page-width)])
  (let print ([sdoc (layout width doc)])
    (match sdoc
      [(struct SEMPTY ()) (void)]
      [(struct STEXT (t rest))
       (display t port)
       (print rest)]
      ;; Discard markup for ordinary printing.
      [(struct SPUSH (f rest))
       (print rest)]
      [(struct SPOP (rest))
       (print rest)]
      [(struct SLINE (is rest))
       (newline port)
       (display is port)
       (print rest)])))

(define (pretty-format doc [width (current-page-width)])
  (let ([out (open-output-string)])
    (pretty-print doc out width)
    (get-output-string out)))

;; This is sorta like the exception monad, except:
;;
;; a) it accumulates (tupling) return values instead of choosing (summation), and
;; b) it generalizes this accumulation to an unbounded stack of return values.
;;
;; So every time you reach a PUSH node, you expect one more return value,
;; and every time you reach a POP node, you return an extra value.
;;
;; The idea is that we fork a separate space in which to return pretty printed
;; text every time we reach a PUSH node, so that we can format just that portion,
;; but we still want the rest of the document, which we combine with the marked-up
;; text.

;; pretty-markup : (doc a) ((union string a) (union string a) -> (union string a)) [nat] -> (union string a)
(define (pretty-markup doc combine [width (current-page-width)])
  (car (let markup ([sdoc (layout width doc)])
         (match sdoc
           [(struct SEMPTY ()) (list "")]
           [(struct STEXT (t rest))
            (let ([r (markup rest)])
              (cons (combine t (car r))
                    (cdr r)))]
           [(struct SPUSH (f rest))
            (let ([r (markup rest)])
              (cons (combine (f (car r)) (cadr r))
                    (cddr r)))]
           [(struct SPOP (rest))
            (cons "" (markup rest))]
           [(struct SLINE (is rest))
            (let ([r (markup rest)])
              (cons (combine (string-append "\n" is)
                             (car r))
                    (cdr r)))]))))

;; Front end:
(provide/contract [pretty-print ((doc?) (output-port? (or/c natural-number/c #f)) . ->* . any)]
                  [pretty-format ((doc?) ((or/c natural-number/c #f)) . ->* . string?)]
                  [pretty-markup ((doc? (any/c any/c . -> . any)) ((or/c natural-number/c #f)) . ->* . any)]
                  [current-page-width (parameter/c (or/c natural-number/c #f))])
(provide/contract [doc? (any/c . -> . boolean?)]
                  [string->doc (string? . -> . doc?)]
                  [value->doc  (any/c . -> . doc?)])

;; Basic combinators:
(provide/contract [empty doc?]
                  [char (char? . -> . doc?)]
                  [text (string? . -> . doc?)]
                  [nest (natural-number/c doc? . -> . doc?)]
                  [label (string? doc? . -> . doc?)]
                  [markup (procedure? doc? . -> . doc?)]
                  [group (doc? . -> . doc?)]
                  [line doc?]
                  [break doc?]
                  [soft-line doc?]
                  [soft-break doc?])

;; Alignment:
(provide/contract [align (doc? . -> . doc?)]
                  [hang (natural-number/c doc? . -> . doc?)]
                  [indent (natural-number/c doc? . -> . doc?)])

;; Operators:
(provide/contract [h-append   (() () #:rest (listof doc?) . ->* . doc?)]
                  [hs-append  (() () #:rest (listof doc?) . ->* . doc?)]
                  [vs-append  (() () #:rest (listof doc?) . ->* . doc?)]
                  [vsb-append (() () #:rest (listof doc?) . ->* . doc?)]
                  [v-append   (() () #:rest (listof doc?) . ->* . doc?)]
                  [vb-append  (() () #:rest (listof doc?) . ->* . doc?)])

;; List combinators:
(provide/contract [v-concat/s   ((listof doc?) . -> . doc?)]
                  [vs-concat    ((listof doc?) . -> . doc?)]
                  [hs-concat    ((listof doc?) . -> . doc?)]
                  [v-concat     ((listof doc?) . -> . doc?)])
(provide/contract [vb-concat/s  ((listof doc?) . -> . doc?)]
                  [vsb-concat   ((listof doc?) . -> . doc?)]
                  [h-concat     ((listof doc?) . -> . doc?)]
                  [vb-concat    ((listof doc?) . -> . doc?)])
(provide/contract [apply-infix  (doc? (listof doc?) . -> . (listof doc?))])

;; Fillers:
(provide/contract [fill (natural-number/c doc? . -> . doc?)]
                  [fill/break (natural-number/c doc? . -> . doc?)])

;; Constants:
(provide/contract [lparen doc?]
                  [rparen doc?]
                  [lbrace doc?]
                  [rbrace doc?]
                  [lbracket doc?]
                  [rbracket doc?]
                  [langle doc?]
                  [rangle doc?]
                  [squote doc?]
                  [dquote doc?]
                  [semi doc?]
                  [colon doc?]
                  [comma doc?]
                  [space doc?]
                  [dot doc?]
                  [backslash doc?]
                  [equals doc?]
                  [ellipsis doc?])
