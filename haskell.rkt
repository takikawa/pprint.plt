;; =============================================================================
;;
;;  haskell.ss - compatibility library for the Haskell version
;;  Copyright (C) 2005 - 2008 David Herman
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

#lang racket/base
(require "main.rkt")

(provide empty char text nest group)

(provide line
         (rename-out [break      linebreak]
                     [soft-line  softline]
                     [soft-break softbreak]))

(provide (rename-out [h-append   <>]
                     [hs-append  <+>]
                     [v-append   <$>]
                     [vs-append  </>]
                     [vb-append  <$$>]
                     [vsb-append <//>]))

(provide (rename-out [h-concat    hcat]
                     [hs-concat   hsep]
                     [v-concat    vsep]
                     [vs-concat   fill-sep]
                     [v-concat/s  sep]
                     [vb-concat   vcat]
                     [vsb-concat  fill-cat]
                     [vb-concat/s cat]
                     [apply-infix punctuate]))

(provide fill (rename-out [fill/break fill-break]))

(provide align hang indent)

(provide lparen rparen lbrace rbrace lbracket rbracket langle rangle squote dquote
         semi colon comma space dot backslash equals ellipsis)
