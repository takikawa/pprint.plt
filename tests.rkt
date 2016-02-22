#lang racket/base

(require rackunit
         "main.rkt")

(define-syntax pprint
  (syntax-rules ()
    [(_ width e)
     (let ([s (open-output-string)])
       (parameterize ([current-page-width width]
                      [current-output-port s])
         (pretty-print e))
       (let ([result (get-output-string s)])
;        (printf "*** RESULT: ~v~n" result)
;        (printf "~a~n~n" result)
         result))]))

(define (block header lines footer)
  (v-append (nest 4 (apply v-append header lines))
            footer))

(test-case "soft-break breaks when it doesn't fit"
  (check-equal? (pprint 10 (apply vs-append (map text '("david" "andrew" "herman"))))
                "david\nandrew\nherman"))
(test-case "hard break outside of group always breaks"
  (check-equal? (pprint 100 (v-append (text "hello") (text "world")))
                "hello\nworld"))
(test-case "nest only affects future indentation inside scope"
  (check-equal? (pprint 80 (v-append (nest 2 (v-append (text "hello") (text "world")))
                                     (text "!")))
                "hello\n  world\n!"))
(test-case "simple align example"
  (check-equal? (pprint 80 (hs-append (text "hi")
                                      (align (v-append (text "nice")
                                                       (text "world")))))
                "hi nice\n   world"))
(test-case "simple hang example"
  (check-equal? (pprint 20 (hang 4 (vs-concat
                                    (map text '("the" "hang" "combinator"
                                                      "indents" "these"
                                                      "words" "!")))))
                "the hang combinator\n    indents these\n    words !"))
(test-case "simple indent example"
  (check-equal? (pprint 80 (v-append (nest 4 (v-append (text "line 1")
                                                       (text "line 2")
                                                       (text "line 3")))
                                     (text "line 4")))
                "line 1\n    line 2\n    line 3\nline 4"))
(test-case "nesting affects future, not current indentation"
  (check-equal? (pprint 80 (v-append (nest 4 (v-append (text "line 1")
                                                       (text "line 2")
                                                       (text "line 3")))
                                     (text "line 4")))
                "line 1\n    line 2\n    line 3\nline 4"))

(test-case "moderate-sized example (for loop)"
  (check-equal? (pprint 80 (block (text "for (int i = 0; i < 10; i++) {")
                                  (list (text "print(i);")
                                        (text "print(\"hello\");")
                                        (block (text "if (i > 0) {")
                                               (list (text "print(\"not first\");"))
                                               (text "}"))
                                        (text "print(\"world\");"))
                                  (text "}")))
                #<<END
for (int i = 0; i < 10; i++) {
    print(i);
    print("hello");
    if (i > 0) {
        print("not first");
    }
    print("world");
}
END
                ))

