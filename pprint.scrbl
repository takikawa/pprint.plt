#lang scribble/manual

@begin[(require scribble/example)
       (require (for-label racket/base pprint))]

@(define the-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require pprint))
     the-eval))

@title[#:tag "top"]{@bold{PPrint}: A Universal Pretty-Printer}

by Dave Herman (@tt{dherman at ccs dot neu dot edu})

PPrint is a library for @deftech{pretty-printing}: generating textual representations
formatted to fit as well as possible on a fixed-width device such as a text editor or
printer. While Racket provides an excellent pretty-printer for Racket code,
this package provides a more general library for pretty-printing any text.

@table-of-contents[]

@section[#:tag "started"]{Getting Started with PPrint}

To use PPrint, first @racket[require] it from the package catalog:

@racketblock[(require pprint)]

Here's a simple example of pretty-printing a fragment of code.

@examples[#:eval the-eval
          (pretty-print
           (v-append
            (nest 4 (v-append (text "while (true) {")
                              (text "f();");")
                              (nest 4 (v-append (text "if (done())")
                                                (text "exit();")))))
            (text "}")))]

Things to notice about this example:

@itemize[
    @item{The @racket[pretty-print] function takes as its argument the result of
          composing many different PPrint library functions such as @racket[v-append]
          and @racket[text].}
    @item{The @racket[v-append] function appends multiple lines of text.}
    @item{The @racket[nest] function increases the indentation level for subsequent lines.}
]

@section[#:tag "doc"]{Abstract Documents}
@declare-exporting[pprint]

Formatting text in PPrint involves creating an "abstract document" or @deftech{doc},
which encapsulates formatting information for the pretty printer. The library
functions of PPrint build and combine docs, which can then be rendered for pretty
printing (see @secref{rendering}).

@defproc[(doc? (x any)) boolean?]{Determines whether a value is a member of the @tech{doc} datatype.}

When using the @racket[markup] constructor, the @tech{doc} datatype may be thought of
as a parameterized type @italic{doc a} for arbitrary markup of type @italic{a}. See the
documentation for @racket[markup] for details.

@section[#:tag "library"]{Library Documentation}
@declare-exporting[pprint]

@subsection[#:tag "rendering"]{Rendering Documents}

@defproc[(pretty-print (d doc?) (out output-port? (current-output-port)) (width (or/c #f natural-number/c) (current-page-width))) any]{
Pretty prints the doc @racket[d] to the output @racket[out] with a maximum page width of @racket[width].
If @racket[width] is @racket[#f], the page width is considered infinite.}

@defproc[(pretty-format (d doc?) (width (or/c #f natural-number/c) (current-page-width))) string?]{
Pretty prints the doc @racket[d] to a string with a maximum page width of @racket[width].
If @racket[width] is @racket[#f], the page width is considered infinite.}

@defproc[(pretty-markup (d doc?) (combine ((or/c string? _a) (or/c string? _a) -> (or/c string? _a))) (width (or/c #f natural-number/c) (current-page-width))) (or/c string? _a)]{
Pretty prints the doc @racket[d] to an instance of type @italic{a}, which is determined by the type of
the @racket[markup] nodes in @racket[d], with a maximum page width of @racket[width].
If @racket[width] is @racket[#f], the page width is considered infinite.

The process of generating the markup relies on the ability to concatenate
strings or markup, and this concatenation is dependent on the type @italic{a}. So the
@racket[combine] argument is required in order to concatenate fragments of
marked-up text.}

@defparam[current-page-width w (or/c #f natural-number/c)]{
A parameter specifying the default maximum page width, in columns, for pretty printing.
If @racket[#f], the page width is considered infinite.}

@subsection[#:tag "basic"]{Basic Documents}

@defthing[empty doc?]{
The empty document, which contains the empty string.}

@defproc[(char (c char?)) doc?]{
Constructs a document containing the single character @racket[c].}

@defproc[(text (s string?)) doc?]{
Constructs a document containing the fixed string @racket[s].}

@defproc[(nest (n natural-number/c) (d doc?)) doc?]{
Constructs a document like @racket[d] but with the current indentation level increased by @racket[n].}

@bold{NOTE:} The @racket[nest] combinator does @italic{not} affect the current line's
indentation. Indentation is only inserted after a @racket[line] or a @racket[break].

@examples[#:eval the-eval (pretty-print (nest 4 (text "not indented")))
                          (pretty-print (nest 4 (h-append (text "not indented")
                                                          line
                                                          (text "indented"))))]

@defproc[(label (s string?) (d doc?)) doc?]{
Constructs a document like @racket[d] but with the current indentation suffixed by the string @racket[s].}

@defproc[(markup (f ((or/c string? _a) -> (or/c string? _a))) (d (_doc _a))) (_doc _a)]{
Creates a document node with a markup transformer, which is applied by
@racket[pretty-markup] to produce a pretty-printed document with markup
information. The markup is assumed not to affect the width of the
string. This allows you, for example, to produce X-expressions from
pretty-printed source.}

@examples[#:eval the-eval
  (eval:no-prompt (define (empty-xexpr? x)
                    (or (null? x) (equal? x ""))))
  (eval:no-prompt (define (combine x1 x2)
                    (cond
                      [(empty-xexpr? x1) x2]
                      [(empty-xexpr? x2) x1]
                      [else (list x1 x2)])))
  (pretty-markup (markup (λ (x) `(em ,x)) (text "hi!")) combine)]

@defproc[(group (d doc?)) doc?]{
Creates a document like @racket[d] but with all line breaks removed, if it fits on a single line.}

@defthing[line doc?]{
A document containing a line break, which is replaced with a single
space when placed in the context of a @racket[group].}

@defthing[break doc?]{
A document containing a line break, which is replaced with the empty
string when placed in the context of a @racket[group].}

@defthing[soft-line doc?]{
Equivalent to @racket[(group line)].}

@defthing[soft-break doc?]{
Equivalent to @racket[(group break)].}

@subsection[#:tag "compound"]{Compound Documents}

@defproc[(h-append (d doc?) ...) doc?]{
Concatenates documents @racket[d ...].}

@defproc[(hs-append (d doc?) ...) doc?]{
Concatenates documents @racket[d ...] with successive pairs of documents separated by @racket[space].}

@defproc[(v-append (d doc?) ...) doc?]{
Concatenates documents @racket[d ...] with successive pairs of documents separated by @racket[line].}

@defproc[(vs-append (d doc?) ...) doc?]{
Concatenates documents @racket[d ...] with successive pairs of documents separated by @racket[soft-line].}

@defproc[(vb-append (d doc?) ...) doc?]{
Concatenates documents @racket[d ...] with successive pairs of documents separated by @racket[break].}

@defproc[(vsb-append (d doc?) ...) doc?]{
Concatenates documents @racket[d ...] with successive pairs of documents separated by @racket[soft-break].}

@subsection[#:tag "lists"]{List Utilities}

@defproc[(h-concat (ds (listof doc?))) doc?]{
Concatenates documents @racket[ds].}

@defproc[(hs-concat (ds (listof doc?))) doc?]{
Concatenates documents @racket[ds] with successive pairs of documents separated by @racket[space].}

@defproc[(v-concat (ds (listof doc?))) doc?]{
Concatenates documents @racket[ds] with successive pairs of documents separated by @racket[line].}

@defproc[(vs-concat (ds (listof doc?))) doc?]{
Concatenates documents @racket[ds] with successive pairs of documents separated by @racket[soft-line].}

@defproc[(v-concat/s (ds (listof doc?))) doc?]{
Concatenates documents @racket[ds] with successive pairs of documents separated by spaces if they all
fit on one line;
otherwise concatenates them vertically. Equivalent to @racket[(group (v-concat ds))].}

@defproc[(vb-concat (ds (listof doc?))) doc?]{
Concatenates documents @racket[ds] with successive pairs of documents separated by @racket[break].}

@defproc[(vsb-concat (ds (listof doc?))) doc?]{
Concatenates documents @racket[ds] with successive pairs of documents separated by @racket[soft-break].}

@defproc[(vb-concat/s (ds (listof doc?))) doc?]{
Concatenates documents @racket[ds] if they all fit on one line;
otherwise concatenates them vertically. Equivalent to @racket[(group (vb-concat ds))].}

@defproc[(apply-infix (d doc?) (ds (listof doc?))) (listof doc?)]{
Concatenates documents @racket[ds] with successive pairs of documents separated by @racket[d].}

@subsection[#:tag "fillers"]{Fillers}

@defproc[(fill (n natural-number/c) (d doc?)) doc?]{
Creates a document like @racket[d] but with enough @racket[space]s to pad its width to @racket[n],
or no @racket[space]s if the width is already greater than or equal to @racket[n].

@examples[#:eval the-eval
          (pretty-print
           (hs-append
            (text "let")
            (align (vb-append (hs-append (fill 6 (text "empty"))
                                         (text "::")
                                         (text "Doc"))
                              (hs-append (fill 6 (text "nest"))
                                         (text "::")
                                         (text "Int -> Doc -> Doc"))
                              (hs-append (fill 6 (text "linebreak"))
                                         (text "::")
                                         (text "Doc"))))))]}

@defproc[(fill/break (n natural-number/c) (d doc?)) doc?]{
Creates a document like @racket[d] but with enough @racket[space]s to pad its width to @racket[n],
or if the width is already @racket[n] or greater, increases the nesting level by @racket[n] and
appends a @racket[line].

@examples[#:eval the-eval
          (pretty-print
           (hs-append
            (text "let")
            (align (vb-append (hs-append (fill/break 6 (text "empty"))
                                         (text "::")
                                         (text "Doc"))
                              (hs-append (fill/break 6 (text "nest"))
                                         (text "::")
                                         (text "Int -> Doc -> Doc"))
                              (hs-append (fill/break 6 (text "linebreak"))
                                         (text "::")
                                         (text "Doc"))))))]}

@subsection[#:tag "alignment"]{Context-Sensitive Alignment}

The alignment operators were introduced in @link["http://research.microsoft.com/users/daan"]{Daan Leijen}'s
@link["http://research.microsoft.com/users/daan/pprint.html"]{PPrint} library for Haskell. These
are useful in practice but more expensive than other operations. They determine
their layout relative to the current column.

@defproc[(align (d doc?)) doc?]{
Creates a document like @racket[d] but with the nesting level set to the current column.}

@defproc[(hang (n natural-number/c) (d doc?)) doc?]{
Creates a document like @racket[d] but with the nesting level set to the current column plus @racket[n].
Equivalent to @racket[(align (nest n d))].}

@defproc[(indent (n natural-number/c) (d doc?)) doc?]{
Creates a document like @racket[d] but indented by @racket[n] spaces from the current column.}

@subsection[#:tag "constants"]{Useful Constants}

@defthing[comma doc?]{@racket[(char #\,)]}
@defthing[semi doc?]{@racket[(char #\;)]}
@defthing[colon doc?]{@racket[(char #\:)]}
@defthing[lparen doc?]{@racket[(char #\()]}
@defthing[rparen doc?]{@racket[(char #\))]}
@defthing[lbracket doc?]{@racket[(char #\[)]}
@defthing[rbracket doc?]{@racket[(char #\])]}
@defthing[lbrace doc?]{@racket[(char #\{)]}
@defthing[rbrace doc?]{@racket[(char #\})]}
@defthing[langle doc?]{@racket[(char #\<)]}
@defthing[rangle doc?]{@racket[(char #\>)]}
@defthing[space doc?]{@racket[(char #\space)]}
@defthing[ellipsis doc?]{@racket[(text "...")]}
@defthing[squote doc?]{@racket[(char #\')]}
@defthing[dquote doc?]{@racket[(char #\")]}
@defthing[dot doc?]{@racket[(char #\.)]}
@defthing[backslash doc?]{@racket[(char #\\)]}
@defthing[equals doc?]{@racket[(char #\=)]}

@section[#:tag "haskell"]{Haskell Compatibility Library}
@declare-exporting[pprint/haskell]

@racketblock[(require pprint/haskell)]

For those who are more familiar with the names in the Haskell library,
this library is provided as a compatibility mode. (This might be
useful for porting existing Haskell code, for example.)

@defthing[empty doc?]{Same as @racket[empty].}
@defthing[char doc?]{Same as @racket[char].}
@defthing[text doc?]{Same as @racket[text].}
@defthing[nest (natural-number/c doc? -> doc?)]{Same as @racket[nest].}
@defthing[group (doc? -> doc?)]{Same as @racket[group].}

@defthing[line doc?]{Same as @racket[line].}
@defthing[linebreak doc?]{Same as @racket[break].}
@defthing[softline doc?]{Same as @racket[soft-line].}
@defthing[softbreak doc?]{Same as @racket[soft-break].}

@defthing[<> (doc? ... -> doc?)]{Same as @racket[h-append].}
@defthing[<+> (doc? ... -> doc?)]{Same as @racket[hs-append].}
@defthing[<$> (doc? ... -> doc?)]{Same as @racket[v-append].}
@defthing[</> (doc? ... -> doc?)]{Same as @racket[vs-append].}
@defthing[<$$> (doc? ... -> doc?)]{Same as @racket[vb-append].}
@defthing[<//> (doc? ... -> doc?)]{Same as @racket[vsb-append].}

@defthing[hcat ((listof doc?) -> doc?)]{Same as @racket[h-concat].}
@defthing[hsep ((listof doc?) -> doc?)]{Same as @racket[hs-concat].}
@defthing[vsep ((listof doc?) -> doc?)]{Same as @racket[v-concat].}
@defthing[fill-sep ((listof doc?) -> doc?)]{Same as @racket[vs-concat].}
@defthing[sep ((listof doc?) -> doc?)]{Same as @racket[v-concat/s].}
@defthing[vcat ((listof doc?) -> doc?)]{Same as @racket[vb-concat].}
@defthing[fill-cat ((listof doc?) -> doc?)]{Same as @racket[vsb-concat].}
@defthing[cat ((listof doc?) -> doc?)]{Same as @racket[vb-concat/s].}
@defthing[punctuate (doc? (listof doc?) -> doc?)]{Same as @racket[apply-infix].}

@defthing[fill (natural-number/c doc? -> doc?)]{Same as @racket[fill].}
@defthing[fill-break (natural-number/c doc? -> doc?)]{Same as @racket[fill/break].}

@defthing[align (doc? -> doc?)]{Same as @racket[align].}
@defthing[hang (natural-number/c doc? -> doc?)]{Same as @racket[hang].}
@defthing[indent (natural-number/c doc? -> doc?)]{Same as @racket[indent].}

@defthing[comma doc?]{Same as @racket[comma].}
@defthing[semi doc?]{Same as @racket[semi].}
@defthing[colon doc?]{Same as @racket[colon].}
@defthing[lparen doc?]{Same as @racket[lparen].}
@defthing[rparen doc?]{Same as @racket[rparen].}
@defthing[lbrace doc?]{Same as @racket[lbrace].}
@defthing[rbrace doc?]{Same as @racket[rbrace].}
@defthing[lbracket doc?]{Same as @racket[lbracket].}
@defthing[rbracket doc?]{Same as @racket[rbracket].}
@defthing[langle doc?]{Same as @racket[langle].}
@defthing[rangle doc?]{Same as @racket[rangle].}
@defthing[space doc?]{Same as @racket[space].}
@defthing[ellipsis doc?]{Same as @racket[ellipsis].}
@defthing[squote doc?]{Same as @racket[squote].}
@defthing[dquote doc?]{Same as @racket[dquote].}
@defthing[dot doc?]{Same as @racket[dot].}
@defthing[backslash doc?]{Same as @racket[backslash].}
@defthing[equals doc?]{Same as @racket[equals].}

@section[#:tag "design"]{Design Notes}

@subsection[#:tag "history"]{History}

Functional pretty printers have a surprisingly long and illustrious tradition in
the literature. The ancestry of this library goes something like this:
@itemize[
    @item{1995 - John Hughes publishes a paper [@elemref["Hug95"]{Hug95}]
                 on creating an algebra of "pretty documents" for the implementation
                 of a pretty-printing library.}
    @item{1997 - Simon Peyton Jones implements this as a Haskell library [@elemref["Pey97"]{Pey97}].}
    @item{1998 - Philip Wadler publishes a paper [@elemref["Wad98"]{Wad98}] improving
                 on Hughes' algebra and design.}
    @item{2001 - Daan Leijen implements this as a Haskell library [@elemref["Lei01"]{Lei01}].}
    @item{2001 - Ralph Becket ports Leijen's library to Mercury, a strict functional/logic
                 language [@elemref["Bec02"]{Bec02}].}
]{}

This library is a translation of the Haskell PPrint library, but with
help from Becket's Mercury implementation for maintaining efficiency
in a strict language.

@subsection[#:tag "mercury"]{Mercury Port}

Becket's port makes the following modifications to the Haskell
library:

@itemize[
    @item{He eliminates the @tt{UNION} constructor, since the only place union is
          really required is for the @racket[group] operation. In a strict language,
          this prevents unnecessary construction of duplicate data.}
    @item{He delays the calculation of @tt{best} and @tt{flatten} on the two arms of
          the union.}
    @item{He adds a @tt{LABEL} constructor, which allows programmers to specify
          arbitrary text for indentation, rather than just spaces.}
]

Becket further modifies the Haskell algorithm by eliminating the
@tt{SimpleDoc} datatype and directly producing output from within the
layout function, rather than first generating the intermediate
@tt{SimpleDoc}. However, this changes the behavior of the original
algorithm. The layout function in the Haskell library examines not
just the current sub-document but its entire context (i.e., the rest
of the document) in order to determine whether it fits on the current
line. The Mercury port, however, only uses the current sub-document to
make this decision.

The following example demonstrates the difference in behavior:

@examples[#:eval the-eval
          (parameterize ([current-page-width 13])
            (pretty-print
             (vs-append (text "pretty") (text "printer"))))]

With a column width less than 14 (i.e., @racket[(string-length "pretty printer")]),
the Haskell library would determine that the flattened document does not
fit, and decide to break lines. The Mercury library, however, only
looks at the soft break and chooses not to break because @racket[(text " ")] has
length 1 and therefore fits, and it subsequently overruns the length of
the line.

@subsection[#:tag "racket"]{racket Port}

I've chosen a design somewhere in between the two. The code mostly
follows the Haskell version, but I've replaced the @tt{UNION} constructor
with a @tt{GROUP} construct as in Becket's implementation. This way there
is no unnecessary duplication of data. Furthermore, the flattened
version is only computed by need when the layout function reaches a
@tt{GROUP} node, and of course the recursion on the non-flattened version
is only computed if the flattened version fails to fit.

I've also added Becket's @tt{LABEL} constructor.

@subsection[#:tag "modifications"]{Modification History}

@itemize[
    @item{2006/9/26 - Added @tt{MARKUP} constructor with @racket[markup] and @racket[pretty-markup] operations.}
    @item{2006/9/27 - The previous implementation didn't correctly prune the search space. Philip Wadler [@elemref["Wad98"]{Wad98}]
                      demonstrated examples of nested occurrences of @tt{GROUP}:

                      @racketblock[(define (test-performance n)
                                     (parameterize ([current-page-width 5])
                                       (pretty-format
                                        (let build-example ([n n])
                                          (if (= n 1)
                                              (group (v-append (text "hello")
                                                               (text (number->string n))))
                                              (group (v-append (build-example (sub1 n))
                                                               (text (number->string n)))))))
                                       (void)))]

                      This example can arbitrarily nest a bunch of @tt{GROUP} nodes where the very first
                      one encountered in the layout algorithm should discover that flattening will
                      fail (i.e., because @racket["hello"] is larger than the page width of 5 characters). In
                      the past, the layout algorithm would completely compute the layout of the
                      flattened version before calling @tt{fits?} to discover that it would fail.

                      In a lazy language, this is optimized for free: the complete recursive call to
                      layout isn't computed until it's needed, and if @tt{fits?} determines it isn't
                      needed, it gets short-circuited.

                      In an eager language, you need to perform the short-circuiting explicitly. I've
                      added an implementation of backtracking with exceptions in the layout
                      algorithm. You can test the above example and see that it performs quite well
                      now.}
    @item{2006/9/29 - Added the @tt{combine} argument to @racket[pretty-markup].}
    @item{2008/9/2 - Finally fixed the implementation of the @tt{layout} algorithm. It was
                     not trying the flattened version @italic{first}, so it was never producing
                     flattened output. Also, backtracking should happen when we reach a @tt{TEXT}
                     node that's wider than the remainder of the column, whereas the code was
                     backtracking after overrunning the column.}
]

@bibliography[#:tag "bib"
   @bib-entry[#:key "Bec02"
              #:title "pprint.m"
              #:url "http://www.cs.mu.oz.au/research/mercury/information/doc-latest/mercury_library/pprint.html"
              #:author "Ralph Becket"
              #:date "2002"]{}
   @bib-entry[#:key "Hug95"
              #:title "The Design of a Pretty-Printing Library"
              #:url "http://www.cs.chalmers.se/~rjmh/Papers/pretty.html"
              #:author "John Hughes"
              #:date "1995"]{}
   @bib-entry[#:key "Lei01"
              #:title "PPrint, a Prettier Printer"
              #:url "http://research.microsoft.com/users/daan/pprint.html"
              #:date "2001"
              #:author "Daan Leijen"]{}
   @bib-entry[#:key "Pey97"
              #:title "A Pretty-Printer Library in Haskell"
              #:date "1997"
              #:url "http://research.microsoft.com/~simonpj/downloads/pretty-printer/pretty.html"
              #:author "Simon Peyton Jones"]{}
   @bib-entry[#:key "Wad98"
              #:title "A Prettier Printer"
              #:date "1998"
              #:url "http://homepages.inf.ed.ac.uk/wadler/topics/language-design.html#prettier"
              #:author "Philip Wadler"]]{}
