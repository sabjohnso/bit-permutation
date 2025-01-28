#lang scribble/doc

@begin[
  (require
    (for-label (except-in racket #%app) spork)
    racket racket/sandbox
    scribble/eval (except-in scribble/manual link))

  (define env-eval
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-memory-limit 50])
      (make-evaluator 'racket #:requires '(bit-permutation bit-permutation/visualization bit-permutation/bits))))]

@title{Bit Permutation Reference Manual}
@defmodule[bit-permutation]
@;;
@;; ... bits
@;;
@section{Bits}
@subsection{Bits}

@defproc[(bits [size exact-positive-intger?] [value natural-number/c]) bits?]
@defproc[(bits? [v any/c]) boolean?]
@defproc[(bits-size [v bits?]) exact-positive-intger?]
@defproc[(bits-value [v bits?]) natural-number/c]

@subsection{Function for Bits}
@defproc[(bits-ref [bs bits?] [index natural-number/c]) bit/c]
@defproc[(bits-set [bs bits?] [index natural-number/c] [b bit/c]) bits?]
@defproc[(bits-zeros [size exact-positive-integer?]) bits?]
@defproc[(bits-ones [size exact-positive-integer?]) bits?]
@defproc[(bits [size exact-positive-integer?] [index natural-number/c]) bits?]
@defproc[(bits-and [u bits?] [v bits?]) bits?]
@defproc[(bits-ior [u bits?] [v bits?]) bits?]
@defproc[(bits-xor [u bits?] [v bits?]) bits?]
@defproc[(bits-not [v bits?]) bits?]
@defproc[(bits-shift [bs bits?] [offset exact-integer?]) bits?]
@defproc[(bits-mirror [bs bits?]) bits?]
@defproc[(bits-duplicate [u bits?]) bits?]
@defproc[(bits-count-leading-zeros [bs bits?]) natural-number/c]
@defproc[(bits-count-trailing-zeros [bs bits?]) natural-number/c]
@defproc[(bits-empty? [bs bits?]) boolean?]
@defproc[(bits-active-indices [bs bits?]) (listof natural-number/c)]

@;;
@;; ... Bounded Permutations
@;;
@section{Bounded Permutations}

@; Masks
@subsection{Masks}
@defproc[(mask [v bits?]) (and/c perm? mask?)]
@defproc[(mask? [v any/c]) boolean?]
@defproc[(mask-bits [v mask?]) bits?]

@; Preshift
@subsection{Preshift Permutations}
@defproc[(preshift [p perm?] [offset exact-integer?]) (and/c perm? preshift?)]
@defproc[(preshift? [v any/c]) boolean?]
@defproc[(preshift-perm [v preshift?]) perm?]
@defproc[(preshift-offset [v preshift?]) exact-integer?]

@; Postshift
@subsection{Postshift Permutations}
@defproc[(postshift [p perm?] [offset exact-integer?]) (and/c perm? postshift?)]
@defproc[(postshift? [v any/c]) boolean?]
@defproc[(postshift-perm [v postshift?]) perm?]
@defproc[(postshift-offset [v postshift?]) exact-integer?]

@; Split
@subsection{Split Permutations}
@defproc[(split [size exact-positive-integer?] [perms (listof perm?)]) (and/c perm? split?)]
@defproc[(split? [v any/c]) boolean?]
@defproc[(split-size [v split?]) exact-positive-integer?]
@defproc[(split-perms [v split?]) (listof perm?)]

@; Sequence
@subsection{Sequence Permutations}
@defproc[(seq [size exact-posivite-integer?] [perms (listof perm?)]) (and/c perm? seq?)]
@defproc[(seq? [v any/c]) boolean?]
@defproc[(seq-size [v seq?]) exact-positive-intger?]
@defproc[(seq-perms [v seq?]) (listof perm?)]

@; Functions
@subsection{Permutation Functions}
@defproc[(perm? [v any/c]) boolean?]
@defproc[(perm-size [v perm?]) exact-positive-integer?]
@defproc[(domain [v perm?]) bits?]
@defproc[(codomain [v perm?]) bits?]
@defproc[(perm-duplicate [v perm?]) perm?]
@defproc[(perm-invert [v perm?]) perm?]
@defproc[(perm-disjoint? [v perm?] ...) boolean?]
@defproc[(perm-inver [v perm?]) perm?]
@defproc[(perm-mirror [v perm?]) perm?]
@defproc[(perm-zip [v perm?]) perm?]
@defproc[(perm=? [u perm?] [v perm?]) boolean?]
@defproc[(perm-inverse=? [u perm?] [v perm?]) boolean?]
@defproc[(perm-morror=? [u perm?] [v perm?]) boolean?]
@defproc[(perm-inverse-mirror=? [u perm?] [v perm?]) boolean?]
@defproc[(perm-symmetric? [v perm?]) boolean?]
@defproc[(disjoint? [v perm?] ...) boolean?]
@defproc[(perm+ [u perm?] [v perm?] ...) (and/c perm? split?)]
@defproc[(perm-> [u perm?] [v perm?] ...) (and/c perm? seq?)]


@;
@; ... Visualization
@;
@section{Visualization}
@defmodule[bit-permutation/visualization]
