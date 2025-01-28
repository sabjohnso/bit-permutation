#lang racket

(provide (all-from-out (submod "." core)))
(module core racket
  (provide
   make-simple-left-expansion
   (contract-out
    [make-left-expanding-permutation make-left-expanding-permutation-signature/c]))

  (require bit-permutation/bounded bit-permutation/bits)

  (define (make-simple-left-expansion size bits/index nindices)
    (let* ([n (quotient bits/index 2)]
           [bits (bits size (sub1 (arithmetic-shift 1 n)))])
      (split size (list (mask bits) (postshift (mask (bits-shift bits n)) (* (sub1 nindices) n))))))

  (define (make-left-expanding-permutation bits/index nindices)
    (let ([size (* bits/index nindices)])
      (define (recur current-size current-bits/index accum)
        (if (> current-bits/index bits/index)
            (seq (* bits/index nindices) accum)
          (recur (twice current-size) (twice current-bits/index)
                 (cons (make-simple-left-expansion current-size current-bits/index nindices)
                       (map perm-duplicate accum)))))
     (recur (twice nindices) 2 '())))

  (define (twice x)
    (+ x x))

  (define make-left-expanding-permutation-signature/c
    (->i ([bits/index exact-positive-integer?]
          [nindices exact-positive-integer?])
         [result (bits/index nindices)
                 (perm-sizeof/c (* bits/index nindices))])))

(require (submod "." core))

(module+ test
  (require (submod ".." core) bit-permutation rackunit rackunit/spec)

  (describe "make-simple-left-expansion"
    (make-simple-left-expansion 4 2 2))


  (describe "make-left-expanding-permutation"
    (define p (make-left-expanding-permutation 4 2))
    (check-true (perm? p)))

  (describe "make-right-expanding-permutation"
    (void)))
