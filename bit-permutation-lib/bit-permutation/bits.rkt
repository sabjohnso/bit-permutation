#lang racket

(provide
 (except-out (struct-out bits) bits)
 (contract-out
  [bits bits-constructor/c]
  [bits-zeros (-> exact-positive-integer? bits?)]
  [bits-ones (-> exact-positive-integer? bits?)]
  [bits-and bits-binary-operator/c]
  [bits-ior bits-binary-operator/c]
  [bits-xor bits-binary-operator/c]
  [bits-not bits-unary-operator/c]
  [bits-duplicate bits-duplication/c]
  [bits-shift bits-shift/c]
  [bits-ref bits-ref/c]
  [bits-count-leading-zeros bits-count/c]
  [bits-count-trailing-zeros bits-count/c]
  [bits-popcount bits-count/c]
  [bits-empty? bits-empty/c]
  [bits-active-indices bits-active-indices-signature/c]
  [bit bit-signature/c]))

(require srfi/54)

(struct bits
  (size value)
  #:methods gen:custom-write
  ((define (write-proc bits port mode)
     (display
      (format "#<bits ~a>" (format-bits  (bits-size bits) (bits-value bits)))
      port)))
  #:transparent)

(define (format-bits size value)
  (cat value 'binary (+ 2 size) #\0))

(define (bits-zeros size)
  (bits size 0))

(define (bits-ones size)
  (bits size (sub1 (arithmetic-shift 1 size))))

(define (bits-and b1 b2)
  (bits (bits-size b1) (bitwise-and (bits-value b1) (bits-value b2))))

(define (bits-ior b1 b2)
  (bits (bits-size b1) (bitwise-ior (bits-value b1) (bits-value b2))))

(define (bits-xor b1 b2)
  (bits (bits-size b1) (bitwise-xor (bits-value b1) (bits-value b2))))

(define (bits-not b)
  (match-let ([(bits size value) b])
    (bits size (bitwise-and (bitwise-not value) (sub1 (arithmetic-shift 1 size))))))

(define (bits-shift b offset)
  (match-let ([(bits size value) b])
    (bits size (arithmetic-shift value offset))))

(define (bits-duplicate b)
  (match-let ([(bits size value) b])
    (bits (* 2 size) (bitwise-ior value (arithmetic-shift value size)))))

(define (nbits-right n)
  (sub1 (arithmetic-shift 1 n)))

(define (nbits-left size n)
  (arithmetic-shift (nbits-right n) (- size n)))

(define (bits-ref bs index)
  (bitwise-and (bits-value (bits-shift bs (- index))) 1))

(define (bits-count-leading-zeros b)
  (match-let ([(bits size value) b])
    (define (recur value n accum)
      (cond [(zero? n) accum]
            [(zero? (bitwise-and value (nbits-left size n)))
             (recur (arithmetic-shift value n) (* 2 n) (+ n accum))]
            [else (recur value (quotient n 2) accum)]))
    (recur value 1 0)))

(define (bits-count-trailing-zeros b)
  (match-let ([(bits size value) b])
    (define (recur value n accum)
      (cond [(zero? n) accum]
            [(zero? (bitwise-and value (nbits-right n)))
             (recur (arithmetic-shift value (- n)) (* 2 n) (+ n accum))]
            [else (recur value (quotient n 2) accum)]))
    (recur value 1 0)))

(define (bits-popcount b)
  (match-let ([(bits size value) b])
    (for/fold ([n 0])
        ([i (in-range size)])
      (if (zero? (bitwise-and value (arithmetic-shift 1 i))) n (add1 n)))))

(define (bits-empty? b)
  (zero? (bits-value b)))

(define (bits-active-indices bs)
  (match-let ([(bits size value) bs])
    (reverse
     (for/fold ([result '()])
         ([index (in-range size)])
       (if (zero? (bits-ref bs index)) result
         (cons index result))))))

(define (bit size index)
  (bits size (arithmetic-shift 1 index)))

(define/contract (natural-number</c v)
  (-> natural-number/c contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'natural-number</c v)
   #:first-order
   (λ (arg) (and (exact-nonnegative-integer? arg) (< arg v)))))

(define/contract (not-overflowing/c size)
  (-> exact-positive-integer? contract?)
  (let ([overflow-value (arithmetic-shift 1 size)])
    (make-flat-contract
     #:name (build-compound-type-name 'not-overflowing/c size)
     #:first-order (λ (arg) (and (exact-nonnegative-integer? arg)
                                 (< arg overflow-value))))))

(define/contract (same-size/c bits)
  (-> bits? contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'same-size/c)
   #:first-order (λ (arg) (and (bits? arg) (= (bits-size arg) (bits-size bits))))))

(define/contract (twice-the-size/c bits)
  (-> bits? contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'twice-the-size/c)
   #:first-order (λ (arg) (and (bits? arg) (= (bits-size arg) (* 2 (bits-size bits)))))))

(define bits-binary-operator/c
  (->i ([b1 bits?] [b2 (b1) (same-size/c b1)])
       [result (b1) (same-size/c b1)]))

(define bits-unary-operator/c
  (->i ([b bits?]) [result (b) (same-size/c b)]))

(define bits-constructor/c
  (->i ([size exact-positive-integer?] [value (size) (not-overflowing/c size)])
       [result bits?]))

(define bit/c (or/c 0 1))

(define bits-ref/c
  (->i ([bits bits?] [index (bits) (and/c natural-number/c (</c (bits-size bits)))])
      [result bit/c]))

(define bits-shift/c
  (->i ([b bits?] [offset exact-integer?])
       [result (b) (same-size/c b)]))

(define bits-duplication/c
  (->i ([b bits?])
       [result (b) (twice-the-size/c b)]))

(define bits-count/c
  (-> bits? natural-number/c))

(define bits-empty/c
  (-> bits? boolean?))


(define bits-active-indices-signature/c
  (-> bits? (listof natural-number/c)))

(define/contract (bits-of-size/c size)
  (-> exact-positive-integer? contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'bits-of-size/c size)
   #:first-order
   (λ (arg) (and (bits? arg) (= (bits-size arg) size)))))

(define bit-signature/c
  (->i ([size exact-positive-integer?] [index (size) (natural-number</c size)])
       [result (size) (bits-of-size/c size)]))
