#lang racket

(provide
 (except-out (struct-out mask) mask)
 (except-out (struct-out preshift) preshift)
 (except-out (struct-out postshift) postshift)
 (except-out (struct-out split) split)
 (except-out (struct-out seq) seq)
 (contract-out
  [perm? predicate/c]
  [domain (-> perm? bits?)]
  [codomain (-> perm? bits?)]
  [perm-size (-> perm? exact-positive-integer?)]
  [mask (-> bits? (and/c mask?))]
  [preshift preshift/c]
  [postshift postshift/c]
  [split split/c]
  [seq  seq/c]
  [perm-duplicate (-> perm? perm?)]))

(require bit-permutation/bits)

(define (perm? arg)
  (or (mask? arg)
      (preshift? arg)
      (postshift? arg)
      (split? arg)
      (seq? arg)))

(struct mask
  (value)
  #:property prop:procedure
  (λ (this arg) (apply-mask this arg))
  #:transparent)

(define (apply-mask perm arg)
  (match-let ([(mask value) perm])
    (bits-and value arg)))

(struct preshift
  (perm offset)
  #:property prop:procedure
  (λ (this arg) (apply-preshift this arg)))

(define (apply-preshift perm arg)
  (match-let* ([(preshift perm offset) perm]
               [size (perm-size perm)])
    (perm (bits-and (bits-ones size)
                    (bits-shift arg offset)))))

(struct postshift
  (perm offset)
  #:property prop:procedure
  (λ (this arg) (apply-postshift this arg)))

(define (apply-postshift perm arg)
  (match-let* ([(postshift perm offset) perm]
               [size (perm-size perm)])
    (bits-and (bits-ones size)
              (bits-shift (perm arg) offset))))

(struct split
  (size perms)
  #:property prop:procedure
  (λ (this arg) (apply-split this arg)))

(define (apply-split perm arg)
  (let ([size (split-size perm)]
        [perms (split-perms perm)])
    (define (recur perms accum)
      (if (null? perms) accum
        (recur (cdr perms) (bits-ior ((car perms) arg) accum))))
    (recur perms (bits-zeros size))))

(struct seq
  (size perms)
  #:property prop:procedure
  (λ (this arg) (apply-seq this arg)))

(define (apply-seq perm arg)
  (let ([size (seq-size perm)]
        [perms (seq-perms perm)])
    (define (recur perms accum)
      (if (null? perms) accum
        (recur (cdr perms) ((car perms) accum))))
    (recur perms arg)))

(define (perm-size perm)
  (match perm
    [(mask value) (bits-size value)]
    [(preshift perm _) (perm-size perm)]
    [(postshift perm _) (perm-size perm)]
    [(split size _) size]
    [(seq size _) size]))

(define (domain perm)
  (match perm
    [(mask value) value]
    [(preshift perm offset) (bits-shift (domain perm) (- offset))]
    [(postshift perm offset) (domain perm)]
    [(split size perms)
     (for/fold ([accum (bits-zeros size)])
         ([perm perms])
       (bits-ior accum (domain perm)))]
    [(seq size perms)
     (if (null? perms) (bits-zeros size)
       (domain (car perms)))]))

(define (codomain perm)
  (perm (domain perm)))

(define (perm-duplicate perm)
  (match perm
    [(mask bits) (mask (bits-duplicate bits))]
    [(preshift perm offset) (preshift (perm-duplicate perm) offset)]
    [(postshift perm offset) (postshift (perm-duplicate perm) offset)]
    [(split size perms) (split (* 2 size) (map perm-duplicate perms))]))

(define/contract (bounded-preshift-offset/c perm)
  (-> perm? contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'bounded-preshift-offset/c perm)
   #:first-order
   (λ (offset)
     (and (exact-integer? offset)
          (cond [(positive? offset) (<= offset (bits-count-leading-zeros (domain perm)))]
                [(negative? offset) (<= (abs offset) (bits-count-trailing-zeros (domain perm)))]
                [else #t])))))

(define/contract (bounded-postshift-offset/c perm)
  (-> perm? contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'bounded-postshift-offset perm)
   #:first-order
   (λ (offset)
     (and (exact-integer? offset)
          (cond [(positive? offset) (<= offset (bits-count-leading-zeros (codomain perm)))]
                [(negative? offset) (<= offset (bits-count-trailing-zeros (codomain perm)))]
                [else #t])))))

(define/contract (same-size/c perm)
  (-> perm? contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'same-size/c perm)
   #:first-order
   (λ (arg)
     (and (perm? arg)
          (= (perm-size arg) (perm-size perm))))))

(define (disjoint? p1 p2)
  (and (bits-empty? (bits-and (domain p1) (domain p2)))
       (bits-empty? (bits-and (codomain p1) (codomain p2)))))

(define/contract (disjoint-perm-list/c size)
  (-> exact-positive-integer? contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'disjoint-perm-list/c size)
   #:first-order
   (λ (arg)
     (and (list? arg)
          (or (null? arg)
              (let ([p1 (car arg)])
                (for/fold ([accum-domain (domain p1)]
                           [accum-codomain (codomain p1)]
                           [result (= (perm-size p1) size)] #:result result)
                    ([p2 (cdr arg)] #:break (not result))
                  (values
                   (bits-ior accum-domain (domain p2))
                   (bits-ior accum-codomain (codomain p2))
                   (and (perm? p2)
                        (= (perm-size p2) size)
                        (bits-empty? (bits-and (domain p2) accum-domain))
                        (bits-empty? (bits-and (codomain p2) accum-codomain)))))))))))

(define/contract (composable-perm-list/c size)
  (-> exact-positive-integer? contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'composable-perm-list/c size)
   #:first-order
   (λ (arg)
     (and (list? arg)
          (or (null? arg)
              (let ([p1 (car arg)])
                (for/fold ([p1 p1] [result (= (perm-size p1) size)] #:result result)
                    ([p2 (cdr arg)]
                     #:break (not result))
                  (values p2 (and (perm? p2)
                                  (= size (perm-size p2))
                                  (equal? (codomain p1) (domain p2)))))))))))

(define (perm-sizeof/c size)
  (make-flat-contract
   #:name (build-compound-type-name 'perm-sizeof/c size)
   #:first-order
   (λ (arg) (and (perm? arg) (= (perm-size arg) size)))))

(define preshift/c
  (->i ([perm perm?] [offset (perm) (bounded-preshift-offset/c perm)])
       [result (perm) (and/c preshift? (same-size/c perm))]))

(define postshift/c
  (->i ([perm perm?] [offset (perm) (bounded-postshift-offset/c perm)])
       [result (perm) (and/c postshift? (same-size/c perm))]))

(define split/c
  (->i ([size exact-positive-integer?] [perms (size) (disjoint-perm-list/c size)])
       [result (size) (and/c split? (perm-sizeof/c size))]))

(define seq/c
  (->i ([size exact-positive-integer?] [perms (size) (composable-perm-list/c size)])
       [result (size) (and/c seq? (perm-sizeof/c size))]))
