#lang racket

(provide
 (except-out (struct-out mask) mask)
 (except-out (struct-out preshift) preshift)
 (except-out (struct-out postshift) postshift)
 (except-out (struct-out split) split)
 (except-out (struct-out seq) seq)
 (contract-out
  [perm? predicate/c]
  [perm-sizeof/c contract?]
  [domain (-> perm? bits?)]
  [codomain (-> perm? bits?)]
  [perm-size (-> perm? exact-positive-integer?)]
  [mask (-> bits? (and/c mask?))]
  [preshift preshift/c]
  [postshift postshift/c]
  [split split/c]
  [seq  seq/c]
  [perm-duplicate (-> perm? perm?)]
  [perm-invert (-> perm? perm?)]
  [perm-> (->* (perm?) #:rest (listof perm?) perm?)]
  [perm+ (->* (perm?) #:rest (listof perm?) perm?)]
  [perm-trace-bits (-> perm? (listof (cons/c natural-number/c natural-number/c)))]
  [perm-mirror (->i ([perm perm?]) [result (perm) (same-size-perm/c perm)])]
  [perm=? (->i ([p1 perm?] [p2 (p1) (same-size-perm/c p1)]) [result boolean?])]
  [perm-inverse=? (->i ([p1 perm?] [p2 (p1) (same-size-perm/c p1)]) [result boolean?])]
  [perm-mirror=? (->i ([p1 perm?] [p2 (p1) (same-size-perm/c p1)]) [result boolean?])]
  [perm-inverse-mirror=? (->i ([p1 perm?] [p2 (p1) (same-size-perm/c p1)]) [result boolean?])]
  [perm-identity? (-> perm? boolean?)]
  [perm-symmetric? (-> perm? boolean?)]
  [perm-zip (-> perm? perm?)]
  [disjoint? (->* () () #:rest (listof perm?) boolean?)]))

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
  (λ (this arg) (apply-preshift this arg))
  #:transparent)

(define (apply-preshift perm arg)
  (match-let* ([(preshift perm offset) perm]
               [size (perm-size perm)])
    (perm (bits-and (bits-ones size)
                    (bits-shift arg offset)))))

(struct postshift
  (perm offset)
  #:property prop:procedure
  (λ (this arg) (apply-postshift this arg))
  #:transparent)

(define (apply-postshift perm arg)
  (match-let* ([(postshift perm offset) perm]
               [size (perm-size perm)])
    (bits-and (bits-ones size)
              (bits-shift (perm arg) offset))))

(struct split
  (size perms)
  #:property prop:procedure
  (λ (this arg) (apply-split this arg))
  #:transparent)

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
  (λ (this arg) (apply-seq this arg))
  #:transparent)

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

(define (perm-invert perm)
  (match perm
    [(mask bits) (mask bits)]
    [(preshift perm offset) (postshift (perm-invert perm) (- offset))]
    [(postshift perm offset) (preshift (perm-invert perm) (- offset))]
    [(split size perms) (split size (map perm-invert perms))]
    [(seq size perms) (seq size (reverse (map perm-invert perms)))]))

(define (perm-mirror perm)
  (match perm
    [(mask bits) (mask (bits-mirror bits))]
    [(preshift perm offset) (preshift (perm-mirror perm) (- offset))]
    [(postshift perm offset) (postshift (perm-mirror perm) (- offset))]
    [(split size perms) (split size (map perm-mirror perms))]
    [(seq size perms) (seq size (map perm-mirror perms))]))

(define (perm-> perm . perms)
  (seq (perm-size perm) (cons perm perms)))

(define (perm+ perm . perms)
  (split (perm-size perm) (cons perm perms)))


(define (perm-trace-bits p)
  (let* ([size (perm-size p)]
         [domain (domain p)])
    (for/list ([index (bits-active-indices domain)])
      (cons index (car (bits-active-indices (p (bit size index))))))))

(define (perm-zip p)
  (match p
    [(mask bits) (mask bits)]
    [(preshift perm offset) (preshift (perm-zip perm) offset)]
    [(postshift perm offset) (postshift (perm-zip perm) offset)]
    [(split size perms) (perm-zip-split (split size perms))]
    [(seq size perms) (seq size (map perm-zip perms))]))


(define (perm-zip-split split-perm)
  (match split-perm
    [(split size (list (seq _ (list permss ...)) ...))
     #:when (zip-compatible? permss)
     (define (recur n permss accum)
       (displayln accum)
       (if (zero? n) (seq size (reverse accum))
         (recur (sub1 n) (map cdr permss)
                (cons (split size (map car permss))
                      accum))))
     (recur (length (car permss)) permss '())]
    [(split size perms) (split size (map perm-zip perms))]))

(define (zip-compatible? permss)
  (define (recur n permss)
    (or (zero? n)
        (let ([perms (map car permss)])
          (and (apply (disjoint? perms))
               (recur (sub1 n) (map cdr perms))))))
  (if (< (length permss) 2) #f
    (let ([n (length (car permss))])
      (and (for/and ([perms permss])
             (= n (length perms)))
           (recur n permss)))))

(define (perm=? p1 p2)
  (and (equal? (domain p1) (domain p2))
       (equal? (codomain p1) (codomain p2))
       (equal? (perm-trace-bits p1)
               (perm-trace-bits p2))))

(define (perm-inverse=? p1 p2)
  (perm=? p1 (perm-invert p2)))

(define (perm-mirror=? p1 p2)
  (perm=? p1 (perm-mirror p2)))

(define (perm-inverse-mirror=? p1 p2)
  (perm=? p1 (perm-invert (perm-mirror p2))))

(define (perm-identity? p)
  (equal? (domain p) (codomain p)))

(define (perm-symmetric? p)
  (perm-mirror=? p p))

(define (disjoint? . ps)
  (cond [(= 2 (length ps))
         (let ([p1 (car ps)]
                  [p2 (cadr ps)])
              (and (bits-empty? (bits-and (domain p1) (domain p2)))
                   (bits-empty? (bits-and (codomain p1) (codomain p2)))))]
        [(< (length ps) 2) #t]
        [else (and (let ([p1 (car ps)])
                     (for/and ([p2 (cdr ps)])
                       (disjoint? p1 p2)))
                   (apply disjoint? (cdr ps)))]))

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

(define/contract (same-size-perm/c perm)
  (-> perm? contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'same-size-perm/c perm)
   #:first-order
   (λ (arg)
     (and (perm? arg)
          (= (perm-size arg) (perm-size perm))))))

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
       [result (perm) (and/c preshift? (same-size-perm/c perm))]))

(define postshift/c
  (->i ([perm perm?] [offset (perm) (bounded-postshift-offset/c perm)])
       [result (perm) (and/c postshift? (same-size-perm/c perm))]))

(define split/c
  (->i ([size exact-positive-integer?] [perms (size) (disjoint-perm-list/c size)])
       [result (size) (and/c split? (perm-sizeof/c size))]))

(define seq/c
  (->i ([size exact-positive-integer?] [perms (size) (composable-perm-list/c size)])
       [result (size) (and/c seq? (perm-sizeof/c size))]))
