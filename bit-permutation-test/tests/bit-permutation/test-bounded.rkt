#lang racket

(module+ test
  (require
   bit-permutation
   rackunit rackunit/spec)

  (describe "mask"
    (it "accepts bits"
      (check-not-exn (thunk (mask (bits 8 #b11001100)))))
    (it "does not accept anything else"
      (check-exn exn:fail? (thunk (mask #b11001100))))
    (context "with a mask defined"
      (define p (mask (bits 8 #b11001100)))
      (it "is a perm constructor"
        (check-true (perm? p)))
      (it "has the same domain and codomain"
        (check-equal? (domain p) (codomain p)))
      (it "operates as a bit mask on bits"
        (check-equal? (p (bits-ones (perm-size p)))
                      (domain p)))))

  (describe "preshift"
    (it "accepts a perm? and an offset (positive or negative)"
      (check-not-exn (thunk (preshift (mask (bits 8 #b00111100))  2)))
      (check-not-exn (thunk (preshift (mask (bits 8 #b00111100)) -2))))
    (it "will not accept an offset that shifts its domain out of range"
      (check-exn exn:fail? (thunk (preshift (mask (bits 8 #b00111100)) 3))))
    (context "with a preshift perm defined"
      (define m (mask (bits 8 #b00111100)))
      (define s 1)
      (define p (preshift m s))
      (it "is a perm constructor"
        (check-true (perm? p)))
      (it "holds a perm"
        (check-true (perm? (preshift-perm p)))
        (check-equal? (preshift-perm p) m))
      (it "holds an offset"
        (check-equal? (preshift-offset p) s))
      (it "has a size equal to the underlying perm"
        (check-equal? (perm-size p) (perm-size (preshift-perm p))))
      (it "has a domain that is the domain of its child perm shifted opposite its offset value"
        (check-equal? (domain p) (bits-shift (domain m) (- s))))
      (it "has a codomain that is the codomain of the underlying perm"
        (check-equal? (codomain p) (codomain m)))
      (it "has the same number of bits in its domain and comdomain"
        (check-equal? (bits-popcount (domain p))
                      (bits-popcount (codomain p))))))

  (describe "postshift"
    (it "accepts a perm? annd a offset (positive or negative)"
      (check-not-exn (thunk (postshift (mask (bits 8 #b00111100))  2)))
      (check-not-exn (thunk (postshift (mask (bits 8 #b00111100)) -2))))
    (it "will not accept an offset that shifts its codomain out of range"
      (check-exn exn:fail? (thunk (postshift (mask (bits 8 #b01111100)) 2))))
    (context "with a postshift perm defined"
      (define m (mask (bits 8 #b00111100)))
      (define s 1)
      (define p (postshift m s))
      (it "holds a perm"
        (check-true (perm? (postshift-perm p)))
        (check-equal? (postshift-perm p) m))
      (it "holds an offset"
        (check-equal? (postshift-offset p) s))
      (it "has a size that is equal to the size of the underlying perm"
        (check-equal? (perm-size p) (perm-size m)))
      (it "has the same domain as its child permutation"
        (check-equal? (domain p) (domain m)))
      (it "has a codomain that is equivalent to the underlying perm's codomain, but shifted"
        (check-equal? (codomain p) (bits-shift (codomain m) s)))
      (it "has the same number of bits in its domain and comdomain"
        (check-equal? (bits-popcount (domain p))
                      (bits-popcount (codomain p))))))

  (describe "split"
    (it "accepts a size and a list (possibly empty) of perm?"
      (check-not-exn (thunk (split 8 '())))
      (check-not-exn (thunk (split 8 (list (mask (bits 8 #b00000011)) (mask (bits 8 #b00001100)))))))
    (it "will NOT accept a list of perm? if the elements have different sizes than the specified size"
      (define size 8)
      (define p1 (mask (bits 8 #b00000011)))
      (define p2 (mask (bits 4 #b0011)))
      (check-not-equal? (perm-size p1) (perm-size p2))
      (check-exn exn:fail? (thunk (split size (list p1 p2)))))
    (it "will NOT accept a list of perm? if any of the elments have overlapping domains"
      (check-exn exn:fail?
                 (thunk (split 8 (list (mask (bits 8 #b00000011)) (mask (bits 8 #b00000110)))))))
    (it "will NOT accept a list of perm? if any of the elments have overlapping codomains"
      (check-exn exn:fail?
                 (thunk (split 8 (list
                                  (postshift (mask (bits 8 #b00000011)) 1)
                                  (mask (bits 8 #b00001100)))))))

    (context "with a split permutation defined"
      (define size 8)
      (define p1 (postshift (mask (bits size #b00000011)) 4))
      (define p2 (mask (bits size #b00001100)))
      (define p (split size (list p1 p2)))
      (it "is a perm"
        (check-true (perm? p)))
      (it "has a size"
        (check-equal? (perm-size p) size))
      (it "has a domain equal to the union of the child perm domains"
        (check-equal? (domain p) (bits-ior (domain p1) (domain p2))))
      (it "has a codomain equal to the union of the child perm codomains"
        (check-equal? (codomain p) (bits-ior (codomain p1) (codomain p2))))
      (it "has the same number of bits in its domain and comdomain"
        (check-equal? (bits-popcount (domain p))
                      (bits-popcount (codomain p))))))


  (describe "seq"
    (it "accepts size composable list of perms (possibly empty)"
      (check-not-exn (thunk (seq 8 '())))
      (check-not-exn
       (thunk (seq 8 (list (postshift (mask (bits 8 #b00000011) ) 2)
                           (preshift (mask (bits 8 #b00110000)) 2))))))
    (it "does NOT accept a list of permutations that are not composable"
      (check-exn exn:fail? (thunk (seq 4 (list (mask (bits 4 #b0011)) (mask (bits 4 #b1100)))))))
    (context "with a defined seq perm"
      (define size 8)
      (define p1 (split size (list (postshift (mask (bits size #b00001100)) 2)
                                   (mask (bits size #b00000011)))))
      (define p2 (split size (list (postshift (mask (bits size #b00100010)) 1)
                                   (mask (bits size #b00010001)))))
      (define p (seq size (list p1 p2)))
      (it "is a perm?"
        (perm? p))
      (it "has a size"
        (check-equal? (perm-size p) size))
      (it "has a domain that is the same as the domain of its first child"
        (check-equal? (domain p) (domain p1)))
      (it "has a codomain that is the same as the domain of its last child"
        (check-equal? (codomain p) (codomain p2)))
      (it "has the same number of bits in its domain and comdomain"
        (check-equal? (bits-popcount (domain p))
                      (bits-popcount (codomain p))))))

  (describe "perm-duplicate"
    (it "duplicates a permutation, doubling the size and repeating the patterns on the domain and codomain"
      (it "operates on masks"
        (check-equal?
         (perm-duplicate (mask (bits 4 #b0011)))
         (mask (bits 8 #b00110011))))
      (it "operates on preshift perms"
        (define p1 (preshift (mask (bits 4 #b0010)) 1))
        (define p2 (perm-duplicate p1))
        (check-equal? (domain p2) (bits-duplicate (domain p1)))
        (check-equal? (codomain p2) (bits-duplicate (codomain p1))))
      (it "operates on preshift perms"
        (define p1 (postshift (mask (bits 4 #b0010)) 1))
        (define p2 (perm-duplicate p1))
        (check-equal? (domain p2) (bits-duplicate (domain p1)))
        (check-equal? (codomain p2) (bits-duplicate (codomain p1))))
      (it "operates on split perms"
        (define size 8)
        (define p1 (split size
                          (list (mask (bits size #b00000011))
                                (postshift (mask (bits size #b00001100)) 2))))
        (define p2 (perm-duplicate p1))
        (check-equal? (domain p2) (bits-duplicate (domain p1)))
        (check-equal? (codomain p2) (bits-duplicate (codomain p1)))
        (check-equal? (perm-size p2) (* 2 (perm-size p1)))))))
