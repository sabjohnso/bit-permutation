#lang racket

(module+ test
  (require bit-permutation/bits rackunit rackunit/spec)

  (describe "bits"
    (it "construct a bits object from a size (number of bits) and natural number not overflowing that size"
      (check-true (bits? (bits 2 #b00)))
      (check-true (bits? (bits 2 #b01)))
      (check-true (bits? (bits 2 #b10)))
      (check-true (bits? (bits 2 #b11))))

    (it "does not accept a size of zero (or negative sizes)"
      (check-exn exn:fail? (thunk (bits 0 #b0)))
      (check-exn exn:fail? (thunk (bits -1 #b0))))

    (it "does not accept a value that overflows the size"
      (check-exn exn:fail? (thunk (bits 2 #b111))))

    (it "displays all of its bits when printed"
      (check-equal? (format "~a" (bits 2 #b00)) "#<bits #b00>")))

  (describe "bits-ones"
    (it "constructs bits from a size where every bit is set"
      (check-equal? (bits-ones 2) (bits 2 #b11))))

  (describe "bits-zeros"
    (it "construct bits form a size where every bit is unset"
      (check-equal? (bits-zeros 2) (bits 2 #b00))))

  (describe "bits-and"
    (it "returns the bitwise and of its two input arguments"
      (check-equal? (bits-and (bits 2 #b10) (bits 2 #b01)) (bits 2 #b00))
      (check-equal? (bits-and (bits 2 #b01) (bits 2 #b11)) (bits 2 #b01)))
    (it "does not accept argument with differing sizes"
      (check-exn exn:fail? (thunk (bits-and (bits 2 #b11) (bits 4 #b1000))))))

  (describe "bits-ior"
    (it "returns the bitwise and of its two input arguments"
      (check-equal? (bits-ior (bits 2 #b10) (bits 2 #b01)) (bits 2 #b11))
      (check-equal? (bits-ior (bits 2 #b01) (bits 2 #b00)) (bits 2 #b01)))
    (it "does not accept argument with differing sizes"
      (check-exn exn:fail? (thunk (bits-ior (bits 2 #b11) (bits 4 #b1000))))))

  (describe "bits-xor"
    (it "returns the bitwise and of its two input arguments"
      (check-equal? (bits-xor (bits 2 #b10) (bits 2 #b01)) (bits 2 #b11))
      (check-equal? (bits-xor (bits 2 #b01) (bits 2 #b11)) (bits 2 #b10)))
    (it "does not accept argument with differing sizes"
      (check-exn exn:fail? (thunk (bits-xor (bits 2 #b11) (bits 4 #b1000))))))

  (describe "bits-not"
    (it "returns the bitwise negation of its input argument"
      (check-equal? (bits-not (bits 2 #b10)) (bits 2 #b01))))

  (describe "bits-duplicate"
    (it "returns duplicated bits"
      (check-equal? (bits-duplicate (bits 2 #b10)) (bits 4 #b1010))))


  (describe "bits-count-leading-zeros"
    (it "counts the number of leading zeros"
      (check-equal? (bits-count-leading-zeros (bits 4 #b0010)) 2)))

  (describe "bits-count-trailing-zeros"
    (it "counts the number of trailing zeros"
      (check-equal? (bits-count-trailing-zeros (bits 4 #b0010)) 1)))

  (describe "bits-popcount"
    (it "count the number of bits set"
      (check-equal? (bits-popcount (bits 4 #b0110)) 2)))

  (describe "bits-ref"
    (it "references a bit in bits"
      (define bs (bits 4 #b0110))
      (check-equal? (bits-ref bs 0) 0)
      (check-equal? (bits-ref bs 1) 1)
      (check-equal? (bits-ref bs 2) 1)
      (check-equal? (bits-ref bs 3) 0)))

  (describe "bits-active-indices"
    (it "returns a list of indices for the bits that are on"
      (check-equal?
       (bits-active-indices (bits 8 #b01010101))
       (list 0 2 4 6))))

  (describe "bit"
    (it "returns bits with the specified bit active"
      (check-equal? (bit 8 1)
                    (bits 8 #b00000010))))

  (describe "bits-active-indices"
    (it "returns a list of indices that are on in the input bits"
      (check-equal? (bits-active-indices (bits 4 #b0101)) '(0 2))))

  (describe "bits-mirror"
    (it "returns the mirror image of the input bits"
      (check-equal? (bits-mirror (bits 8 #b00000011))
                    (bits 8 #b11000000)))))
