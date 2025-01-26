#lang racket

(module+ test
  (require bit-permutation/visualization bit-permutation rackunit rackunit/spec)

  (describe "track-bits"
    (it "tracks bits in a permutation"
      (check-equal? (track-bits (mask (bits 4 #b0101))) '((0 . 0) (2 . 2)))
      (check-equal? (track-bits (postshift (mask (bits 4 #b0011)) 2)) '((0 . 2) (1 . 3)))
      (check-equal?
       (track-bits (split 4 (list (postshift (mask (bits 4 #b0011)) 2)
                                  (postshift (mask (bits 4 #b1100)) -2))))
       '((0 . 2) (1 . 3) (2 . 0) (3 . 1)))))

  (describe "draw-bit"
    (it "draws a bit"
      (check-not-exn (thunk (draw-bit 0 1 (λ (index state) "White"))))))

  (describe "draw-perm"
    (it "draws a perm"
      (check-not-exn
       (thunk
        (draw-perm (mask (bits 4 #b1010))
            (λ (index state)
              (if (zero? state) "Black"
                "White")))))))

  (describe "draw-split"
    (it "draws a split"
      (check-not-exn
       (thunk
        (draw-split (split 4 (list (mask (bits 4 #b0011))
                                   (mask (bits 4 #b1100))))
                    "Gray"
                    (list "Red" "Blue")))))))
