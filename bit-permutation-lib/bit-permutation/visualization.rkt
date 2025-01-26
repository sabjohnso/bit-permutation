#lang racket

(provide
 (contract-out
  [current-bit-diameter (parameter/c exact-positive-integer?)]
  [current-bit-spacing (parameter/c (or/c natural-number/c #f))]
  [current-register-spacing (parameter/c (or/c natural-number/c #f))]
  [current-flow-orientation (parameter/c flow-orientation/c)]
  [current-register-orientation (parameter/c register-orientation/c)]
  [draw-register draw-register-signature/c]
  [draw-bit draw-bit-signature/c]
  [track-bits track-bits-signature/c]
  [draw-perm draw-perm-signature/c]
  [draw-split draw-split-signature/c]))

(require bit-permutation pict srfi/26 racket/draw racket/hash)

(define current-bit-diameter
  (make-parameter 20))

(define current-bit-spacing
  (make-parameter #f))

(define current-register-spacing
  (make-parameter #f))

(define current-flow-orientation
  (make-parameter 'bottom-to-top))

(define current-register-orientation
  (make-parameter 'horizontal))

(define (draw-perm perm bit-colors
          #:bit-diameter [bit-diameter (current-bit-diameter)]
          #:bit-spacing [bit-spacing (current-bit-spacing)]
          #:register-spacing [register-spacing (current-register-spacing)]
          #:flow-orientation [flow-orientation (current-flow-orientation)])
  (parameterize* ([current-bit-diameter bit-diameter]
                  [current-bit-spacing
                   (if bit-spacing bit-spacing (current-bit-diameter))]
                  [current-register-spacing
                   (if register-spacing register-spacing
                     (* (perm-size perm) (current-bit-spacing)))]
                  [current-flow-orientation flow-orientation]
                  [current-register-orientation
                   (if (or (eq? flow-orientation 'bottom-to-top)
                           (eq? flow-orientation 'top-to-bottom))
                       'horizontal
                     'vertical)])
    (let* ([bit-tracks (track-bits perm)]
           [inverse-bit-tracks (invert-alist bit-tracks)])
      (let-values ([(domain domain-bits) (draw-register (domain perm) bit-colors)]
                   [(codomain codomain-bits)
                    (draw-register (codomain perm)
                                   (build-codomain-bit-colors bit-colors inverse-bit-tracks))])
        ((make-composite-link-fn domain-bits codomain-bits bit-tracks bit-colors)
         (make-registers domain codomain))))))

(define (make-registers domain codomain)
  (case (current-flow-orientation)
    [(bottom-to-top) (vc-append (current-register-spacing) codomain domain)]
    [(top-to-bottom) (vc-append (current-register-spacing) domain codomain)]
    [(left-to-right) (hc-append (current-register-spacing) domain codomain)]
    [(right-to-left) (hc-append (current-register-spacing) codomain domain)]))

(define (make-composite-link-fn domain-bits codomain-bits bit-tracks  bit-colors)
  (let-values ([(angle) (select-angle)]
               [(domain-finder codomain-finder) (select-finders)])
    (apply compose
           (for/list ([d/c bit-tracks])
             (let ([color (bit-colors (car d/c) 1)]
                   [domain-bit (list-ref domain-bits (car d/c))]
                   [codomain-bit (list-ref codomain-bits (cdr d/c))])
               (make-link-fn
                domain-bit codomain-bit domain-finder codomain-finder angle color))))))

(define (make-link-fn domain-bit codomain-bit domain-finder codomain-finder angle color)
  (cut pin-arrow-line
    (current-bit-diameter)  <>
    domain-bit domain-finder
    codomain-bit codomain-finder
    #:color color
    #:start-angle angle
    #:end-angle angle))

(define (select-angle)
  (case (current-flow-orientation)
    [(bottom-to-top) (/ pi 2)]
    [(top-to-bottom) (- (/ pi 2))]
    [(left-to-right)  0]
    [(right-to-left) pi]
    [else (error "Failed to select angle")]))

(define (select-finders)
  (case (current-flow-orientation)
    [(bottom-to-top) (values ct-find cb-find)]
    [(top-to-bottom) (values cb-find ct-find)]
    [(left-to-right) (values rc-find lc-find)]
    [(right-to-left) (values lc-find rc-find)]
    [else (error "Failed to select finders")]))

(define (build-codomain-bit-colors bit-colors inverse-bit-tracks)
  (λ (index state)
    (if (zero? state) (bit-colors index state)
      (bit-colors (cdr (assoc index inverse-bit-tracks)) state))))

(define (draw-split
         split-perm inactive-color colors
         #:bit-diameter [bit-diameter (current-bit-diameter)]
         #:bit-spacing [bit-spacing (current-bit-diameter)]
         #:register-spacing [register-spacing (current-register-spacing)]
         #:flow-orientation [flow-orientation (current-flow-orientation)])
  (parameterize* ([current-bit-diameter bit-diameter]
                  [current-bit-spacing (if bit-spacing bit-spacing (current-bit-diameter))]
                  [current-register-spacing (if register-spacing register-spacing (* (perm-size split-perm) (current-bit-spacing)))]
                  [current-flow-orientation flow-orientation])
    (draw-perm split-perm (build-split-bit-colors split-perm inactive-color colors))))

(define (build-split-bit-colors split-perm inactive-color colors)
  (let ([color-table (build-split-color-table split-perm colors)])
    (λ (index state)
      (if (zero? state) inactive-color
        (hash-ref color-table index)))))

(define (build-split-color-table split-perm colors)
  (for/fold ([color-table (make-immutable-hash)])
      ([perm (split-perms split-perm)]
       [color colors])
    (let ([indices (bits-active-indices (domain perm))])
      (hash-union color-table
                  (for/hash ([index indices])
                    (values index color))))))

(define (draw-register
         bits bit-colors
         #:bit-diameter [bit-diameter (current-bit-diameter)]
         #:bit-spacing [bit-spacing (current-bit-spacing)]
         #:register-orientation [register-orientation (current-register-orientation)])
  (parameterize* ([current-bit-diameter bit-diameter]
                  [current-bit-spacing (if bit-spacing bit-spacing (current-bit-diameter))]
                  [current-register-orientation register-orientation])
    (let ([bits (for/list ([index (bits-size bits)])
                  (draw-bit index (bits-ref bits index) bit-colors))])
      (values
       (if (eq? (current-register-orientation) 'horizontal)
           (apply hc-append (current-bit-spacing) bits)
         (apply vc-append (current-bit-spacing) bits))
       bits))))

(define (draw-bit index state bit-colors #:bit-diameter [bit-diameter (current-bit-diameter)])
  (disk bit-diameter #:color (bit-colors index state) #:draw-border? #f))

(define (track-bits perm)
  (let* ([size (perm-size perm)]
         [domain (domain perm)])
    (for/list ([index (bits-active-indices domain)])
      (cons index (car (bits-active-indices (perm (bit size index))))))))

(define (invert-alist alist)
  (for/list ([kv alist])
    (cons (cdr kv) (car kv))))

(define/contract (length=/c n)
  (-> natural-number/c contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'length=/c n)
   #:first-order (λ (arg) (and (list? arg) (= (length arg) n)))))

(define flow-orientation/c
  (or/c 'left-to-right 'right-to-left 'top-to-bottom 'bottom-to-top))

(define draw-split-signature/c
  (->i
   ([split split?]
    [inactive-color (or/c (is-a?/c color%) string?)]
    [colors [split] (and/c (listof (or/c (is-a?/c color%) string?))
                           (length=/c (length (split-perms split))))])
   (#:bit-diameter [bit-diameter 20]
    #:bit-spacing [bit-spacing (or/c real? #f)]
    #:register-spacing [register-spacing (or/c real? #f)]
    #:flow-orientation [floworientation flow-orientation/c])
   [result pict?]))

(define register-orientation/c
  (or/c 'horizontal 'vertical))

(define bit/c (or/c 0 1))

(define bit-colors/c
  (-> natural-number/c bit/c (or/c string? (is-a?/c color%))))

(define draw-bit-signature/c
  (->* (natural-number/c bit/c bit-colors/c) (#:bit-diameter exact-positive-integer?) pict?))

(define draw-register-signature/c
  (->* (bits? bit-colors/c)
       (#:bit-diameter exact-positive-integer?
        #:bit-spacing (or/c natural-number/c #f)
        #:register-orientation register-orientation/c)
       (values pict? (listof pict?))))

(define track-bits-signature/c
  (-> perm? (listof (cons/c natural-number/c natural-number/c))))

(define draw-perm-signature/c
  (->* (perm? bit-colors/c)
       (#:bit-diameter exact-positive-integer?
        #:bit-spacing (or/c exact-positive-integer? #f)
        #:register-spacing (or/c exact-positive-integer? #f)
        #:flow-orientation flow-orientation/c)
       pict?))
