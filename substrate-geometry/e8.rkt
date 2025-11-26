#lang racket/base

;; E8 Lattice Operations
;; Package B: Geometry layer
;; Implements E8 exceptional Lie algebra root system

(require "../kernel-spec.rkt"
         racket/list
         racket/vector)

(provide
 e8-construct-roots
 e8-get-simple-roots
 e8-point-norm-sq
 make-e8-point
 e8-point->list
 list->e8-point)

;; Construct all 240 E8 roots
;; Type 1: (±eᵢ ± eⱼ) for i ≠ j: 112 roots
;; Type 2: ½(±e₁±e₂±...±e₈) with even # of -1s: 128 roots
(define (e8-construct-roots)
  "Construct all 240 E8 roots using exact arithmetic"
  (let ((roots '()))
    ;; Type 1: (±eᵢ ± eⱼ) for i ≠ j
    (for* ([i (in-range 8)]
           [j (in-range (add1 i) 8)]
           [sign-i (in-list '(1 -1))]
           [sign-j (in-list '(1 -1))])
      (let ((coords (make-list 8 0)))
        (set! coords (for/list ([idx (in-range 8)])
                       (cond
                         [(= idx i) sign-i]
                         [(= idx j) sign-j]
                         [else 0])))
        (set! roots (cons coords roots))))
    
    ;; Type 2: ½(±e₁±e₂±...±e₈) with even number of minus signs
    (for ([mask (in-range 256)])  ; 2^8 combinations
      (let* ((signs (for/list ([i (in-range 8)])
                      (= (bitwise-and mask (arithmetic-shift 1 i)) 0)))
             (num-minus (length (filter (lambda (s) (not s)) signs))))
        (when (even? num-minus)  ; Even number of -1s
          (let ((coords (for/list ([s signs])
                          (if s 1/2 -1/2))))
            (set! roots (cons coords roots))))))
    
    (reverse roots)))

;; Get simple roots (Dynkin diagram basis) for E8
;; Bourbaki convention - using exact rationals
(define (e8-get-simple-roots)
  "Get 8 simple roots for E8 in exact rational form"
  (list
   (list -1/2 -1/2 -1/2 -1/2 -1/2 -1/2 -1/2 1/2)  ; α₁
   (list 1 1 0 0 0 0 0 0)                          ; α₂
   (list -1 1 0 0 0 0 0 0)                         ; α₃
   (list 0 -1 1 0 0 0 0 0)                         ; α₄
   (list 0 0 -1 1 0 0 0 0)                         ; α₅
   (list 0 0 0 -1 1 0 0 0)                         ; α₆
   (list 0 0 0 0 -1 1 0 0)                         ; α₇
   (list 0 0 0 0 0 -1 1 0)))                       ; α₈

;; Compute norm squared of E8 point
(define (e8-point-norm-sq point)
  "Compute squared norm of E8 point"
  (let ((coords (E8-Point-coords point)))
    (for/sum ([x coords])
      (* x x))))

;; Make E8-Point from list of integers
(define (make-e8-point coords)
  "Create E8-Point from list of 8 integers"
  (let ((coords-list (if (list? coords) coords (vector->list coords))))
    (when (not (= (length coords-list) 8))
      (error 'make-e8-point "E8 point must have 8 coordinates"))
    (let ((norm-sq (for/sum ([x coords-list]) (* x x))))
      (E8-Point coords-list norm-sq))))

;; Convert E8-Point to list
(define (e8-point->list point)
  "Convert E8-Point to list of coordinates"
  (E8-Point-coords point))

;; Convert list to E8-Point
(define (list->e8-point coords)
  "Convert list of coordinates to E8-Point"
  (make-e8-point coords))

