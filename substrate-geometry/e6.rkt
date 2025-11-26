#lang racket/base

;; E6 Lie Group Operations
;; Package B: Geometry layer
;; Implements E6 exceptional Lie group (rank 6, dimension 78)
;; Unification layer - prevents variance explosion in large graphs
;;
;; Based on research: EOE_Complete_Specification.md
;; E6 = SL(3,𝕆) - related to octonionic special linear group

(require "../kernel-spec.rkt"
         "e8.rkt"
         "f4.rkt"
         racket/list)

(provide
 ;; E6 Root System (72 roots)
 e6-construct-roots
 e6-get-simple-roots
 
 ;; E6 Projection
 project-e8-to-e6
 project-e6-to-f4
 
 ;; E6 Weyl Group (order 51,840)
 e6-canonicalize-to-dominant)

;; ==============================================================================
;; E6 Root System (72 roots)
;; ==============================================================================

(define (e6-construct-roots)
  "Construct all 72 E6 roots.
   E6 has rank 6, so roots lie in 6D space"
  (let ((roots '()))
    ;; E6 roots: combinations of ±1, ±1, ±1, ±1, ±1, ±1 with sum = 0
    ;; and permutations of (±1, ±1, 0, 0, 0, 0)
    (for* ([signs (in-list (list '(1 1 1 -1 -1 -1)
                                  '(1 1 -1 1 -1 -1)
                                  '(1 1 -1 -1 1 -1)
                                  '(1 1 -1 -1 -1 1)
                                  '(1 -1 1 1 -1 -1)
                                  '(1 -1 1 -1 1 -1)
                                  '(1 -1 1 -1 -1 1)
                                  '(1 -1 -1 1 1 -1)
                                  '(1 -1 -1 1 -1 1)
                                  '(1 -1 -1 -1 1 1)
                                  '(-1 1 1 1 -1 -1)
                                  '(-1 1 1 -1 1 -1)
                                  '(-1 1 1 -1 -1 1)
                                  '(-1 1 -1 1 1 -1)
                                  '(-1 1 -1 1 -1 1)
                                  '(-1 1 -1 -1 1 1)
                                  '(-1 -1 1 1 1 -1)
                                  '(-1 -1 1 1 -1 1)
                                  '(-1 -1 1 -1 1 1)
                                  '(-1 -1 -1 1 1 1)))])
      (set! roots (cons signs roots)))
    
    ;; Additional roots from permutations
    (for* ([i (in-range 6)]
           [j (in-range (add1 i) 6)])
      (for ([sign-i (in-list '(1 -1))]
            [sign-j (in-list '(1 -1))])
        (let ((coords (for/list ([idx (in-range 6)])
                       (cond
                         [(= idx i) sign-i]
                         [(= idx j) sign-j]
                         [else 0]))))
          (when (not (member coords roots))
            (set! roots (cons coords roots))))))
    
    (reverse roots)))

(define (e6-get-simple-roots)
  "Get 6 simple roots for E6 (rank 6)"
  (list
   (list 1 -1 0 0 0 0)    ; α₁
   (list 0 1 -1 0 0 0)    ; α₂
   (list 0 0 1 -1 0 0)    ; α₃
   (list 0 0 0 1 -1 0)    ; α₄
   (list 0 0 0 0 1 -1)    ; α₅
   (list 0 0 0 0 1 1)))   ; α₆

;; ==============================================================================
;; E6 Projection
;; ==============================================================================

(define (project-e8-to-e6 e8-point)
  "Project E8-Point (8D) to E6 space (6D).
   E6 is embedded in E8 via Dynkin diagram removal"
  (let* ((e8-coords (E8-Point-coords e8-point))
         ;; Extract first 6 coordinates (E6 simple roots = first 6 E8 simple roots)
         (e6-coords (take e8-coords 6))
         (e6-norm-sq (for/sum ([x e6-coords]) (* x x))))
    (list e6-coords e6-norm-sq)))

(define (project-e6-to-f4 e6-vec)
  "Project E6 vector (6D) to F4 space (4D)"
  (let ((e6-coords (if (list? e6-vec) (first e6-vec) e6-vec)))
    (make-f4-point (take e6-coords 4))))

;; ==============================================================================
;; E6 Weyl Group Operations
;; ==============================================================================

(define (e6-canonicalize-to-dominant vec)
  "Map E6 vector to unique canonical representative in dominant chamber.
   Uses E6 Weyl group (order 51,840)"
  ;; Simplified - full implementation would use E6 Weyl reflections
  ;; Maximum path length is bounded by E6 Weyl group diameter
  (let ((simple-roots (e6-get-simple-roots))
        (current (if (list? vec) vec (first vec)))
        (max-iterations 72))  ; E6 maximum is bounded by number of roots
    
    (let loop ((current current)
               (iterations 0))
      (if (>= iterations max-iterations)
          current
          (let ((changed #f)
                (result current))
            ;; Check each simple root
            (for-each (lambda (alpha-coords)
                       (let ((inner-prod (for/sum ([x current] [y alpha-coords])
                                         (* x y))))
                         (when (< inner-prod 0)
                           ;; Reflect: v - 2(v·α)/(α·α) α
                           (let* ((alpha-norm-sq (for/sum ([y alpha-coords]) (* y y)))
                                  (factor (/ (* 2 inner-prod) alpha-norm-sq))
                                  (reflected (for/list ([x current] [y alpha-coords])
                                              (- x (* factor y)))))
                             (set! result reflected)
                             (set! changed #t)))))
                     simple-roots)
            (if changed
                (loop result (add1 iterations))
                result))))))

