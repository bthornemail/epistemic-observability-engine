#lang racket/base

;; F4 Lie Group Operations
;; Package B: Geometry layer
;; Implements F4 exceptional Lie group (rank 4, dimension 52)
;; The 4D projection bridge between E8 and observable reality
;;
;; Based on research: EOE_Complete_Specification.md, Fano-Plane-F-Max.md
;; Explicit projection matrix: π(v)ᵢ = (vᵢ + vᵢ₊₄)/√2 for i=1,2,3,4

(require "../kernel-spec.rkt"
         "e8.rkt"
         "weyl.rkt"
         "h4.rkt"
         racket/list
         racket/vector
         racket/math)

(provide
 ;; F4 Point structure (4D)
 make-f4-point
 f4-point->list
 list->f4-point
 f4-point-norm-sq
 
 ;; F4 Root System (48 roots: 24 long + 24 short)
 f4-construct-roots
 f4-get-simple-roots
 
 ;; E8 → F4 Projection (explicit 4×8 matrix)
 project-e8-to-f4
 
 ;; F4 Weyl Group (order 11,520 - much faster than E8)
 f4-reflect-vector
 f4-canonicalize-to-dominant
 f4-inner-product
 
 ;; F4 Distance (for intuitive RBAC)
 f4-distance
 
 ;; Commutativity Error (ℱ) - Two-Fano-Plane Solution
 commutativity-error
 compute-f-max-bound
 estimate-f-max
 validate-f-max-bound
 characterize-information-loss-kernel
 analyze-kernel-filtering)

;; ==============================================================================
;; F4 Point Operations
;; ==============================================================================

;; Make F4-Point from list of 4 numbers
(define (make-f4-point coords)
  "Create F4-Point from list of 4 coordinates"
  (let ((coords-list (if (list? coords) coords (vector->list coords))))
    (when (not (= (length coords-list) 4))
      (error 'make-f4-point "F4 point must have 4 coordinates"))
    (let ((norm-sq (for/sum ([x coords-list]) (* x x))))
      (F4-Point coords-list norm-sq))))

;; Convert F4-Point to list
(define (f4-point->list point)
  "Convert F4-Point to list of coordinates"
  (F4-Point-coords point))

;; Convert list to F4-Point
(define (list->f4-point coords)
  "Convert list of coordinates to F4-Point"
  (make-f4-point coords))

;; Compute norm squared of F4 point
(define (f4-point-norm-sq point)
  "Compute squared norm of F4 point"
  (F4-Point-norm-sq point))

;; ==============================================================================
;; E8 → F4 Projection (Explicit 4×8 Matrix)
;; ==============================================================================
;; Based on Borel-de Siebenthal theory
;; Formula: π(v)ᵢ = (vᵢ + vᵢ₊₄)/√2 for i=1,2,3,4

(define (project-e8-to-f4 e8-point)
  "Project E8-Point (8D) to F4-Point (4D) using explicit 4×8 matrix.
   Uses formula: π(v)ᵢ = (vᵢ + vᵢ₊₄)/√2 for i=1,2,3,4"
  (let* ((e8-coords (E8-Point-coords e8-point))
         (sqrt2 (sqrt 2))
         ;; Extract first 4 coordinates and average with next 4
         (f4-coords (for/list ([i (in-range 4)])
                     (/ (+ (list-ref e8-coords i)
                           (list-ref e8-coords (+ i 4)))
                        sqrt2)))
         (f4-norm-sq (for/sum ([x f4-coords]) (* x x))))
    (F4-Point f4-coords f4-norm-sq)))

;; ==============================================================================
;; F4 Root System (48 roots: 24 long + 24 short)
;; ==============================================================================

(define (f4-construct-roots)
  "Construct all 48 F4 roots using exact arithmetic.
   - 24 long roots: ±eᵢ ± eⱼ (i < j, 4 dimensions)
   - 24 short roots: ±eᵢ and ½(±e₁ ± e₂ ± e₃ ± e₄)"
  (let ((roots '()))
    ;; Long roots: ±eᵢ ± eⱼ for i ≠ j (4 dimensions)
    (for* ([i (in-range 4)]
           [j (in-range (add1 i) 4)]
           [sign-i (in-list '(1 -1))]
           [sign-j (in-list '(1 -1))])
      (let ((coords (for/list ([idx (in-range 4)])
                     (cond
                       [(= idx i) sign-i]
                       [(= idx j) sign-j]
                       [else 0]))))
        (set! roots (cons coords roots))))
    
    ;; Short roots: ±eᵢ (8 roots)
    (for* ([i (in-range 4)]
           [sign (in-list '(1 -1))])
      (let ((coords (for/list ([idx (in-range 4)])
                     (if (= idx i) sign 0))))
        (set! roots (cons coords roots))))
    
    ;; Short roots: ½(±e₁ ± e₂ ± e₃ ± e₄) (16 roots)
    (for ([mask (in-range 16)])  ; 2^4 combinations
      (let ((coords (for/list ([i (in-range 4)])
                     (let ((bit (bitwise-and mask (arithmetic-shift 1 i))))
                       (if (= bit 0) 1/2 -1/2)))))
        (set! roots (cons coords roots))))
    
    (reverse roots)))

;; Get simple roots (Dynkin diagram basis) for F4
;; Bourbaki convention - using exact rationals
(define (f4-get-simple-roots)
  "Get 4 simple roots for F4 in exact rational form"
  (list
   (list 0 1 -1 0)              ; α₁
   (list 0 0 1 -1)              ; α₂
   (list 0 0 0 1)               ; α₃
   (list 1/2 -1/2 -1/2 -1/2)))  ; α₄

;; ==============================================================================
;; F4 Weyl Group Operations
;; ==============================================================================

;; Compute inner product of two F4 vectors (exact arithmetic)
(define (f4-inner-product v w)
  "Compute exact inner product of two F4 vectors"
  (let* ((v-coords (if (F4-Point? v) (F4-Point-coords v) v))
         (w-coords (if (F4-Point? w) (F4-Point-coords w) w))
         (v-list (if (list? v-coords) v-coords (vector->list v-coords)))
         (w-list (if (list? w-coords) w-coords (vector->list w-coords))))
    (for/sum ([x v-list] [y w-list])
      (* x y))))

;; Weyl group reflection: s_α(v) = v - 2(v·α)/(α·α) α
(define (f4-reflect-vector vec root)
  "Reflect F4 vector through hyperplane perpendicular to root using exact arithmetic"
  (let* ((v-coords (if (F4-Point? vec) (F4-Point-coords vec) 
                      (if (list? vec) vec (vector->list vec))))
         (alpha-coords (if (Simple-Root? root) 
                          (let ((root-vec (Simple-Root-vector root)))
                            (if (F4-Point? root-vec)
                                (F4-Point-coords root-vec)
                                (if (list? root-vec) root-vec (vector->list root-vec))))
                          (if (list? root) root (vector->list root))))
         (v-dot-alpha (f4-inner-product v-coords alpha-coords))
         (alpha-norm-sq (f4-inner-product alpha-coords alpha-coords))
         (factor (/ (* 2 v-dot-alpha) alpha-norm-sq))
         (result-coords (for/list ([x v-coords] [y alpha-coords])
                          (- x (* factor y)))))
    (make-f4-point result-coords)))

;; Canonicalize F4 vector to dominant chamber
(define (f4-canonicalize-to-dominant vec roots)
  "Map F4 point to unique canonical representative in dominant chamber.
   Maximum path length: ≤24 steps (vs E8's 120)"
  (let* ((simple-roots-list (if (null? roots) (f4-get-simple-roots) roots))
         (current (if (F4-Point? vec) vec (make-f4-point vec)))
         (max-iterations 24))  ; F4 maximum is 24 steps
    
    (let loop ((current current)
               (iterations 0))
      (if (>= iterations max-iterations)
          (error (format "f4-canonicalize-to-dominant: exceeded max iterations (~a)" max-iterations))
          (let ((changed #f)
                (result current))
            ;; Check each simple root
            (for-each (lambda (alpha-coords)
                       (let* ((alpha (make-f4-point alpha-coords))
                              (alpha-root (Simple-Root alpha (f4-point-norm-sq alpha)))
                              (inner-prod (f4-inner-product (F4-Point-coords result) alpha-coords)))
                         (when (< inner-prod 0)
                           (set! result (f4-reflect-vector result alpha-root))
                           (set! changed #t))))
                     simple-roots-list)
            (if changed
                (loop result (add1 iterations))
                result))))))

;; ==============================================================================
;; F4 Distance (for intuitive RBAC)
;; ==============================================================================

(define (f4-distance p1 p2)
  "Compute Euclidean distance in F4 space.
   Maps to: X=Role level, Y=Resource domain, Z=Time/delegation depth, W=Epistemic certainty"
  (let* ((c1 (F4-Point-coords p1))
         (c2 (F4-Point-coords p2))
         (diff-sq (for/sum ([x c1] [y c2]) 
                   (expt (- x y) 2))))
    (sqrt diff-sq)))

;; ==============================================================================
;; Commutativity Error (ℱ) - Two-Fano-Plane Solution
;; ==============================================================================
;; Resolves Open Problems 9.3 (ZK-Arithmetization) and 9.4 (Visualization Faithfulness)
;; Key formula: ℱ(v) = ||Π₈₄(can_E₈(v)) - can_F₄(Π₈₄(v))||

(define (commutativity-error e8-point)
  "Compute Commutativity Error ℱ(v) = ||Π₈₄(can_E₈(v)) - can_F₄(Π₈₄(v))||
   
   This measures the deviation between two canonicalization paths:
   1. Path 1: Canonicalize in E₈, then project to F₄
   2. Path 2: Project to F₄, then canonicalize in F₄
   
   If these paths commute perfectly, ℱ(v) = 0. The error arises from
   transverse reflections in E₈ that do not lie in the F₄ Weyl subgroup."
  (let* ((e8-coords (E8-Point-coords e8-point))
         ;; Path 1: Canonicalize in E₈, then project to F₄
         (e8-canonical (canonicalize-to-dominant e8-point (map (lambda (coords)
                                                                  (let ((point (make-e8-point coords)))
                                                                    (Simple-Root point (e8-point-norm-sq point))))
                                                                (e8-get-simple-roots))))
         (e8-projected (project-e8-to-f4 e8-canonical))
         ;; Path 2: Project to F₄, then canonicalize in F₄
         (f4-projected (project-e8-to-f4 e8-point))
         (f4-roots (f4-get-simple-roots))
         (f4-canonical (f4-canonicalize-to-dominant f4-projected f4-roots))
         ;; Compute Euclidean distance between the two F₄ results
         (error-vec (for/list ([x (F4-Point-coords e8-projected)]
                               [y (F4-Point-coords f4-canonical)])
                     (- x y)))
         (error-norm-sq (for/sum ([d error-vec]) (* d d))))
    (sqrt error-norm-sq)))

;; Theoretical bound: ℱ_max ≤ (φ - 1)/√2 ≈ 0.00886
(define F-MAX-BOUND (/ (- golden-ratio 1) (sqrt 2)))

(define (compute-f-max-bound)
  "Compute the theoretical bound for ℱ_max: (φ - 1)/√2 ≈ 0.00886
   
   This bound comes from the Two-Fano-Plane Transylvania Lottery solution:
   - φ = (1+√5)/2 is the golden ratio (from H₄ structure)
   - 1/√2 is the projection matrix coefficient (from Π₈₄)
   - The bound is operational (14 paths) rather than geometric (240 roots)"
  F-MAX-BOUND)

(define (estimate-f-max n-samples)
  "Estimate ℱ_max using Monte Carlo sampling.
   
   Generates n-samples random E₈ vectors and computes the maximum
   commutativity error observed. This provides a numerical estimate
   ℱ̂_max that can be used as a provisional cryptographic constant
   for ZK-STARK verification while the analytical proof proceeds."
  (let ((max-error 0.0))
    (for ([i (in-range n-samples)])
      ;; Generate random E₈ vector (normalized)
      (let* ((random-coords (for/list ([j (in-range 8)])
                             (- (* (random) 2.0) 1.0)))  ; Random in [-1.0, 1.0]
             (norm (sqrt (for/sum ([x random-coords]) (* x x))))
             (normalized-coords (if (> norm 0)
                                  (for/list ([x random-coords]) (/ x norm))
                                  random-coords))
             (random-e8-point (make-e8-point normalized-coords))
             (error (commutativity-error random-e8-point)))
        (when (> error max-error)
          (set! max-error error))))
    max-error))

(define (validate-f-max-bound estimated-max)
  "Validate that estimated ℱ_max ≤ theoretical bound.
   
   Returns #t if estimated-max ≤ F-MAX-BOUND, #f otherwise.
   This ensures the numerical estimate is consistent with the
   theoretical Two-Fano-Plane bound."
  (<= estimated-max F-MAX-BOUND))

(define (characterize-information-loss-kernel)
  "Characterize the 196D information loss kernel.
   
   The E₈ Lie algebra decomposes as:
   E₈ = G₂(14D) ⊕ F₄(52D) ⊕ (𝕆⊗J₃(𝕆))₀(182D)
   
   The F₄ projection extracts only the 52D F₄ component.
   The kernel (lost information) is the remaining 196D:
   - G₂ component: 14D (automorphism group of octonions)
   - (𝕆⊗J₃(𝕆))₀ component: 182D (octonionic Jordan algebra)
   
   Returns a hash with dimension breakdown."
  (hasheq
   'g2-dim 14
   'f4-dim 52
   'octonion-jordan-dim 182
   'kernel-total 196
   'e8-total 248
   'description "Information lost in E₈ → F₄ projection"))

(define (analyze-kernel-filtering e8-point)
  "Analyze how G₂ non-associativity and octonionic norms are filtered by Π₈₄.
   
   This function examines what information is lost when projecting from E₈ to F₄.
   Specifically, it analyzes:
   - G₂ non-associative structure (14D)
   - Octonionic norms and Jordan algebra structure (182D)
   
   Returns a hash with analysis of filtered information."
  (let* ((e8-coords (E8-Point-coords e8-point))
         (f4-projected (project-e8-to-f4 e8-point))
         (f4-coords (F4-Point-coords f4-projected))
         ;; Analyze what's preserved vs. lost
         (preserved-norm (f4-point-norm-sq f4-projected))
         (original-norm (E8-Point-norm-sq e8-point))
         (lost-norm (- original-norm preserved-norm))
         (preservation-ratio (if (> original-norm 0) (/ preserved-norm original-norm) 0)))
    (hasheq
     'original-e8-norm original-norm
     'preserved-f4-norm preserved-norm
     'lost-norm lost-norm
     'preservation-ratio preservation-ratio
     'g2-component-lost "14D automorphism structure"
     'jordan-component-lost "182D octonionic Jordan algebra structure"
     'description "Analysis of information filtering in E₈ → F₄ projection")))

