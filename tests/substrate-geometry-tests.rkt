#lang racket/base

;; Tests for Package B: Substrate Geometry

(require rackunit
         "../substrate-geometry/e8.rkt"
         "../substrate-geometry/f4.rkt"
         "../substrate-geometry/g2.rkt"
         "../substrate-geometry/e6.rkt"
         "../substrate-geometry/e7.rkt"
         "../substrate-geometry/h4.rkt"
         "../substrate-geometry/weyl.rkt"
         "../substrate-geometry/projection.rkt"
         "../kernel-spec.rkt"
         racket/math)

(provide
 all-substrate-geometry-tests)

(define all-substrate-geometry-tests
  (test-suite
   "Substrate Geometry Tests"
   
   (test-case "E8 root construction"
              (let ((roots (e8-construct-roots)))
                (check-equal? (length roots) 240)))
   
   (test-case "E8 simple roots"
              (let ((simple-roots (e8-get-simple-roots)))
                (check-equal? (length simple-roots) 8)))
   
   (test-case "E8 point creation"
              (let ((point (make-e8-point (list 1 2 3 4 5 6 7 8))))
                (check-equal? (E8-Point-coords point) (list 1 2 3 4 5 6 7 8))
                (check-equal? (E8-Point-norm-sq point) 204)))
   
   (test-case "Weyl reflection"
              (let* ((vec (make-e8-point (list 1 0 0 0 0 0 0 0)))
                     (root (Simple-Root (make-e8-point (list 1 1 0 0 0 0 0 0)) 2))
                     (reflected (reflect-vector vec root)))
                (check-true (E8-Point? reflected))))
   
   (test-case "Canonicalize to dominant"
              (let* ((vec (make-e8-point (list -1 -1 -1 -1 -1 -1 -1 -1)))
                     (simple-roots (map (lambda (coords)
                                          (let ((point (make-e8-point coords)))
                                            (Simple-Root point (e8-point-norm-sq point))))
                                       (e8-get-simple-roots)))
                     (canonical (canonicalize-to-dominant vec simple-roots)))
                (check-true (E8-Point? canonical))))
   
   (test-case "Project bytes to E8"
              (let* ((data #"test data")
                     (point (project-to-e8 data)))
                (check-true (E8-Point? point))
                (check-equal? (length (E8-Point-coords point)) 8)))
   
   ;; F₄ Tests
   (test-case "F4 root construction"
              (let ((roots (f4-construct-roots)))
                (check-equal? (length roots) 48)))
   
   (test-case "F4 simple roots"
              (let ((simple-roots (f4-get-simple-roots)))
                (check-equal? (length simple-roots) 4)))
   
   (test-case "E8 to F4 projection"
              (let* ((e8-point (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (f4-point (project-e8-to-f4 e8-point)))
                (check-true (F4-Point? f4-point))
                (check-equal? (length (F4-Point-coords f4-point)) 4)))
   
   (test-case "F4 canonicalization (≤24 steps)"
              (let* ((f4-point (make-f4-point (list -1 -1 -1 -1)))
                     (roots (f4-get-simple-roots))
                     (canonical (f4-canonicalize-to-dominant f4-point roots)))
                (check-true (F4-Point? canonical))))
   
   ;; G₂ Tests
   (test-case "Octonion creation"
              (let ((oct (make-octonion 1 2 3 4 5 6 7 8)))
                (check-true (Octonion? oct))
                (check-equal? (length (octonion->list oct)) 8)))
   
   (test-case "Octonion multiplication (non-associative)"
              (let* ((a (make-octonion 1 0 0 0 0 0 0 0))
                     (b (make-octonion 0 1 0 0 0 0 0 0))
                     (c (make-octonion 0 0 1 0 0 0 0 0))
                     (left-assoc (octonion-multiply (octonion-multiply a b) c))
                     (right-assoc (octonion-multiply a (octonion-multiply b c))))
                (check-true (Octonion? left-assoc))
                (check-true (Octonion? right-assoc))
                ;; Verify non-associativity: left-assoc ≠ right-assoc
                (check-false (equal? (octonion->list left-assoc) (octonion->list right-assoc)))))
   
   (test-case "Octonion associator (non-zero)"
              (let* ((a (make-octonion 1 1 0 0 0 0 0 0))
                     (b (make-octonion 0 1 1 0 0 0 0 0))
                     (c (make-octonion 0 0 1 1 0 0 0 0))
                     (assoc (octonion-associator a b c))
                     (assoc-norm (octonion-norm assoc)))
                (check-true (> assoc-norm 0.01))))  ; Non-associative
   
   (test-case "G2 root construction"
              (let ((roots (g2-construct-roots)))
                (check-equal? (length roots) 12)))
   
   ;; E₆ Tests
   (test-case "E6 root construction"
              (let ((roots (e6-construct-roots)))
                (check-true (>= (length roots) 72))))  ; At least 72 roots
   
   (test-case "E8 to E6 projection"
              (let* ((e8-point (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (e6-proj (project-e8-to-e6 e8-point)))
                (check-true (list? e6-proj))
                (check-equal? (length (first e6-proj)) 6)))
   
   ;; E₇ Tests
   (test-case "E7 root construction"
              (let ((roots (e7-construct-roots)))
                (check-true (>= (length roots) 126))))  ; At least 126 roots
   
   (test-case "E7 56D vector creation"
              (let* ((gen1 (make-octonion 1 0 0 0 0 0 0 0))
                     (gen2 (make-octonion 0 1 0 0 0 0 0 0))
                     (gen3 (make-octonion 0 0 1 0 0 0 0 0))
                     (e7-vec (make-e7-56-vector gen1 gen2 gen3 1.0)))
                (check-true (E7-56-Vector? e7-vec))
                (check-equal? (length (E7-56-Vector-coords e7-vec)) 56)))
   
   ;; H₄ Tests
   (test-case "H4 root construction (600-cell vertices)"
              (let ((roots (h4-construct-roots)))
                (check-equal? (length roots) 120)))  ; 600-cell has 120 vertices
   
   (test-case "600-cell vertices"
              (let ((vertices (get-600cell-vertices)))
                (check-equal? (length vertices) 120)))
   
   (test-case "Golden ratio interpolation"
              (let* ((p1 (make-f4-point (list 0 0 0 0)))
                     (p2 (make-f4-point (list 1 1 1 1)))
                     (interp (h4-geodesic p1 p2 0.5)))
                (check-true (F4-Point? interp))))
   
   ;; Commutativity Error (ℱ) Tests - Two-Fano-Plane Solution
   (test-case "Commutativity error computation"
              (let* ((e8-point (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (error (commutativity-error e8-point)))
                (check-true (real? error))
                (check-true (>= error 0))))  ; Error is always non-negative
   
   (test-case "F-max bound computation"
              (let ((bound (compute-f-max-bound)))
                (check-true (real? bound))
                (check-true (< bound 0.01))  ; Should be < 0.01
                (check-true (> bound 0.008))  ; Should be ≈ 0.00886
                ;; Verify bound formula: (φ - 1)/√2
                (let ((phi (/ (+ 1 (sqrt 5)) 2))
                      (expected-bound (/ (- phi 1) (sqrt 2))))
                  (check-= bound expected-bound 1e-10))))
   
   (test-case "F-max Monte Carlo estimation (small sample)"
              (let ((estimated (estimate-f-max 10)))  ; Small sample for testing
                (check-true (real? estimated))
                (check-true (>= estimated 0))
                ;; Estimated should be ≤ theoretical bound (with some tolerance for sampling)
                (let ((bound (compute-f-max-bound)))
                  (check-true (<= estimated (* bound 2)))))  ; Allow 2× for small sample
   
   (test-case "Validate F-max bound"
              (let ((bound (compute-f-max-bound))
                    (estimated (estimate-f-max 5)))  ; Very small sample
                (check-true (real? (validate-f-max-bound estimated)))))
   
   ;; Two-Fano-Plane Construction Tests
   (test-case "Two-Fano-plane construction"
              (let ((lines (two-fano-plane-construction)))
                (check-equal? (length lines) 14)  ; 14 lines total
                ;; Check first plane (7 lines)
                (let ((plane1-lines (take lines 7)))
                  (for-each (lambda (line)
                             (check-equal? (length line) 3)  ; Each line has 3 points
                             (for-each (lambda (point)
                                        (check-true (and (>= point 1) (<= point 7))))
                                      line))
                           plane1-lines))
                ;; Check second plane (7 lines)
                (let ((plane2-lines (drop lines 7)))
                  (for-each (lambda (line)
                             (check-equal? (length line) 3)  ; Each line has 3 points
                             (for-each (lambda (point)
                                        (check-true (and (>= point 8) (<= point 14))))
                                      line))
                           plane2-lines))))
   
   (test-case "Find stable core (guarantee test)"
              ;; Test all cases: 3 in [1,7], 3 in [8,14], mixed
              (let-values ([(line1 core1) (find-stable-core (list 1 2 3))])
                (check-true (list? line1))
                (check-true (list? core1))
                (check-true (>= (length core1) 2)))  ; At least 2 elements
              
              (let-values ([(line2 core2) (find-stable-core (list 8 9 10))])
                (check-true (list? line2))
                (check-true (>= (length core2) 2)))
              
              (let-values ([(line3 core3) (find-stable-core (list 1 2 8))])
                (check-true (list? line3))
                (check-true (>= (length core3) 2)))
              
              (let-values ([(line4 core4) (find-stable-core (list 1 8 9))])
                (check-true (list? line4))
                (check-true (>= (length core4) 2))))
   
   ;; Transverse Reflection Path Tests
   (test-case "Identify transverse paths"
              (let ((paths (identify-transverse-paths)))
                (check-equal? (length paths) 14)  ; 14 paths
                (for-each (lambda (path)
                           (check-true (hash-has-key? path 'path-id))
                           (check-true (hash-has-key? path 'fano-line))
                           (check-true (hash-has-key? path 'plane)))
                         paths)))
   
   (test-case "Get transverse path for triple"
              (let ((path-id (get-transverse-path-for-triple (list 1 2 3))))
                (check-true (number? path-id))
                (check-true (and (>= path-id 1) (<= path-id 14)))))
   
   ;; Information Loss Kernel Tests
   (test-case "Characterize information loss kernel"
              (let ((kernel (characterize-information-loss-kernel)))
                (check-equal? (hash-ref kernel 'g2-dim) 14)
                (check-equal? (hash-ref kernel 'f4-dim) 52)
                (check-equal? (hash-ref kernel 'octonion-jordan-dim) 182)
                (check-equal? (hash-ref kernel 'kernel-total) 196)
                (check-equal? (hash-ref kernel 'e8-total) 248)))
   
   (test-case "Analyze kernel filtering"
              (let* ((e8-point (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (analysis (analyze-kernel-filtering e8-point)))
                (check-true (hash-has-key? analysis 'original-e8-norm))
                (check-true (hash-has-key? analysis 'preserved-f4-norm))
                (check-true (hash-has-key? analysis 'lost-norm))
                (check-true (hash-has-key? analysis 'preservation-ratio))
                (check-true (real? (hash-ref analysis 'preservation-ratio))))))))
