#lang racket/base

;; Tests for Package: substrate-zk
;; ZK-STARK implementation tests

(require rackunit
         "../substrate-zk/field-selection.rkt"
         "../substrate-zk/arithmetization.rkt"
         "../substrate-zk/circuit.rkt"
         "../substrate-geometry/e8.rkt"
         "../substrate-geometry/f4.rkt"
         "../substrate-geometry/weyl.rkt"
         "../kernel-spec.rkt"
         racket/math)

(provide
 all-substrate-zk-tests)

(define all-substrate-zk-tests
  (test-suite
   "Substrate ZK Tests"
   
   ;; ==============================================================================
   ;; Finite Field Selection Tests
   ;; ==============================================================================
   
   (test-suite "Finite Field Selection"
              (test-case "Find suitable prime (small for testing)"
                         (let ((p (find-suitable-prime 64)))  ; Use smaller bits for testing
                           (check-true (prime? p))
                           (check-true (>= (integer-length p) 64))
                           (check-true (verify-field-requirements p))))
              
              (test-case "Compute √2 in field"
                         (let* ((p (find-suitable-prime 64))
                                (sqrt2 (sqrt2-in-field p)))
                           (check-true (not (not sqrt2)))  ; Should not be #f
                           (check-equal? (modulo (* sqrt2 sqrt2) p) 2)))
              
              (test-case "Verify field requirements"
                         (let ((p (find-suitable-prime 64)))
                           (check-true (verify-field-requirements p))
                           (check-true (= (legendre-symbol 2 p) 1)))))
   
   ;; ==============================================================================
   ;; Polynomial Arithmetization Tests
   ;; ==============================================================================
   
   (test-suite "Polynomial Arithmetization"
              (test-case "Weyl reflection polynomial (degree 1)"
                         (let* ((p (find-suitable-prime 64))
                                (v (make-e8-point (list 1 2 3 4 5 6 7 8)))
                                (roots (e8-get-simple-roots))
                                (alpha (first roots))
                                (constraint (weyl-reflection-polynomial v alpha p)))
                           (check-true (Polynomial-Constraint? constraint))
                           (check-equal? (Polynomial-Constraint-degree constraint) 1)))
              
              (test-case "F₄ canonicalization trace polynomial"
                         (let* ((p (find-suitable-prime 64))
                                (v (make-e8-point (list 1 2 3 4 5 6 7 8)))
                                (f4-v (project-e8-to-f4 v))
                                (trace-result (f4-canonicalize-with-trace f4-v (f4-get-simple-roots)))
                                (trace (cdr trace-result))
                                (constraints (f4-canonicalization-trace-polynomial 
                                             (F4-Point-coords f4-v) trace p)))
                           (check-true (list? constraints))
                           (check-true (<= (length constraints) 24))))  ; F₄ max is 24 steps
              
              (test-case "Commutativity error polynomial (degree 2)"
                         (let* ((p (find-suitable-prime 64))
                                (v (make-e8-point (list 1 2 3 4 5 6 7 8)))
                                (f-max (compute-f-max-bound))
                                (f-max-sq (* f-max f-max))
                                (constraint (commutativity-error-polynomial v f-max-sq p)))
                           (check-true (Polynomial-Constraint? constraint))
                           (check-equal? (Polynomial-Constraint-degree constraint) 2)))
              
              (test-case "Verify F₄ trace"
                         (let* ((v (make-e8-point (list 1 2 3 4 5 6 7 8)))
                                (f4-v (project-e8-to-f4 v))
                                (trace-result (f4-canonicalize-with-trace f4-v (f4-get-simple-roots)))
                                (f4-canonical (car trace-result))
                                (trace (cdr trace-result)))
                           (check-true (verify-f4-trace 
                                       (F4-Point-coords f4-v)
                                       (F4-Point-coords f4-canonical)
                                       trace)))
              
              (test-case "Verify commutativity bound"
                         (let* ((v (make-e8-point (list 1 2 3 4 5 6 7 8)))
                                (f-max (compute-f-max-bound)))
                           (check-true (verify-commutativity-bound v f-max)))))
   
   ;; ==============================================================================
   ;; ZK Circuit Tests
   ;; ==============================================================================
   
   (test-suite "ZK Circuit"
              (test-case "F₄ canonicalize with trace"
                         (let* ((v (make-e8-point (list 1 2 3 4 5 6 7 8)))
                                (f4-v (project-e8-to-f4 v))
                                (result (f4-canonicalize-with-trace f4-v (f4-get-simple-roots)))
                                (canonical (car result))
                                (trace (cdr result)))
                           (check-true (F4-Point? canonical))
                           (check-true (list? trace))
                           (check-true (<= (length trace) 24))))
              
              (test-case "Generate proof"
                         (let* ((v (make-e8-point (list 1 2 3 4 5 6 7 8)))
                                (f4-v (project-e8-to-f4 v))
                                (f4-canonical (f4-canonicalize-to-dominant f4-v (f4-get-simple-roots)))
                                (p (find-suitable-prime 64))
                                (proof (generate-proof v f4-canonical p)))
                           (check-true (ZK-Proof? proof))
                           (check-true (<= (length (ZK-Proof-trace proof)) 24))
                           (check-true (real? (ZK-Proof-f-max proof)))))
              
              (test-case "Verify proof"
                         (let* ((v (make-e8-point (list 1 2 3 4 5 6 7 8)))
                                (f4-v (project-e8-to-f4 v))
                                (f4-canonical (f4-canonicalize-to-dominant f4-v (f4-get-simple-roots)))
                                (p (find-suitable-prime 64))
                                (proof (generate-proof v f4-canonical p))
                                (is-valid (verify-proof proof)))
                           (check-true (boolean? is-valid))))
              
              (test-case "ZK canonicalization protocol"
                         (let* ((v (make-e8-point (list 1 2 3 4 5 6 7 8)))
                                (p (find-suitable-prime 64))
                                (result (zk-canonicalization-protocol v p))
                                (canonical (car result))
                                (proof (cdr result)))
                           (check-true (F4-Point? canonical))
                           (check-true (ZK-Proof? proof))
                           (check-true (verify-proof proof)))))))

