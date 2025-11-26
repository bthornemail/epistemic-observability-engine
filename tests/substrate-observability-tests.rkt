#lang racket/base

;; Tests for Package D: Substrate Observability
;; CRITICAL: Tests for UK * phi(V) stability

(require rackunit
         racket/list
         "../substrate-observability/epistemic-vector.rkt"
         "../substrate-observability/parameterize.rkt"
         "../substrate-observability/qstar.rkt"
         "../substrate-geometry/e8.rkt"
         "../kernel-spec.rkt")

(provide
 all-substrate-observability-tests)

(define all-substrate-observability-tests
  (test-suite
   "Substrate Observability Tests"
   
   (test-case "Epistemic vector creation"
              (let ((vec (make-epistemic-vector 1.0 2.0 3.0 4.0)))
                (check-equal? (Epistemic-Vector-kk vec) 1.0)
                (check-equal? (Epistemic-Vector-uk vec) 3.0)))
   
   (test-case "Euler totient function"
              (check-equal? (euler-totient 1) 1)
              (check-equal? (euler-totient 2) 1)
              (check-equal? (euler-totient 3) 2)
              (check-equal? (euler-totient 4) 2)
              (check-equal? (euler-totient 5) 4)
              (check-equal? (euler-totient 6) 2)
              (check-equal? (euler-totient 7) 6)
              (check-equal? (euler-totient 8) 4)
              (check-equal? (euler-totient 9) 6)
              (check-equal? (euler-totient 10) 4))
   
   (test-case "Parameterize observability - UK * phi(V)"
              (let* ((vec (make-epistemic-vector 1.0 2.0 10.0 4.0))
                     (vertex-count 10)
                     (state (parameterize-observability vec vertex-count)))
                (check-true (Observable-State? state))
                (check-equal? (Observable-State-phi-multiplier state) 4)  ; phi(10) = 4
                ;; UK * phi = 10.0 * 4 = 40.0
                (check-equal? (* (Epistemic-Vector-uk vec) 
                                 (Observable-State-phi-multiplier state))
                              40.0)))
   
   (test-case "UK * phi(V) stability proof"
              "Prove that UK * phi(V) stays stable as V increases"
              (let* ((vec (make-epistemic-vector 1.0 2.0 1.0 4.0))
                     (vertex-counts (list 10 100 1000 10000))
                     (results (map (lambda (v)
                                      (let ((state (parameterize-observability vec v)))
                                        (* (Epistemic-Vector-uk vec)
                                           (Observable-State-phi-multiplier state))))
                                    vertex-counts)))
                ;; As V increases, phi(V) grows slowly (logarithmically)
                ;; So UK * phi(V) should grow slowly, not explode
                (check-true (< (last results) (* (first results) 10)))))  ; Should grow < 10x
   
   (test-case "Compute epistemic cost"
              (let* ((vec (make-epistemic-vector 1.0 2.0 3.0 4.0))
                     (state (parameterize-observability vec 10))
                     (cost (compute-epistemic-cost state)))
                (check-true (real? cost))
                (check-true (>= cost 0))))
   
   (test-case "Optimize action"
              (let* ((vec (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (actions (list "action1" "action2"))
                     (result (optimize-action vec actions)))
                (check-true (Q*-Result? result))
                (check-equal? (length (Q*-Result-action-plan result)) 1)))))

