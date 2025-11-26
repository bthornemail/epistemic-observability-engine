#lang racket/base

;; Tests for Package C: Substrate Logic

(require rackunit
         "../substrate-logic/dual-pair.rkt"
         "../substrate-logic/access-control.rkt"
         "../substrate-geometry/e8.rkt"
         "../kernel-spec.rkt")

(provide
 all-substrate-logic-tests)

(define all-substrate-logic-tests
  (test-suite
   "Substrate Logic Tests"
   
   (test-case "Dual pair classification - eager (negative discriminant)"
              (let ((pair (make-dual-pair (lambda (x) x) (lambda (x) x) -1)))
                (check-equal? (classify-dual-pair pair) 'eager)))
   
   (test-case "Dual pair classification - lazy (positive discriminant)"
              (let ((pair (make-dual-pair (lambda (x) x) (lambda (x) x) 1)))
                (check-equal? (classify-dual-pair pair) 'lazy)))
   
   (test-case "Dual pair classification - degenerate (zero discriminant)"
              (let ((pair (make-dual-pair (lambda (x) x) (lambda (x) x) 0)))
                (check-equal? (classify-dual-pair pair) 'eager)))
   
   (test-case "Compute discriminant"
              (check-equal? (compute-discriminant 1 2 1) 0)   ; b² - 4ac = 4 - 4 = 0
              (check-equal? (compute-discriminant 1 3 1) 5)   ; 9 - 4 = 5
              (check-equal? (compute-discriminant 1 1 1) -3)) ; 1 - 4 = -3
   
   (test-case "E8 distance"
              (let* ((p1 (make-e8-point (list 0 0 0 0 0 0 0 0)))
                     (p2 (make-e8-point (list 1 0 0 0 0 0 0 0)))
                     (dist (e8-distance p1 p2)))
                (check-equal? dist 1.0)))
   
   (test-case "Geometric RBAC check"
              (let* ((grant-path (list 1 2 3 4 5 6 7 8))
                     (role-cid "test-role")
                     (expiry (+ (get-current-seconds) 3600))
                     (grant (make-access-grant grant-path role-cid expiry))
                     (target-vec (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (allowed (geometric-rbac-check grant target-vec)))
                (check-true allowed)))
   
   (test-case "Geometric RBAC check - expired"
              (let* ((grant-path (list 1 2 3 4 5 6 7 8))
                     (role-cid "test-role")
                     (expiry (- (get-current-seconds) 3600))  ; Expired
                     (grant (make-access-grant grant-path role-cid expiry))
                     (target-vec (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (allowed (geometric-rbac-check grant target-vec)))
                (check-false allowed)))))

;; Helper for tests
(require racket/date)
(define (get-current-seconds)
  (date->seconds (current-date)))

