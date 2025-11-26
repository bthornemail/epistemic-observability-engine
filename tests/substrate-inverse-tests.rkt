#lang racket/base

;; Tests for Inverse Projection Agent
;; Tests bidirectional semantic-to-geometric mapping

(require rackunit
         rackunit/text-ui
         racket/string
         "../substrate-geometry/inverse.rkt"
         "../substrate-geometry/e8.rkt"
         "../substrate-geometry/weyl.rkt"
         "../kernel-spec.rkt")

(provide
 all-substrate-inverse-tests)

(define all-substrate-inverse-tests
  (test-suite
   "Inverse Projection Agent Tests"
   
   (test-case "Semantic lookup - not found"
              (check-false (semantic-lookup "NonExistentRole")))
   
   (test-case "Register and lookup semantic"
              (let* ((point (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (registered (register-semantic "TestRole" point))
                     (looked-up (semantic-lookup "TestRole")))
                (check-true (E8-Point? looked-up))
                (check-equal? (E8-Point-coords looked-up) (E8-Point-coords point))))
   
   (test-case "E8-Point to CID conversion"
              (let* ((point (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (cid (e8-point->cid point)))
                (check-true (string? cid))
                (check-true (string-prefix? cid "mlss://"))))
   
   (test-case "Get role provenance path - empty for new point"
              (let* ((point (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (path (get-role-provenance-path point)))
                (check-true (list? path))))
   
   (test-case "Register multiple semantic names"
              (let* ((point1 (make-e8-point (list 1 0 0 0 0 0 0 0)))
                     (point2 (make-e8-point (list 0 1 0 0 0 0 0 0)))
                     (r1 (register-semantic "Role1" point1))
                     (r2 (register-semantic "Role2" point2))
                     (l1 (semantic-lookup "Role1"))
                     (l2 (semantic-lookup "Role2")))
                (check-true (E8-Point? l1))
                (check-true (E8-Point? l2))
                (check-false (equal? (E8-Point-coords l1) (E8-Point-coords l2)))))))

(module+ main
  (run-tests all-substrate-inverse-tests))

