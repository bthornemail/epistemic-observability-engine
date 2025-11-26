#lang racket/base

;; Integration Tests
;; Full pipeline tests for RPC server and end-to-end workflows

(require rackunit
         rackunit/text-ui
         "../rpc/server.rkt"
         "../rpc/handlers.rkt"
         "../substrate-geometry/e8.rkt"
         "../substrate-geometry/f4.rkt"
         "../substrate-geometry/g2.rkt"
         "../substrate-geometry/e6.rkt"
         "../substrate-geometry/e7.rkt"
         "../substrate-geometry/h4.rkt"
         "../substrate-geometry/projection.rkt"
         "../substrate-observability/epistemic-vector.rkt"
         "../substrate-observability/qstar.rkt"
         "../kernel-spec.rkt")

(provide
 all-integration-tests)

(define all-integration-tests
  (test-suite
   "Integration Tests"
   
   (test-case "Full pipeline: canonicalize -> parameterize -> optimize"
              (let* ((vec (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (canonicalized (handle-rpc-canonicalize vec))
                     (epistemic-vec (make-epistemic-vector 1.0 2.0 3.0 4.0))
                     (actions (list "action1" "action2")))
                (check-true (E8-Point? canonicalized))
                (check-true (Q*-Result? (optimize-action canonicalized actions)))))
   
   (test-case "RPC handler: canonicalize"
              (let* ((vec (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (result (handle-rpc-canonicalize vec)))
                (check-true (E8-Point? result))))
   
   (test-case "RPC handler: grant access"
              (let* ((agent-vec (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (resource-vec (make-e8-point (list 2 3 4 5 6 7 8 9)))
                     (grant (handle-rpc-grant-access agent-vec resource-vec)))
                (check-true (Access-Grant? grant))))
   
   (test-case "RPC handler: evaluate Q"
              (let* ((vec (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (action "test-action")
                     (result (handle-rpc-evaluate-q vec action)))
                (check-true (Q*-Result? result)))
   
   ;; Full Dimensional Descent Chain Tests
   (test-case "E8 → E7 → E6 → F4 chain"
              (let* ((e8-point (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (e7-vec (project-e8-to-e7 e8-point))
                     (e6-proj (project-e8-to-e6 e8-point))
                     (f4-point (project-e8-to-f4 e8-point)))
                (check-true (E7-56-Vector? e7-vec))
                (check-true (list? e6-proj))
                (check-true (F4-Point? f4-point))))
   
   (test-case "Fast canonicalization path (F4)"
              (let* ((e8-point (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (fast-canon (canonicalize-fast e8-point)))
                (check-true (E8-Point? fast-canon))))
   
   ;; F₄ RPC Tests
   (test-case "RPC: project to F4"
              (let* ((e8-point (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (result (handle-rpc-project-to-f4 e8-point)))
                (check-true (hash? result))
                (check-true (hash-has-key? result 'f4-coords))))
   
   (test-case "RPC: F4 distance"
              (let* ((role1 (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (role2 (make-e8-point (list 2 3 4 5 6 7 8 9)))
                     (result (handle-rpc-f4-distance role1 role2)))
                (check-true (hash? result))
                (check-true (hash-has-key? result 'distance))))
   
   ;; G₂ RPC Tests
   (test-case "RPC: update UK state (non-associative)"
              (let* ((current (make-octonion 1 0 0 0 0 0 0 0))
                     (neighbor (make-octonion 0 1 0 0 0 0 0 0))
                     (result (handle-rpc-update-uk-state current neighbor)))
                (check-true (hash? result))
                (check-true (hash-ref result 'non-associative))))
   
   ;; E₇ RPC Tests
   (test-case "RPC: project to E7 56D"
              (let* ((e8-point (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (result (handle-rpc-project-to-e7-56 e8-point)))
                (check-true (hash? result))
                (check-equal? (hash-ref result 'dimension) 56)))
   
   ;; H₄ RPC Tests
   (test-case "RPC: zoom role (golden-ratio)"
              (let* ((role-path (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (result (handle-rpc-zoom-role role-path 0.5)))
                (check-true (hash? result))
                (check-true (hash-has-key? result 'zoomed-coords))))
   
   (test-case "RPC: render 600-cell"
              (let* ((state (make-e8-point (list 1 2 3 4 5 6 7 8)))
                     (result (handle-rpc-render-600cell state)))
                (check-true (hash? result))
                (check-equal? (hash-ref result 'vertex-count) 120)
                (check-true (hash-has-key? result 'golden-ratio))))))

(module+ main
  (run-tests all-integration-tests))

