#lang racket/base

;; Geometric RBAC (Role-Based Access Control)
;; Package C: Logic layer
;; Implements access control using E8Point distance and BIP32 paths

(require "../kernel-spec.rkt"
         "../substrate-geometry/e8.rkt"
         "../substrate-geometry/f4.rkt"
         "../substrate-geometry/projection.rkt"
         "../substrate-geometry/weyl.rkt")

(provide
 geometric-rbac-check
 make-access-grant
 e8-distance
 f4-rbac-distance)  ; 4D F₄ distance for intuitive RBAC

;; Compute distance between two E8 points
(define (e8-distance p1 p2)
  "Compute distance between two E8 points"
  (let* ((coords1 (E8-Point-coords p1))
         (coords2 (E8-Point-coords p2))
         (diff (for/list ([x coords1] [y coords2])
                 (- x y)))
         (dist-sq (for/sum ([d diff])
                    (* d d))))
    (sqrt dist-sq)))

;; Check if access grant allows access to E8 point
(define (geometric-rbac-check grant vec)
  "Check if access grant allows access to E8 vector based on geometric distance"
  (let* ((grant-path (Access-Grant-e8-path grant))
         (grant-point (make-e8-point grant-path))
         (target-point (if (E8-Point? vec) vec (make-e8-point vec)))
         (distance (e8-distance grant-point target-point))
         (threshold 10.0))  ; Distance threshold for access
    ;; Check if distance is within threshold and grant hasn't expired
    (and (< distance threshold)
         (> (Access-Grant-expiry-time grant) (get-current-seconds)))))

;; Create access grant from E8 path, role CID, and expiry
(define (make-access-grant e8-path role-cid expiry-time)
  "Create Access-Grant from E8 path, role CID, and expiry time"
  (Access-Grant e8-path role-cid expiry-time))

;; Helper: Get current Unix timestamp
(require racket/date)
(define (get-current-seconds)
  "Get current Unix timestamp"
  (date->seconds (current-date)))

;; ==============================================================================
;; F₄ RBAC Distance (4D - Intuitive for Humans)
;; ==============================================================================

(define (f4-rbac-distance role1 role2)
  "Compute F₄ distance between two roles for intuitive RBAC.
   Maps to: X=Role level, Y=Resource domain, Z=Time/delegation depth, W=Epistemic certainty.
   Projects E₈ points to F₄ (4D) for human-perceivable distance calculation"
  (let* ((e8-point1 (if (E8-Point? role1) role1 (make-e8-point role1)))
         (e8-point2 (if (E8-Point? role2) role2 (make-e8-point role2)))
         (f4-point1 (project-e8-to-f4 e8-point1))
         (f4-point2 (project-e8-to-f4 e8-point2)))
    (f4-distance f4-point1 f4-point2)))

