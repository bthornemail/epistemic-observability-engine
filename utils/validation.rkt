#lang racket/base

;; Input Validation Utilities
;; Production-grade input validation for all modules

(require "errors.rkt"
         "../kernel-spec.rkt"
         racket/string)

(provide
 validate-e8-point
 validate-epistemic-vector
 validate-cbs-id
 validate-actions-list
 validate-vertex-count)

;; Validate E8-Point
(define (validate-e8-point x)
  "Validate that x is a valid E8-Point with 8 coordinates"
  (if (not (E8-Point? x))
      (raise-validation-error "Expected E8-Point")
      (let ((coords (E8-Point-coords x)))
        (if (not (= (length coords) 8))
            (raise-validation-error "E8-Point must have exactly 8 coordinates")
            x))))

;; Validate Epistemic-Vector
(define (validate-epistemic-vector x)
  "Validate that x is a valid Epistemic-Vector"
  (if (not (Epistemic-Vector? x))
      (raise-validation-error "Expected Epistemic-Vector")
      x))

;; Validate CBS-ID
(define (validate-cbs-id x)
  "Validate that x is a valid CBS-ID (mlss:// format)"
  (if (not (string? x))
      (raise-validation-error "CBS-ID must be a string")
      (if (not (string-prefix? x "mlss://"))
          (raise-validation-error "CBS-ID must start with 'mlss://'")
          x)))

;; Validate actions list
(define (validate-actions-list x)
  "Validate that x is a non-empty list of strings"
  (if (not (list? x))
      (raise-validation-error "Actions must be a list")
      (if (null? x)
          (raise-validation-error "Actions list cannot be empty")
          (if (not (andmap string? x))
              (raise-validation-error "All actions must be strings")
              x))))

;; Validate vertex count
(define (validate-vertex-count x)
  "Validate that x is a positive real number"
  (if (not (real? x))
      (raise-validation-error "Vertex count must be a real number")
      (if (not (positive? x))
          (raise-validation-error "Vertex count must be positive")
          x)))

