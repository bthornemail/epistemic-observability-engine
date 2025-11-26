#lang racket/base

;; Error Handling Utilities
;; Production-grade error types and handling

(provide
 exn:epistemic-observability?
 raise-epistemic-error
 raise-validation-error
 raise-computation-error
 handle-errors)

;; Custom exception type for epistemic observability errors
(struct exn:epistemic-observability exn:fail (context)
  #:transparent)

;; Raise epistemic observability error
(define (raise-epistemic-error message [context #f])
  (raise (exn:epistemic-observability message (current-continuation-marks) context)))

;; Raise validation error
(define (raise-validation-error message)
  (raise-epistemic-error (string-append "Validation error: " message)))

;; Raise computation error
(define (raise-computation-error message)
  (raise-epistemic-error (string-append "Computation error: " message)))

;; Generic error handler wrapper
(define (handle-errors thunk)
  (with-handlers ([exn:epistemic-observability?
                   (lambda (e)
                     (printf "Epistemic Observability Error: ~a\n" (exn-message e))
                     (raise e))]
                  [exn:fail?
                   (lambda (e)
                     (printf "Error: ~a\n" (exn-message e))
                     (raise e))])
    (thunk)))

