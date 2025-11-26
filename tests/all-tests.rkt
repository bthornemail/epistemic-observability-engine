#lang racket/base

;; All Tests Suite
;; Integration tests for full pipeline

(require rackunit
         rackunit/text-ui
         "substrate-core-tests.rkt"
         "substrate-geometry-tests.rkt"
         "substrate-observability-tests.rkt"
         "substrate-logic-tests.rkt"
         "substrate-inverse-tests.rkt")

(provide
 run-all-tests)

(define all-tests
  (test-suite
   "Epistemic Observability Engine - All Tests"
   all-substrate-core-tests
   all-substrate-geometry-tests
   all-substrate-observability-tests
   all-substrate-logic-tests
   all-substrate-inverse-tests))

(define (run-all-tests)
  "Run all tests and return test results"
  (run-tests all-tests))

(module+ main
  (run-all-tests))

