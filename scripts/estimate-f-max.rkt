#lang racket/base

;; Script to estimate ℱ_max using Monte Carlo sampling
;; Usage: racket scripts/estimate-f-max.rkt [n-samples]

(require "../substrate-geometry/f4.rkt"
         "../substrate-geometry/e8.rkt"
         racket/format)

(define n-samples
  (if (> (vector-length (current-command-line-arguments)) 0)
      (string->number (vector-ref (current-command-line-arguments) 0))
      10000))  ; Default: 10,000 samples

(printf "Estimating ℱ_max with ~a samples...\n" n-samples)
(printf "This may take a while for large sample sizes...\n\n")

(define start-time (current-inexact-milliseconds))
(define estimated-max (estimate-f-max n-samples))
(define end-time (current-inexact-milliseconds))
(define elapsed-time (- end-time start-time))

(define theoretical-bound (compute-f-max-bound))
(define is-valid (validate-f-max-bound estimated-max))

(printf "============================================================\n")
(printf "RESULTS\n")
(printf "============================================================\n")
(printf "\n")
(printf "Samples:              ~a\n" n-samples)
(printf "Estimated ℱ_max:      ~a\n" (real->decimal-string estimated-max 8))
(printf "Theoretical bound:    ~a\n" (real->decimal-string theoretical-bound 8))
(printf "Bound ratio:          ~a\n" (real->decimal-string (/ estimated-max theoretical-bound) 4))
(printf "Valid (≤ bound):      ~a\n" (if is-valid "✓ YES" "✗ NO"))
(printf "Elapsed time:         ~a ms\n" (real->decimal-string elapsed-time 2))
(printf "\n")

(if is-valid
    (begin
      (printf "✓ SUCCESS: Estimated ℱ_max is within theoretical bound!\n"))
    (begin
      (printf "⚠ WARNING: Estimated ℱ_max exceeds theoretical bound.\n")
      (printf "  This may indicate:\n")
      (printf "  - Sample size too small\n")
      (printf "  - Need to refine theoretical bound\n")
      (printf "  - Numerical precision issues\n")))

(printf "\n")
(printf "For ZK-STARK, use ℱ_max = ~a\n" (real->decimal-string estimated-max 8))
(printf "\n")

