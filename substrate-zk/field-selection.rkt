#lang racket/base

;; Finite Field Selection for ZK-STARK
;; Package: substrate-zk
;; Implements finite field selection where √2 exists (required for projection matrix Π₈₄)

(require racket/math
         math/number-theory)

(provide
 find-suitable-prime
 sqrt2-in-field
 verify-field-requirements)

;; ==============================================================================
;; Legendre Symbol (for quadratic residue testing)
;; ==============================================================================

(define (legendre-symbol a p)
  "Compute Legendre symbol (a/p) using Euler's criterion.
   
   Returns:
   - 1 if a is quadratic residue mod p
   - -1 if a is quadratic non-residue mod p
   - 0 if p divides a"
  (cond
    [(= (modulo a p) 0) 0]
    [else
     (let ((exponent (/ (- p 1) 2)))
       (modular-expt a (exact-floor exponent) p))]))

;; ==============================================================================
;; Tonelli-Shanks Algorithm (for computing square roots in finite fields)
;; ==============================================================================

(define (tonelli-shanks n p)
  "Compute square root of n modulo prime p using Tonelli-Shanks algorithm.
   
   Returns one of the two square roots, or #f if n is not a quadratic residue."
  (cond
    ;; Check if n is quadratic residue
    [(not (= (legendre-symbol n p) 1)) #f]
    ;; Trivial cases
    [(= (modulo n p) 0) 0]
    [(= p 2) (modulo n 2)]
    [(= (modulo n p) 1) 1]
    [else
     ;; Factor p-1 = Q * 2^S
     (let* ((p-minus-1 (- p 1))
            (s 0)
            (q p-minus-1))
       (let loop-factor ()
         (when (even? q)
           (set! s (+ s 1))
           (set! q (quotient q 2))
           (loop-factor)))
       
       ;; Find quadratic non-residue z
       (define z
         (let find-z ([candidate 2])
           (if (= (legendre-symbol candidate p) -1)
               candidate
               (find-z (+ candidate 1)))))
       
       ;; Initialize with mutable variables
       (let ((m s)
             (c (modular-expt z q p))
             (t (modular-expt n q p))
             (r (modular-expt n (/ (+ q 1) 2) p)))
         ;; Main loop
         (let loop-main ()
           (if (= (modulo t p) 1)
               r
               ;; Find smallest i such that t^(2^i) = 1
               (let find-i ([i 1] [tt (modulo (* t t) p)])
                 (if (= (modulo tt p) 1)
                     (let ((b (modular-expt c (expt 2 (- m i 1)) p)))
                       (set! m i)
                       (set! c (modulo (* b b) p))
                       (set! t (modulo (* t c) p))
                       (set! r (modulo (* r b) p))
                       (loop-main))
                     (find-i (+ i 1) (modulo (* tt tt) p)))))))])))

;; ==============================================================================
;; Find Suitable Prime
;; ==============================================================================

(define (find-suitable-prime [min-bits 256])
  "Find cryptographic prime p where 2 is quadratic residue (√2 exists in 𝔽_p).
   
   Condition: p ≡ ±1 (mod 8) ensures 2 is quadratic residue.
   
   Returns prime p suitable for ZK-STARK with minimum bit length specified."
  (let* ((min-value (expt 2 (- min-bits 1)))
         (max-value (expt 2 min-bits))
         (candidate (next-prime min-value)))
    ;; Check if 2 is quadratic residue (p ≡ ±1 mod 8)
    (let loop ([p candidate])
      (cond
        [(>= p max-value)
         ;; If we exceed max, try next range
         (find-suitable-prime (+ min-bits 1))]
        [(and (prime? p)
              (let ((mod8 (modulo p 8)))
                (or (= mod8 1) (= mod8 7))))
         ;; Verify 2 is actually quadratic residue
         (if (= (legendre-symbol 2 p) 1)
             p
             (loop (next-prime p)))]
        [else
         (loop (next-prime p))]))))

;; ==============================================================================
;; Compute √2 in Finite Field
;; ==============================================================================

(define (sqrt2-in-field p)
  "Compute √2 in finite field 𝔽_p using Tonelli-Shanks algorithm.
   
   Returns the square root of 2 modulo p, or #f if 2 is not a quadratic residue."
  (tonelli-shanks 2 p))

;; ==============================================================================
;; Verify Field Requirements
;; ==============================================================================

(define (verify-field-requirements p)
  "Verify that prime p satisfies all requirements for ZK-STARK.
   
   Requirements:
   - 2 is quadratic residue mod p (Legendre symbol = 1)
   - p is prime
   - p has sufficient bit length for security (≥ 256 bits)
   
   Returns #t if all requirements satisfied, #f otherwise."
  (and
   (prime? p)
   (>= (integer-length p) 256)
   (= (legendre-symbol 2 p) 1)
   (not (not (sqrt2-in-field p)))))  ; √2 must be computable
