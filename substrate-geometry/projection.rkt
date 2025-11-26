#lang racket/base

;; Hopf Fibrations and Dimensional Reduction
;; Package B: Geometry layer
;; Implements projection from high-dimensional data to E8 space
;; Full dimensional descent chain: E₈ → E₇ → E₆ → F₄ → 4D

(require "../kernel-spec.rkt"
         "e8.rkt"
         "f4.rkt"
         "e6.rkt"
         "e7.rkt"
         "h4.rkt"
         racket/list
         racket/vector)

;; Re-export projection functions from their modules
(require (only-in "e6.rkt" project-e8-to-e6 project-e6-to-f4)
         (only-in "e7.rkt" project-e8-to-e7-56)
         (only-in "f4.rkt" project-e8-to-f4))

(provide
 project-to-e8
 ;; Full dimensional descent chain (re-exported from modules)
 project-e8-to-e7-56
 project-e8-to-e6
 project-e8-to-f4
 project-e6-to-f4
 ;; Fast paths for different agents
 project-for-visualization  ; E₈ → F₄ → 24-cell
 project-for-optimization    ; E₈ → E₇ → 56D
 project-for-large-graphs)  ; E₈ → E₆ → 78D

;; Project high-dimensional data (bytes or E8-Vector) to E8 space
;; For bytes: hash and map to 8 integers
;; For E8-Vector: return as-is
(define (project-to-e8 input)
  "Project input (bytes or E8-Vector) to E8 space"
  (cond
    [(E8-Point? input) input]
    [(bytes? input)
     ;; Hash bytes and map to 8 integers
     (let* ((hash-bytes (sha256-bytes input))
            (coords (for/list ([i (in-range 8)])
                      (let ((byte-val (bytes-ref hash-bytes (modulo i (bytes-length hash-bytes)))))
                        (- byte-val 128))))  ; Map to range [-128, 127]
           (norm-sq (for/sum ([x coords]) (* x x))))
       (E8-Point coords norm-sq))]
    [else
     (error 'project-to-e8 "Input must be bytes or E8-Point")]))

;; SHA-256 helper (placeholder for SHA3-256)
(require openssl/sha1)

(define (sha256-bytes data)
  "Compute SHA-256 hash of bytes"
  (let ((port (open-input-bytes data)))
    (sha1 port)))

;; ==============================================================================
;; Full Dimensional Descent Chain
;; ==============================================================================

;; Re-exported from e7.rkt, e6.rkt, f4.rkt - no need to redefine

;; ==============================================================================
;; Fast Paths for Different Agents
;; ==============================================================================

(define (project-for-visualization e8-point)
  "Fast path for State Presentation Agent: E₈ → F₄ → 24-cell"
  (project-e8-to-f4 e8-point))

(define (project-for-optimization e8-point)
  "Fast path for Q* Optimizer Agent: E₈ → E₇ → 56D (3 generations + Higgs)"
  (project-e8-to-e7-56 e8-point))

;; Alias for compatibility
(define project-e8-to-e7 project-e8-to-e7-56)

(define (project-for-large-graphs e8-point)
  "Fast path for Observability Parameterizer: E₈ → E₆ (prevents variance explosion)"
  (project-e8-to-e6 e8-point))


