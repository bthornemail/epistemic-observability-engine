#lang racket/base

;; Inverse Projection Agent
;; Package B: Geometry layer - Critical Extension
;; Provides bidirectional mapping between semantic labels and E8-Points
;; Completes the Vision-Epistemic Isomorphism with human usability

(require "../kernel-spec.rkt"
         "../substrate-core/cbs.rkt"
         "../substrate-core/provenance.rkt"
         "e8.rkt"
         "weyl.rkt"
         racket/string)

(provide
 semantic-lookup
 register-semantic
 get-role-provenance-path
 e8-point->cid
 canonicalize-to-dominant-tracked
 record-reflection-step)

;; Semantic registry: semantic-path -> E8-Point
(define semantic-registry (make-hash))

;; Reflection history: point-CID -> list of Simple-Roots (delegation path)
(define reflection-history (make-hash))

;; Convert E8-Point to CID for storage
(define (e8-point->cid point)
  "Convert E8-Point to content ID for storage"
  (let ((coords-bytes (string->bytes/utf-8 (format "~a" (E8-Point-coords point)))))
    (canonicalize-data coords-bytes)))

;; Semantic lookup: Resolve human-readable path to E8-Point
(define (semantic-lookup semantic-path)
  "Resolve a human-readable semantic path to its canonical E8-Point.
   Fast O(1) lookup via content-addressed semantic registry.
   
   Returns #f if not found."
  (hash-ref semantic-registry semantic-path #f))

;; Register semantic label for E8-Point with provenance
(define (register-semantic semantic-path e8-point)
  "Register a semantic label for a canonical E8-Point.
   Records provenance of naming event.
   
   Returns the registered E8-Point."
  (let* ((point-cid (e8-point->cid e8-point))
         (binding-data (list 'semantic-binding semantic-path (E8-Point-coords e8-point)))
         (binding-bytes (string->bytes/utf-8 (format "~a" binding-data)))
         (cid (canonicalize-data binding-bytes)))
    ;; Store in semantic registry
    (hash-set! semantic-registry semantic-path e8-point)
    ;; Record provenance
    (record-provenance
     (Provenance-Record (list point-cid)
                        "register-semantic"
                        cid))
    e8-point))

;; Record a reflection step in the history
(define (record-reflection-step input-point root output-point)
  "Record a Weyl reflection step for provenance tracking"
  (let ((output-cid (e8-point->cid output-point)))
    (hash-update! reflection-history output-cid
                  (lambda (existing) (cons root existing))
                  '())))

;; Get reflection history for a point
(define (get-reflection-history point)
  "Get the list of Simple-Roots used to generate this point from origin"
  (let ((point-cid (e8-point->cid point)))
    (hash-ref reflection-history point-cid '())))

;; Get role provenance path (delegation lineage)
(define (get-role-provenance-path point)
  "Return the exact sequence of Weyl reflections (Simple-Root list)
   that generated this point from the origin — the delegation lineage.
   
   Returns the path in order from origin to final point."
  (reverse (get-reflection-history point)))

;; Enhanced canonicalize-to-dominant with reflection tracking
;; This wraps the original function to track reflection steps
(define (canonicalize-to-dominant-tracked vec roots)
  "Canonicalize to dominant chamber while tracking reflection history"
  (let* ((simple-roots-list (if (null? roots) (e8-get-simple-roots) roots))
         (current (if (E8-Point? vec) vec (make-e8-point vec)))
         (max-iterations 1000)
         (reflection-path '()))
    
    (let loop ((current current)
               (iterations 0)
               (path reflection-path))
      (if (>= iterations max-iterations)
          (begin
            ;; Store final path
            (let ((final-cid (e8-point->cid current)))
              (hash-set! reflection-history final-cid (reverse path)))
            (error (format "canonicalize-to-dominant: exceeded max iterations (~a)" max-iterations)))
          (let-values ([(next changed) (canonicalize-to-dominant-step current simple-roots-list)])
            (if changed
                (let ((root-used (find-reflecting-root current next simple-roots-list)))
                  (when root-used
                    (record-reflection-step current root-used next)
                    (loop next (add1 iterations) (cons root-used path))))
                (begin
                  ;; Store final path
                  (let ((final-cid (e8-point->cid next)))
                    (hash-set! reflection-history final-cid (reverse path)))
                  next)))))))

