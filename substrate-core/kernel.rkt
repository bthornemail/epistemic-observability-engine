#lang racket/base

;; Deterministic Kernel Scheduler
;; Package A: Core substrate layer
;; Takes input CID, runs Transform, produces output CID, logs ProvenanceRecord

(require "../kernel-spec.rkt"
         "cbs.rkt"
         "provenance.rkt")

(provide
 kernel-execute
 kernel-transform)

;; Transform function type: CID -> CID
(define-type Transform (-> CBS-ID CBS-ID))

;; Execute kernel operation: input CID -> Transform -> output CID -> ProvenanceRecord
(define (kernel-execute input-cid transform-id transform-fn)
  "Execute transform and record provenance"
  (let* ((output-cid (transform-fn input-cid))
         (record (Provenance-Record (list input-cid)
                                    transform-id
                                    output-cid)))
    (record-provenance record)
    output-cid))

;; Kernel transform wrapper
(define (kernel-transform input-cid transform-id transform-fn)
  "Wrapper for kernel-execute with automatic transform ID"
  (kernel-execute input-cid transform-id transform-fn))


