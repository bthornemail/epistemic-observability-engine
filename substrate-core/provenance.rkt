#lang racket/base

;; Provenance Chain Protocol (PCP)
;; Package A: Core substrate layer
;; Implements Merkle DAG for computation traces

(require uuid
         racket/date
         "../kernel-spec.rkt"
         "cbs.rkt")

(provide
 record-provenance
 get-provenance-by-cid
 get-provenance-chain)

;; Provenance chain storage (CID -> Provenance-Record)
(define provenance-chain (make-hash))

;; Record provenance and return output CID
(define (record-provenance record)
  "Store provenance record and return output CID"
  (let ((output-cid (Provenance-Record-output-cid record)))
    (hash-set! provenance-chain output-cid record)
    output-cid))

;; Get provenance by CID
(define (get-provenance-by-cid cid)
  "Retrieve provenance record by output CID"
  (hash-ref provenance-chain cid #f))

;; Get provenance chain for a CID (backwards traversal)
(define (get-provenance-chain cid)
  "Build provenance chain backwards from given CID"
  (let loop ((current-cid cid)
             (chain '()))
    (let ((record (get-provenance-by-cid current-cid)))
      (if (not record)
          chain
          (let ((input-cids (Provenance-Record-input-cids record)))
            (if (null? input-cids)
                (cons record chain)
                ;; Follow first input (could be extended to follow all)
                (let ((prev-cid (car input-cids)))
                  (if (member prev-cid (map (lambda (r) (Provenance-Record-output-cid r)) chain))
                      (cons record chain)
                      (loop prev-cid (cons record chain))))))))))


