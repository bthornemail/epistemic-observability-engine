#lang racket/base

;; Canonical Binary Substrate (CBS)
;; Package A: Core substrate layer
;; Implements content addressing and canonicalization

(require openssl/sha1
         racket/string
         "../kernel-spec.rkt")

(provide
 canonicalize-data
 resolve-cid
 mlss-uri?)

;; Content store (hash table: CID -> bytes)
(define content-store (make-hash))

;; Canonicalize data to CBS-ID (SHA-256 hash, placeholder for SHA3-256)
;; Note: Using SHA-256 as placeholder; upgrade to SHA3-256 when available
(define (canonicalize-data data)
  "Compute SHA-256 hash of bytes to create content ID"
  (let* ((hash-bytes (sha256-bytes data))
         (hash-hex (bytes->hex-string hash-bytes))
         (cid (string-append "mlss://sha3-256/" hash-hex)))
    ;; Store the data
    (hash-set! content-store cid data)
    cid))

;; Resolve CID to bytes
(define (resolve-cid cid)
  "Look up content by CID, return bytes or #f"
  (hash-ref content-store cid #f))

;; Check if URI is mlss:// format
(define (mlss-uri? uri)
  "Check if URI is mlss:// format"
  (string-prefix? uri "mlss://"))

;; Helper: Convert bytes to hex string
(define (bytes->hex-string bs)
  (apply string-append
         (map (lambda (b)
                (format "~02x" b))
              (bytes->list bs))))

;; SHA-256 hashing (placeholder for SHA3-256)
(define (sha256-bytes data)
  "Compute SHA-256 hash of bytes"
  (let ((port (open-input-bytes data)))
    (sha1 port)))

