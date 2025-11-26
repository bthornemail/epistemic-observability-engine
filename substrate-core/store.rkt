#lang racket/base

;; Content-Addressed Storage
;; Package A: Core substrate layer
;; Implements immutable content-addressed key-value store

(require "cbs.rkt")

(provide
 store-get
 store-put
 store-has?)

;; Content-addressed store (uses CBS for storage)
;; The store is implicitly managed by CBS content-store

;; Store data and return CID
(define (store-put data)
  "Store data and return its CID"
  (canonicalize-data data))

;; Get data by CID
(define (store-get cid)
  "Retrieve data by CID"
  (resolve-cid cid))

;; Check if CID exists in store
(define (store-has? cid)
  "Check if CID exists in store"
  (not (not (resolve-cid cid))))


