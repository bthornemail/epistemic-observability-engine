#lang racket/base

;; Tests for Package A: Substrate Core

(require rackunit
         racket/string
         "../substrate-core/cbs.rkt"
         "../substrate-core/provenance.rkt"
         "../substrate-core/store.rkt"
         "../kernel-spec.rkt")

(provide
 all-substrate-core-tests)

(define all-substrate-core-tests
  (test-suite
   "Substrate Core Tests"
   
   (test-case "CBS canonicalize-data"
              (let* ((data #"test data")
                     (cid (canonicalize-data data)))
                (check-true (mlss-uri? cid))
                (check-true (string-prefix? cid "mlss://sha3-256/"))))
   
   (test-case "CBS resolve-cid"
              (let* ((data #"test data")
                     (cid (canonicalize-data data))
                     (resolved (resolve-cid cid)))
                (check-equal? resolved data)))
   
   (test-case "Store put/get"
              (let* ((data #"store test")
                     (cid (store-put data))
                     (retrieved (store-get cid)))
                (check-equal? retrieved data)
                (check-true (store-has? cid))))
   
   (test-case "Provenance record"
              (let* ((record (Provenance-Record (list "input1" "input2")
                                                "test-transform"
                                                "output1"))
                     (cid (record-provenance record)))
                (check-equal? cid "output1")
                (check-equal? (get-provenance-by-cid cid) record)))))

