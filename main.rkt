#lang racket/base

;; Main Entry Point
;; Epistemic Observability Engine
;; Starts JSON-RPC server

(require "rpc/server.rkt"
         "config.rkt"
         "utils/logging.rkt")

(provide
 main)

;; Main function
(define (main [port #f])
  "Start the Epistemic Observability Engine JSON-RPC server"
  ;; Load configuration
  (when (file-exists? "config.json")
    (load-config "config.json"))
  
  ;; Set up signal handlers for graceful shutdown
  (with-handlers ([exn:break?
                   (lambda (e)
                     (log-info "Received shutdown signal")
                     (stop-rpc-server)
                     (exit 0))])
    (start-rpc-server port)
    (log-info "Server started. Press Ctrl+C to stop.")
    ;; Keep server running
    (let loop ()
      (sleep 1)
      (loop))))

;; Run if executed directly
(module+ main
  (define port (if (> (vector-length (current-command-line-arguments)) 0)
                   (string->number (vector-ref (current-command-line-arguments) 0))
                   8080))
  (main port))

