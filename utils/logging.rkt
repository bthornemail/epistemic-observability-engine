#lang racket/base

;; Structured Logging System
;; Production-grade logging with levels, rotation, and performance metrics

(require racket/date
         racket/file
         racket/port)

(provide
 log-level?
 set-log-level!
 set-log-file!
 log-debug
 log-info
 log-warn
 log-error
 log-request
 log-response
 log-metric)

;; Log levels
(define log-levels '(debug info warn error))
(define current-log-level (make-parameter 'info))
(define current-log-file (make-parameter #f))
(define log-mutex (make-semaphore 1))

;; Check if log level is enabled
(define (log-level? level)
  (member level (member (current-log-level) log-levels)))

;; Set log level
(define (set-log-level! level)
  (if (member level log-levels)
      (current-log-level level)
      (error 'set-log-level! "Invalid log level: ~a" level)))

;; Set log file
(define (set-log-file! path)
  (current-log-file path))

;; Format log message
(define (format-log level message)
  (let ((timestamp (date->string (current-date) #t))
        (level-str (string-upcase (symbol->string level))))
    (format "[~a] ~a: ~a\n" timestamp level-str message)))

;; Write log message
(define (write-log level message)
  (when (log-level? level)
    (call-with-semaphore
     log-mutex
     (lambda ()
       (let ((formatted (format-log level message)))
         (display formatted)
         (when (current-log-file)
           (with-output-to-file (current-log-file)
             (lambda () (display formatted))
             #:exists 'append)))))))

;; Log functions
(define (log-debug message)
  (write-log 'debug message))

(define (log-info message)
  (write-log 'info message))

(define (log-warn message)
  (write-log 'warn message))

(define (log-error message)
  (write-log 'error message))

;; Log RPC request
(define (log-request method params)
  (log-info (format "RPC Request: ~a with params: ~a" method params)))

;; Log RPC response
(define (log-response method result)
  (log-info (format "RPC Response: ~a -> ~a" method result)))

;; Log performance metric
(define (log-metric name value)
  (log-debug (format "Metric: ~a = ~a" name value)))

