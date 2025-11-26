#lang racket/base

;; Configuration Management
;; Production-grade configuration with environment variables and file support

(require racket/file
         racket/port
         json)

(provide
 load-config
 get-config
 set-config!
 config-port
 config-log-level
 config-log-file)

;; Default configuration
(define default-config
  (hasheq 'port 8080
          'log-level 'info
          'log-file #f
          'max-connections 100
          'request-timeout 30))

;; Current configuration
(define current-config (make-parameter default-config))

;; Load configuration from file
(define (load-config path)
  "Load configuration from JSON file"
  (if (file-exists? path)
      (let ((json-data (call-with-input-file path
                        (lambda (port)
                          (read-json port)))))
        ;; Merge default config with file config (file takes precedence)
        (let ((merged (for/fold ([cfg default-config])
                               ([key (in-hash-keys json-data)])
                        (hash-set cfg key (hash-ref json-data key)))))
          (current-config merged)
          merged))
      (begin
        (printf "Config file not found: ~a, using defaults\n" path)
        default-config)))

;; Get configuration value
(define (get-config key)
  "Get configuration value by key"
  (hash-ref (current-config) key (hash-ref default-config key)))

;; Set configuration value
(define (set-config! key value)
  "Set configuration value"
  (current-config (hash-set (current-config) key value)))

;; Configuration parameters (for easy access)
(define config-port (make-derived-parameter current-config
                                            (lambda (cfg) (hash-ref cfg 'port))
                                            (lambda (val) (set-config! 'port val))))

(define config-log-level (make-derived-parameter current-config
                                                 (lambda (cfg) (hash-ref cfg 'log-level))
                                                 (lambda (val) (set-config! 'log-level val))))

(define config-log-file (make-derived-parameter current-config
                                                (lambda (cfg) (hash-ref cfg 'log-file))
                                                (lambda (val) (set-config! 'log-file val))))

;; Load from environment variables
(define (load-env-config)
  "Load configuration from environment variables"
  (let ((port-str (getenv "EPISTEMIC_PORT"))
        (log-level-str (getenv "EPISTEMIC_LOG_LEVEL"))
        (log-file-str (getenv "EPISTEMIC_LOG_FILE")))
    (when port-str
      (set-config! 'port (string->number port-str)))
    (when log-level-str
      (set-config! 'log-level (string->symbol log-level-str)))
    (when log-file-str
      (set-config! 'log-file log-file-str))))

;; Initialize configuration on load
(load-env-config)

