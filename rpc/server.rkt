#lang racket/base

;; JSON-RPC 2.0 Server
;; RPC Interface Layer
;; Uses web-server library for HTTP JSON-RPC server

(require web-server/servlet
         web-server/servlet-env
         web-server/http
         json
         racket/string
         "../kernel-spec.rkt"
         "../substrate-geometry/e8.rkt"
         "../substrate-geometry/inverse.rkt"
         "../config.rkt"
         "../utils/logging.rkt"
         "handlers.rkt")

(provide
 start-rpc-server
 stop-rpc-server)

;; Server state
(define server-instance #f)
(define request-count (make-parameter 0))
(define metrics-store (make-hash))

;; Increment request count
(define (increment-request-count)
  (request-count (add1 (request-count))))

;; Update metric
(define (update-metric name value)
  (hash-set! metrics-store name value))

;; Parse JSON-RPC request
(define (parse-rpc-request req)
  "Parse JSON-RPC 2.0 request"
  (let ((body (request-post-data/raw req)))
    (if body
        (let ((json-data (bytes->jsexpr body)))
          (hash-ref json-data 'method #f)
          (hash-ref json-data 'params #f)
          (hash-ref json-data 'id #f)
          json-data)
        #f)))

;; Create JSON-RPC response
(define (make-rpc-response result id)
  "Create JSON-RPC 2.0 success response"
  (jsexpr->bytes
   (hasheq 'jsonrpc "2.0"
           'result result
           'id id)))

;; Create JSON-RPC error response
(define (make-rpc-error code message id)
  "Create JSON-RPC 2.0 error response"
  (jsexpr->bytes
   (hasheq 'jsonrpc "2.0"
           'error (hasheq 'code code
                         'message message)
           'id id)))

;; Convert E8-Point to JSON-serializable format
(define (e8-point->json point)
  "Convert E8-Point to JSON format"
  (hasheq 'coords (E8-Point-coords point)
          'norm-sq (E8-Point-norm-sq point)))

;; Convert JSON to E8-Point
(define (json->e8-point json-data)
  "Convert JSON to E8-Point"
  (let ((coords (hash-ref json-data 'coords))
        (norm-sq (hash-ref json-data 'norm-sq)))
    (make-e8-point coords)))

;; Handle health check
(define (handle-health req)
  "Handle health check request"
  (response/full 200 #"OK" (current-seconds) #"application/json"
                 '() (list (jsexpr->bytes (hasheq 'status "healthy"
                                                  'request_count (request-count))))))

;; Handle metrics
(define (handle-metrics req)
  "Handle metrics request"
  (response/full 200 #"OK" (current-seconds) #"application/json"
                 '() (list (jsexpr->bytes (hasheq 'request_count (request-count)
                                                  'metrics (hash-copy metrics-store))))))

;; Handle JSON-RPC method call
(define (handle-rpc-method method params id)
  "Handle a JSON-RPC method call"
  (log-request method (format "~a" params))
  (let ((start-time (current-inexact-milliseconds))
        (result
         (case method
           [("canonicalize")
            (let ((vec (json->e8-point (hash-ref params 'vector))))
              (e8-point->json (handle-rpc-canonicalize vec)))]
           [("grant_access")
            (let ((agent-vec (json->e8-point (hash-ref params 'agent)))
                  (resource-vec (json->e8-point (hash-ref params 'resource))))
              ;; Return access grant as JSON
              (hasheq 'granted #t))]
           [("evaluate_q")
            (let ((vec-or-role (hash-ref params 'vector (hash-ref params 'role #f)))
                  (action-or-resource (hash-ref params 'action (hash-ref params 'resource #f))))
              (if (and vec-or-role action-or-resource)
                  (let ((result (handle-rpc-evaluate-q 
                                 (if (string? vec-or-role)
                                     vec-or-role
                                     (json->e8-point vec-or-role))
                                 (if (string? action-or-resource)
                                     action-or-resource
                                     (json->e8-point action-or-resource)))))
                    (hasheq 'value (Q*-Result-value result)
                            'action-plan (Q*-Result-action-plan result)))
                  (error "evaluate_q requires 'vector'/'role' and 'action'/'resource'")))]
           [("resolve_name")
            (let ((name (hash-ref params 'name)))
              (if name
                  (handle-rpc-resolve-name name)
                  (error "resolve_name requires 'name' parameter")))]
           [("audit_role")
            (let ((role (hash-ref params 'role)))
              (if role
                  (handle-rpc-audit-role role)
                  (error "audit_role requires 'role' parameter")))]
           [("register_semantic")
            (let ((name (hash-ref params 'name))
                  (vec (json->e8-point (hash-ref params 'vector))))
              (if (and name vec)
                  (handle-rpc-register-semantic name vec)
                  (error "register_semantic requires 'name' and 'vector' parameters")))]
           ;; F₄ RPC Methods
           [("project_to_f4")
            (let ((e8-point (json->e8-point (hash-ref params 'e8_point))))
              (handle-rpc-project-to-f4 e8-point))]
           [("f4_distance")
            (let ((role1 (json->e8-point (hash-ref params 'role1)))
                  (role2 (json->e8-point (hash-ref params 'role2))))
              (handle-rpc-f4-distance role1 role2))]
           [("render_24cell")
            (let ((state (json->e8-point (hash-ref params 'state))))
              (handle-rpc-render-24cell state))]
           ;; E₇ RPC Methods
           [("project_to_e7_56")
            (let ((e8-point (json->e8-point (hash-ref params 'e8_point))))
              (handle-rpc-project-to-e7-56 e8-point))]
           [("e7_generation_distance")
            (let ((role1 (json->e8-point (hash-ref params 'role1)))
                  (role2 (json->e8-point (hash-ref params 'role2))))
              (handle-rpc-e7-generation-distance role1 role2))]
           ;; G₂ RPC Methods
           [("update_uk_state")
            (let ((current (hash-ref params 'current_state))
                  (neighbor (hash-ref params 'neighborhood_state)))
              (handle-rpc-update-uk-state current neighbor))]
           [("octonion_multiply")
            (let ((a (hash-ref params 'a))
                  (b (hash-ref params 'b)))
              (handle-rpc-octonion-multiply a b))]
           ;; H₄ RPC Methods
           [("zoom_role")
            (let ((role-path (json->e8-point (hash-ref params 'role_path)))
                  (depth (hash-ref params 'depth 0.5)))
              (handle-rpc-zoom-role role-path depth))]
           [("render_600cell")
            (let ((state (json->e8-point (hash-ref params 'state))))
              (handle-rpc-render-600cell state))]
           ;; ZK-STARK RPC Methods
           [("zk.canonicalize")
            (handle-rpc-zk-canonicalize params)]
           [("zk.verify")
            (handle-rpc-zk-verify params)]
           [else
            (error (format "Unknown method: ~a" method))])))
    (let* ((end-time (current-inexact-milliseconds))
           (latency (- end-time start-time)))
      (log-metric (string-append "rpc." method ".latency") latency)
      (log-response method (format "~a" result))
      (response/full 200 #"OK" (current-seconds) #"application/json"
                     '() (list (make-rpc-response result id))))))

;; Handle JSON-RPC request
(define (handle-request req)
  "Handle JSON-RPC request"
  (increment-request-count)
  (let ((uri-path (url-path (request-uri req)))
        (path-str (path->string (url-path (request-uri req)))))
    (cond
      [(or (equal? uri-path '("/health")) (string-suffix? path-str "/health"))
       (handle-health req)]
      [(or (equal? uri-path '("/metrics")) (string-suffix? path-str "/metrics"))
       (handle-metrics req)]
      [else
       (let ((rpc-data (parse-rpc-request req)))
         (if (not rpc-data)
             (begin
               (log-error "Parse error in RPC request")
               (response/full 400 #"Bad Request" (current-seconds) #"application/json"
                              '() (list (make-rpc-error -32700 "Parse error" #f))))
             (let* ((method (hash-ref rpc-data 'method #f))
                    (params (hash-ref rpc-data 'params #f))
                    (id (hash-ref rpc-data 'id #f)))
               (if (not method)
                   (begin
                     (log-error "Invalid RPC request: missing method")
                     (response/full 400 #"Bad Request" (current-seconds) #"application/json"
                                    '() (list (make-rpc-error -32600 "Invalid Request" id))))
                   (handle-rpc-method method params id)))))])))

;; Start RPC server
(define (start-rpc-server [port #f])
  "Start JSON-RPC server on given port (or from config)"
  (let ((server-port (or port (config-port))))
    (log-info (format "Starting Epistemic Observability Engine on port ~a" server-port))
    (when (config-log-file)
      (set-log-file! (config-log-file)))
    (set-log-level! (config-log-level))
    (set! server-instance
          (serve/servlet handle-request
                        #:port server-port
                        #:launch-browser? #f
                        #:servlet-regexp #rx""
                        #:servlet-path "/"))
    (log-info "Server started successfully")))

;; Stop RPC server
(define (stop-rpc-server)
  "Stop JSON-RPC server"
  (when server-instance
    (log-info "Stopping server...")
    (server-instance 'stop)
    (set! server-instance #f)
    (log-info "Server stopped")))
