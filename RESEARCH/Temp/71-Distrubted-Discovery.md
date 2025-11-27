---
id: distributed-discovery-e8-weyl-orbit
title: "Dynamic Distributed E8 Weyl Orbit Computation Architecture"
level: intermediate
type: specification
tags: [e8, weyl-orbit, distributed-systems, termux, udp, tcp, coordinator-worker]
keywords: [distributed-e8, weyl-orbit-computation, termux, udp-discovery, tcp-task-distribution, coordinator-worker, multi-protocol]
prerequisites: []
enables: []
related: [federated-consensus, eoe-technical-appendix]
readingTime: 30
difficulty: 3
blackboard:
  status: draft
  assignedAgent: null
  lastUpdate: 2025-01-27
  dependencies: []
  watchers: []
  r5rsEngine: null
  selfBuilding:
    enabled: false
    source: null
    pattern: null
---
Designing a dynamic, distributed system that works in Termux and other Unix environments. Reviewing existing network protocols and distributed architecture patterns:


[4 tools called]


Designing a dynamic, distributed E8 Weyl orbit computation system that works in Termux and other Unix environments. Creating the architecture document:


[2 tools called]


Design for a dynamic, distributed E8 Weyl orbit computation system:

## Dynamic Distributed E8 Weyl Orbit Computation Architecture

### Overview

A coordinator-worker system that:
- Uses UDP for lightweight discovery (Termux-friendly)
- Dynamically adds/removes clients
- Distributes E8 Weyl orbit computation tasks
- Works across Unix environments (Termux, Linux, macOS)
- Aggregates results to shared storage

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Coordinator Node                       │
│  (Laptop or Linode VM - Central Task Dispatcher)         │
│  - UDP Discovery Server (port 8888)                      │
│  - TCP Task Server (port 9000)                           │
│  - Task Queue & Distribution                              │
│  - Result Aggregation & Storage                          │
└─────────────────────────────────────────────────────────┘
                    ↕ UDP Discovery
                    ↕ TCP Task/Result
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│ Worker Node 1│  │ Worker Node 2│  │ Worker Node N│
│ (Termux)     │  │ (Termux)     │  │ (Any Unix)   │
│ - UDP Client │  │ - UDP Client │  │ - UDP Client │
│ - TCP Worker │  │ - TCP Worker │  │ - TCP Worker │
│ - E8 Compute│  │ - E8 Compute│  │ - E8 Compute│
└──────────────┘  └──────────────┘  └──────────────┘
```

### Protocol Design

#### Phase 1: UDP Discovery (Termux-friendly)

**UDP Broadcast/Multicast on port 8888**

```racket
;; Discovery Message Format (JSON)
{
  "type": "DISCOVERY_REQUEST" | "DISCOVERY_RESPONSE",
  "node_id": "hex-string",
  "capabilities": {
    "e8_compute": true,
    "max_orbit_size": 1000000,
    "platform": "termux" | "linux" | "macos"
  },
  "address": "ip:port",
  "timestamp": 1234567890
}
```

**Discovery Flow:**
1. Coordinator broadcasts `DISCOVERY_REQUEST` every 5 seconds
2. Workers respond with `DISCOVERY_RESPONSE` (their TCP port)
3. Coordinator maintains worker registry
4. Workers can join/leave dynamically

#### Phase 2: TCP Task Distribution (Reliable)

**TCP Connection on port 9000 (coordinator) or dynamic (workers)**

```racket
;; Task Assignment Message
{
  "type": "TASK_ASSIGN",
  "task_id": "uuid",
  "task_type": "WEYL_ORBIT",
  "params": {
    "start_point": [x1, x2, ..., x8],
    "max_orbit_size": 1000000,
    "bip32_path": "m/44'/0'/0'/0/0",
    "checkpoint_interval": 100000
  },
  "deadline": 3600  // seconds
}

;; Result Submission Message
{
  "type": "RESULT_SUBMIT",
  "task_id": "uuid",
  "status": "COMPLETE" | "PARTIAL" | "ERROR",
  "points_computed": 500000,
  "orbit_points": [[x1...x8], ...],  // or file reference
  "checkpoint": "hash",
  "next_checkpoint": "hash"
}
```

### Implementation Files

#### 1. `distributed-e8-coordinator.rkt`

```racket
#lang racket/base

;; Distributed E8 Coordinator
;; Manages worker discovery, task distribution, and result aggregation

(require racket/udp
         racket/tcp
         json
         racket/thread
         racket/hash
         "e8-api.rkt")

(provide
 start-coordinator
 register-worker
 assign-task
 aggregate-results)

;; Worker registry
(struct worker
  (node-id
   address
   tcp-port
   capabilities
   last-seen
   active-tasks)
  #:transparent)

;; Task structure
(struct task
  (task-id
   task-type
   params
   assigned-worker
   status
   result)
  #:transparent)

;; Coordinator state
(struct coordinator-state
  (workers
   task-queue
   completed-tasks
   storage-path)
  #:transparent)

;; Start coordinator
(define (start-coordinator [udp-port 8888] [tcp-port 9000] [storage-path "./e8-results"])
  "Start coordinator server.
UDP-PORT: Discovery port (default 8888)
TCP-PORT: Task server port (default 9000)
STORAGE-PATH: Where to store aggregated results"
  
  ;; UDP Discovery Server
  (thread
   (lambda ()
     (let ((udp-socket (udp-open-socket)))
       (udp-bind! udp-socket #f udp-port)
       (discovery-loop udp-socket))))
  
  ;; TCP Task Server
  (thread
   (lambda ()
     (let ((listener (tcp-listen tcp-port 10 #t)))
       (task-server-loop listener))))
  
  (coordinator-state (make-hash) '() (make-hash) storage-path))

;; Discovery loop (UDP)
(define (discovery-loop socket)
  (let loop ()
    (let-values ([(data addr port) (udp-receive! socket)])
      (let ((message (string->jsexpr (bytes->string/utf-8 data))))
        (handle-discovery-message message addr port))
      (loop))))

;; Handle discovery messages
(define (handle-discovery-message message addr port)
  (match (hash-ref message 'type)
    ["DISCOVERY_REQUEST"
     ;; Worker requesting coordinator info
     (send-discovery-response socket addr port)]
    ["DISCOVERY_RESPONSE"
     ;; Worker announcing itself
     (register-worker-from-discovery message addr port)]))

;; Task distribution algorithm
(define (distribute-tasks coordinator)
  "Distribute E8 Weyl orbit computation tasks to available workers.
Uses round-robin with capacity awareness."
  
  (let* ((available-workers (filter-worker-capacity coordinator))
         (tasks (coordinator-state-task-queue coordinator)))
    
    (for ([task tasks]
          [worker (in-cycle available-workers)])
      (assign-task-to-worker coordinator task worker))))

;; Assign task to worker
(define (assign-task-to-worker coordinator task worker)
  "Assign a task to a worker via TCP connection"
  
  (let* ((worker-addr (worker-address worker))
         (worker-port (worker-tcp-port worker))
         (task-msg (hash 'type "TASK_ASSIGN"
                        'task_id (task-task-id task)
                        'task_type (task-task-type task)
                        'params (task-params task))))
    
    (with-handlers ([exn:fail? (lambda (e) (log-error "Task assign failed" e))])
      (let-values ([(in out) (tcp-connect worker-addr worker-port)])
        (write-json task-msg out)
        (flush-output out)
        (close-output-port out)
        (close-input-port in)))))
```

#### 2. `distributed-e8-worker.rkt`

```racket
#lang racket/base

;; Distributed E8 Worker
;; Connects to coordinator, receives tasks, computes E8 orbits

(require racket/udp
         racket/tcp
         json
         racket/thread
         "e8-api.rkt")

(provide
 start-worker
 connect-to-coordinator
 process-task)

;; Worker configuration
(struct worker-config
  (node-id
   coordinator-addr
   coordinator-udp-port
   coordinator-tcp-port
   max-orbit-size
   checkpoint-interval)
  #:transparent)

;; Start worker
(define (start-worker config)
  "Start worker that connects to coordinator and processes tasks"
  
  ;; UDP Discovery Client
  (thread
   (lambda ()
     (discovery-client-loop config)))
  
  ;; TCP Task Client
  (thread
   (lambda ()
     (task-client-loop config))))

;; Discovery client (announces worker to coordinator)
(define (discovery-client-loop config)
  (let ((socket (udp-open-socket))
        (coord-addr (worker-config-coordinator-addr config))
        (coord-port (worker-config-coordinator-udp-port config)))
    
    ;; Send discovery response every 5 seconds
    (let loop ()
      (let ((message (hash 'type "DISCOVERY_RESPONSE"
                          'node_id (worker-config-node-id config)
                          'capabilities (hash 'e8_compute #t
                                            'max_orbit_size (worker-config-max-orbit-size config)
                                            'platform (detect-platform))
                          'address (format "~a:~a" (get-local-ip) (get-available-port))
                          'timestamp (current-seconds))))
        (udp-send-to socket coord-addr coord-port
                    (string->bytes/utf-8 (jsexpr->string message))))
      (sleep 5)
      (loop))))

;; Task client (receives and processes tasks)
(define (task-client-loop config)
  (let ((coord-addr (worker-config-coordinator-addr config))
        (coord-port (worker-config-coordinator-tcp-port config)))
    
    (let loop ()
      (with-handlers ([exn:fail? (lambda (e) (log-error "Connection failed" e) (sleep 5))])
        (let-values ([(in out) (tcp-connect coord-addr coord-port)])
          (let ((task-msg (read-json in)))
            (process-task config task-msg)
            (close-input-port in)
            (close-output-port out))))
      (sleep 1)
      (loop))))

;; Process E8 Weyl orbit computation task
(define (process-task config task-msg)
  "Compute E8 Weyl orbit for assigned task"
  
  (let* ((task-id (hash-ref task-msg 'task_id))
         (params (hash-ref task-msg 'params))
         (start-point (hash-ref params 'start_point))
         (max-size (hash-ref params 'max_orbit_size))
         (checkpoint-int (hash-ref params 'checkpoint_interval 100000)))
    
    ;; Compute Weyl orbit with checkpointing
    (let ((orbit (compute-weyl-orbit-with-checkpoints start-point max-size checkpoint-int)))
      
      ;; Submit result
      (submit-result config task-id orbit))))

;; Compute Weyl orbit with periodic checkpoints
(define (compute-weyl-orbit-with-checkpoints start-point max-size checkpoint-interval)
  "Compute Weyl orbit with checkpointing for fault tolerance"
  
  (let ((orbit '())
        (visited (make-hash))
        (queue (list start-point))
        (checkpoint-count 0))
    
    (let loop ()
      (when (and (not (null? queue))
                 (< (length orbit) max-size))
        (let ((current (car queue)))
          (set! queue (cdr queue))
          
          (unless (hash-has-key? visited current)
            (hash-set! visited current #t)
            (set! orbit (cons current orbit))
            
            ;; Apply Weyl generators to get neighbors
            (let ((neighbors (apply-weyl-generators current)))
              (set! queue (append queue neighbors)))
            
            ;; Checkpoint
            (when (= (modulo (length orbit) checkpoint-interval) 0)
              (save-checkpoint orbit checkpoint-count)
              (set! checkpoint-count (add1 checkpoint-count)))))
        
        (loop)))
    
    orbit))
```

#### 3. `distributed-e8-storage.rkt`

```racket
#lang racket/base

;; Distributed E8 Storage
;; Handles result aggregation and storage

(require json
         racket/file
         racket/port)

(provide
 aggregate-results
 save-result-batch
 load-result-batch
 merge-orbit-results)

;; Aggregate results from multiple workers
(define (aggregate-results storage-path task-results)
  "Aggregate E8 orbit results from multiple workers into unified dataset"
  
  (let ((all-points (make-hash))
        (total-count 0))
    
    ;; Merge all orbit points (deduplicate)
    (for ([result task-results])
      (let ((points (hash-ref result 'orbit_points)))
        (for ([point points])
          (let ((point-key (point-to-key point)))
            (unless (hash-has-key? all-points point-key)
              (hash-set! all-points point-key point)
              (set! total-count (add1 total-count)))))))
    
    ;; Save aggregated result
    (let ((output-file (build-path storage-path "aggregated-orbit.json")))
      (with-output-to-file output-file
        (lambda ()
          (write-json (hash 'total_points total-count
                           'points (hash-values all-points)
                           'timestamp (current-seconds))))))
    
    total-count))
```

### Termux-Specific Considerations

1. Permissions: Request network permissions in Termux
2. Battery: Use checkpointing to resume after sleep
3. Storage: Use external storage if available
4. Resources: Limit `max_orbit_size` based on available RAM

```racket
;; Termux detection and optimization
(define (detect-platform)
  (cond
    [(file-exists? "/data/data/com.termux") "termux"]
    [(file-exists? "/usr/bin/apt") "linux"]
    [(file-exists? "/usr/bin/brew") "macos"]
    [else "unknown"]))

(define (get-termux-capabilities)
  "Get Termux-specific capabilities and limits"
  (hash 'max_orbit_size 500000      ; Conservative for mobile
        'checkpoint_interval 50000  ; More frequent checkpoints
        'battery_aware #t
        'storage_path "/data/data/com.termux/files/home/e8-results"))
```

### Usage

**On Coordinator (Laptop/Linode):**
```bash
racket -e "(require \"distributed-e8-coordinator.rkt\") (start-coordinator)"
```

**On Worker (Termux/Android):**
```bash
racket -e "(require \"distributed-e8-worker.rkt\") 
           (start-worker (worker-config \"node-1\" \"192.168.1.100\" 8888 9000 1000000 100000))"
```

### Future ESP32 Integration

ESP32 can act as a messaging relay:
- Lightweight UDP forwarder
- Task status broadcaster
- Low-power coordinator proxy

```racket
;; ESP32 Message Relay (future)
(define (esp32-relay-message from to message)
  "ESP32 acts as lightweight message relay between coordinator and workers"
  ;; Forward UDP discovery messages
  ;; Broadcast task status updates
  ;; Minimal computation, just message passing
  )
```

### Benefits

1. Dynamic: Workers join/leave without coordinator restart
2. Fault-tolerant: Checkpointing allows recovery
3. Scalable: Add more workers to increase throughput
4. Termux-compatible: Works in restricted Android environments
5. Cross-platform: Unix-based systems supported
6. Extensible: ESP32 can be added as relay later


Design for a multi-protocol distributed E8 system using BLE, IPv6, and WiFi mesh/AP:

## Enhanced Multi-Protocol Distributed E8 Architecture

### Protocol Stack

```
┌─────────────────────────────────────────────────────────────┐
│              Application Layer (E8 Computation)              │
├─────────────────────────────────────────────────────────────┤
│  BLE GATT    │  IPv6/UDP    │  WiFi Mesh    │  TCP/IP       │
│  (Discovery) │  (Discovery)  │  (Data Sync)  │  (Tasks)      │
├─────────────────────────────────────────────────────────────┤
│  Bluetooth   │  IPv6 Stack   │  WiFi 802.11  │  TCP/IP       │
│  Low Energy  │  (Mesh-ready) │  (AP/Mesh)   │  (Reliable)   │
└─────────────────────────────────────────────────────────────┘
```

### Multi-Protocol Roles

#### 1. BLE (Bluetooth Low Energy) - Low-Power Discovery
**Purpose**: Device discovery, presence detection, lightweight status updates

**Use Cases**:
- Worker node discovery (when WiFi unavailable)
- Battery-powered device presence
- Task status broadcasts (<1KB messages)
- ESP32 integration (low-power relay)

**Implementation**:
```racket
#lang racket/base

;; BLE Discovery Service for Distributed E8
(require json
         racket/thread)

(provide
 start-ble-discovery
 ble-announce-worker
 ble-scan-coordinator
 ble-broadcast-status)

;; BLE Service UUID for E8 Distributed Network
(define E8_DISTRIBUTED_SERVICE_UUID "0000e8d0-0000-1000-8000-00805f9b34fb")

;; BLE Characteristics
(define E8_NODE_ID_CHAR_UUID "0000e8d1-0000-1000-8000-00805f9b34fb")      ; Node ID
(define E8_TASK_STATUS_CHAR_UUID "0000e8d2-0000-1000-8000-00805f9b34fb")  ; Task status
(define E8_COORDINATOR_CHAR_UUID "0000e8d3-0000-1000-8000-00805f9b34fb")   ; Coordinator info

;; BLE Worker Announcement
(define (ble-announce-worker node-id capabilities coordinator-addr)
  "Announce worker via BLE GATT service.
  NODE-ID: Worker identifier
  CAPABILITIES: Hash of worker capabilities
  COORDINATOR-ADDR: Coordinator IPv6 address"
  
  (let ((service-data (hash 'node_id node-id
                            'capabilities capabilities
                            'coordinator coordinator-addr
                            'timestamp (current-seconds))))
    
    ;; In production, would use BLE GATT server
    ;; For now, simulate BLE advertisement
    (hash 'type "BLE_ADVERTISEMENT"
          'service_uuid E8_DISTRIBUTED_SERVICE_UUID
          'data service-data)))

;; BLE Coordinator Discovery
(define (ble-scan-coordinator [timeout 5])
  "Scan for coordinator via BLE.
  Returns list of discovered coordinators"
  
  ;; In production, would use BLE GATT client to scan
  ;; For now, return mock discovery
  (list (hash 'coordinator_id "coord-1"
             'address "2001:db8::1"
             'tcp_port 9000
             'udp_port 8888
             'rssi -70)))
```

#### 2. IPv6 - Modern Networking & Mesh Support
**Purpose**: Native mesh networking, modern addressing, multicast discovery

**Use Cases**:
- IPv6 multicast for coordinator discovery
- Link-local addresses for mesh networks
- Stateless address autoconfiguration (SLAAC)
- Better support for mobile/roaming devices

**Implementation**:
```racket
#lang racket/base

;; IPv6 Discovery and Mesh Networking
(require racket/udp
         json
         racket/string)

(provide
 start-ipv6-discovery
 ipv6-multicast-announce
 ipv6-link-local-address
 ipv6-mesh-connect)

;; IPv6 Multicast Group for E8 Discovery
(define E8_IPV6_MULTICAST "ff02::e8d0")  ; Link-local multicast

;; Get IPv6 Link-Local Address
(define (ipv6-link-local-address)
  "Get IPv6 link-local address for this interface.
  Returns IPv6 address string or #f"
  
  ;; In production, would query network interfaces
  ;; For now, return mock address
  "fe80::1")

;; IPv6 Multicast Discovery
(define (start-ipv6-discovery [multicast-group E8_IPV6_MULTICAST] [port 8888])
  "Start IPv6 multicast discovery server"
  
  (let ((socket (udp-open-socket #f #f #t)))  ; IPv6 socket
    (udp-bind! socket multicast-group port #t)  ; Join multicast group
    
    (thread
     (lambda ()
       (let loop ()
         (let-values ([(data addr port) (udp-receive! socket)])
           (handle-ipv6-discovery-message data addr port))
         (loop))))))

;; IPv6 Multicast Announcement
(define (ipv6-multicast-announce node-info)
  "Announce node via IPv6 multicast"
  
  (let ((socket (udp-open-socket #f #f #t))
        (message (string->bytes/utf-8 (jsexpr->string node-info))))
    (udp-send-to socket E8_IPV6_MULTICAST 8888 message)
    (udp-close socket)))
```

#### 3. WiFi Mesh/AP - High-Bandwidth Data Transfer
**Purpose**: Large data transfer, result aggregation, file sync

**Use Cases**:
- Transfer computed E8 orbit points (large datasets)
- Sync checkpoints between workers
- Aggregate results from multiple workers
- Web interface for monitoring

**Implementation**:
```racket
#lang racket/base

;; WiFi Mesh/AP Integration for Distributed E8
(require json
         racket/tcp
         racket/file)

(provide
 start-wifi-mesh-coordinator
 wifi-mesh-connect
 wifi-transfer-orbit-data
 wifi-sync-checkpoint)

;; WiFi Mesh Configuration
(struct wifi-mesh-config
  (ssid
   password
   channel
   mesh-mode  ; 'ap' | 'mesh' | 'station'
   ipv6-enabled)
  #:transparent)

;; Start WiFi Mesh Coordinator (Access Point Mode)
(define (start-wifi-mesh-coordinator config)
  "Start WiFi AP for mesh network.
  In production, would configure hostapd or similar"
  
  (let ((mesh-ssid (wifi-mesh-config-ssid config))
        (mesh-password (wifi-mesh-config-password config)))
    
    ;; Configure WiFi AP
    ;; In production: system("hostapd ...") or similar
    (hash 'status "started"
          'ssid mesh-ssid
          'mode "access_point"
          'ipv6 (wifi-mesh-config-ipv6-enabled config))))

;; Transfer Large Orbit Data via WiFi
(define (wifi-transfer-orbit-data target-addr orbit-data)
  "Transfer computed orbit data via WiFi (high bandwidth)"
  
  (let ((socket (tcp-connect target-addr 9001)))  ; Data transfer port
    (let-values ([(in out) socket])
      ;; Send orbit data in chunks
      (write-json (hash 'type "ORBIT_DATA"
                       'points orbit-data
                       'count (length orbit-data)) out)
      (flush-output out)
      (close-output-port out)
      (close-input-port in))))
```

#### 4. Enhanced Distributed E8 Coordinator (Multi-Protocol)

```racket
#lang racket/base

;; Enhanced Multi-Protocol Distributed E8 Coordinator
(require "distributed-e8-coordinator.rkt"
         "ble-discovery.rkt"
         "ipv6-discovery.rkt"
         "wifi-mesh.rkt"
         json
         racket/udp
         racket/tcp)

(provide
 start-multi-protocol-coordinator
 register-worker-multi-protocol)

;; Multi-Protocol Coordinator State
(struct multi-protocol-coordinator
  (coordinator-state
   ble-enabled
   ipv6-enabled
   wifi-mesh-enabled
   discovery-protocols)
  #:transparent)

;; Start Multi-Protocol Coordinator
(define (start-multi-protocol-coordinator 
         [udp-port 8888]
         [tcp-port 9000]
         [storage-path "./e8-results"]
         [enable-ble #t]
         [enable-ipv6 #t]
         [enable-wifi-mesh #t])
  "Start coordinator with multiple protocol support"
  
  (let* ((coord-state (start-coordinator udp-port tcp-port storage-path))
         (discovery-protocols '()))
    
    ;; Start UDP Discovery (always enabled)
    (thread
     (lambda ()
       (udp-discovery-server udp-port)))
    
    ;; Start BLE Discovery (if enabled)
    (when enable-ble
      (thread
       (lambda ()
         (start-ble-discovery)))
      (set! discovery-protocols (cons 'ble discovery-protocols)))
    
    ;; Start IPv6 Discovery (if enabled)
    (when enable-ipv6
      (thread
       (lambda ()
         (start-ipv6-discovery)))
      (set! discovery-protoThis design provides a practical, implementable system for distributed E8 computation. Should I implement any specific component first?cols (cons 'ipv6 discovery-protocols)))
    
    ;; Start WiFi Mesh (if enabled)
    (when enable-wifi-mesh
      (let ((mesh-config (wifi-mesh-config "E8-Mesh" "password123" 6 'ap #t)))
        (start-wifi-mesh-coordinator mesh-config))
      (set! discovery-protocols (cons 'wifi-mesh discovery-protocols)))
    
    (multi-protocol-coordinator coord-state enable-ble enable-ipv6 
                                enable-wifi-mesh discovery-protocols)))
```

#### 5. Enhanced Worker (Multi-Protocol Client)

```racket
#lang racket/base

;; Enhanced Multi-Protocol Worker
(require "distributed-e8-worker.rkt"
         "ble-discovery.rkt"
         "ipv6-discovery.rkt"
         "wifi-mesh.rkt")

(provide
 start-multi-protocol-worker
 discover-coordinator-multi-protocol)

;; Multi-Protocol Worker Configuration
(struct multi-protocol-worker-config
  (node-id
   coordinator-addr
   use-ble
   use-ipv6
   use-wifi-mesh
   fallback-order)  ; Order to try protocols: '(ble ipv6 udp wifi)
  #:transparent)

;; Discover Coordinator Using Multiple Protocols
(define (discover-coordinator-multi-protocol config)
  "Try multiple protocols to discover coordinator.
  Returns coordinator info or #f"
  
  (let ((fallback-order (multi-protocol-worker-config-fallback-order config)))
    
    (let loop ([protocols fallback-order])
      (if (null? protocols)
          #f
          (let* ((protocol (car protocols))
                 (coordinator (try-protocol-discovery protocol config)))
            (if coordinator
                coordinator
                (loop (cdr protocols))))))))

;; Try Protocol-Specific Discovery
(define (try-protocol-discovery protocol config)
  "Try to discover coordinator using specific protocol"
  
  (case protocol
    [(ble)
     (if (multi-protocol-worker-config-use-ble config)
         (ble-scan-coordinator 5)
         #f)]
    [(ipv6)
     (if (multi-protocol-worker-config-use-ipv6 config)
         (ipv6-multicast-discover)
         #f)]
    [(udp)
     (udp-broadcast-discover)]
    [(wifi)
     (if (multi-protocol-worker-config-use-wifi-mesh config)
         (wifi-mesh-discover)
         #f)]
    [else #f]))

;; Start Multi-Protocol Worker
(define (start-multi-protocol-worker config)
  "Start worker with multi-protocol support"
  
  ;; Discover coordinator using multiple protocols
  (let ((coordinator (discover-coordinator-multi-protocol config)))
    (if coordinator
        (begin
          ;; Start protocol-specific connections
          (when (multi-protocol-worker-config-use-ble config)
            (ble-announce-worker (multi-protocol-worker-config-node-id config)
                                (hash 'e8_compute #t)
                                (hash-ref coordinator 'address)))
          
          (when (multi-protocol-worker-config-use-ipv6 config)
            (ipv6-multicast-announce (hash 'node_id (multi-protocol-worker-config-node-id config)
                                          'address (ipv6-link-local-address))))
          
          ;; Start main worker loop
          (start-worker (worker-config (multi-protocol-worker-config-node-id config)
                                      (hash-ref coordinator 'address)
                                      8888  ; UDP port
                                      9000  ; TCP port
                                      1000000  ; max orbit size
                                      100000)))  ; checkpoint interval
        (error "Could not discover coordinator via any protocol"))))
```

### Protocol Selection Strategy

**Automatic Protocol Selection**:
1. Try BLE first (low power, if available)
2. Fall back to IPv6 multicast (modern, mesh-friendly)
3. Fall back to UDP broadcast (universal)
4. Use WiFi mesh for data transfer (high bandwidth)

**Use Case Mapping**:
- Discovery: BLE → IPv6 → UDP
- Task Assignment: TCP/IP (reliable)
- Data Transfer: WiFi mesh (high bandwidth)
- Status Updates: BLE or IPv6 (low overhead)
- ESP32 Integration: BLE (low power)

### Termux-Specific Enhancements

```racket
;; Termux BLE Support Detection
(define (termux-ble-available?)
  "Check if BLE is available in Termux"
  (and (file-exists? "/data/data/com.termux")
       ;; In production, would check for BLE permissions
       #t))

;; Termux IPv6 Support
(define (termux-ipv6-available?)
  "Check if IPv6 is available in Termux"
  (let ((result (system "ip -6 addr show 2>/dev/null")))
    (not (equal? result ""))))

;; Termux WiFi Mesh Support
(define (termux-wifi-mesh-available?)
  "Check if WiFi mesh/AP mode is available in Termux"
  ;; Termux may have limited WiFi capabilities
  ;; Would need root or special permissions
  #f)
```

### ESP32 Integration (Future)

```racket
;; ESP32 as BLE Relay
(define (esp32-ble-relay coordinator-info)
  "ESP32 acts as BLE relay for coordinator discovery.
  Low-power device that broadcasts coordinator info via BLE"
  
  (hash 'type "ESP32_BLE_RELAY"
        'coordinator coordinator-info
        'power_mode "low"
        'range "10m"))
```

### Benefits of Multi-Protocol Approach

1. Resilience: Multiple discovery methods
2. Efficiency: Use the right protocol for each task
3. Compatibility: Works across different environments
4. Scalability: WiFi mesh for high-bandwidth aggregation
5. Low Power: BLE for battery-powered devices
6. Modern: IPv6 for mesh networking

This design provides a flexible, multi-protocol system that adapts to available hardware and network conditions. Should I implement any specific protocol component first?