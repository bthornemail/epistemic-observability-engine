# Epistemic Observability Engine - API Reference

## JSON-RPC 2.0 Interface

The Epistemic Observability Engine exposes a JSON-RPC 2.0 interface for all operations.

### Base URL
```
http://localhost:8080/
```

### Health Check
```
GET /health
```

Returns server health status.

**Response:**
```json
{
  "status": "healthy",
  "request_count": 42
}
```

### Metrics
```
GET /metrics
```

Returns server metrics.

**Response:**
```json
{
  "request_count": 42,
  "metrics": {}
}
```

### RPC Methods

#### canonicalize
Canonicalize an E8 vector to its dominant chamber representative.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "method": "canonicalize",
  "params": {
    "vector": {
      "coords": [1, 2, 3, 4, 5, 6, 7, 8],
      "norm-sq": 204
    }
  },
  "id": 1
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "result": {
    "coords": [1, 2, 3, 4, 5, 6, 7, 8],
    "norm-sq": 204
  },
  "id": 1
}
```

#### grant_access
Grant access from an agent to a resource based on geometric RBAC.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "method": "grant_access",
  "params": {
    "agent": {
      "coords": [1, 2, 3, 4, 5, 6, 7, 8]
    },
    "resource": {
      "coords": [2, 3, 4, 5, 6, 7, 8, 9]
    }
  },
  "id": 2
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "result": {
    "granted": true
  },
  "id": 2
}
```

#### evaluate_q
Evaluate Q* optimization for a given vector/role and action/resource. Supports both E8-Point coordinates and semantic role names.

**Request (with coordinates):**
```json
{
  "jsonrpc": "2.0",
  "method": "evaluate_q",
  "params": {
    "vector": {
      "coords": [1, 2, 3, 4, 5, 6, 7, 8]
    },
    "action": "test-action"
  },
  "id": 3
}
```

**Request (with semantic names):**
```json
{
  "jsonrpc": "2.0",
  "method": "evaluate_q",
  "params": {
    "role": "CEO",
    "resource": "Q_2025-budget-proposal"
  },
  "id": 3
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "result": {
    "value": 0.0,
    "action-plan": ["read", "write"]
  },
  "id": 3
}
```

#### resolve_name
Resolve a semantic name to its E8-Point and delegation provenance path.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "method": "resolve_name",
  "params": {
    "name": "CEO"
  },
  "id": 4
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "result": {
    "name": "CEO",
    "point": [1, 2, 3, 4, 5, 6, 7, 8],
    "path": [
      {
        "coords": [0, 0, 0, 0, 0, -1, 1, 0],
        "length-sq": 2
      }
    ]
  },
  "id": 4
}
```

#### audit_role
Audit a role by returning its complete delegation reflection chain.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "method": "audit_role",
  "params": {
    "role": "CTO"
  },
  "id": 5
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "result": {
    "role": "CTO",
    "delegation-path": [
      {
        "coords": [0, 0, 0, 0, 0, -1, 1, 0],
        "length-sq": 2
      },
      {
        "coords": [0, 0, 0, 0, -1, 1, 0, 0],
        "length-sq": 2
      }
    ],
    "depth": 2
  },
  "id": 5
}
```

#### register_semantic
Register a semantic name for an E8-Point with provenance tracking.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "method": "register_semantic",
  "params": {
    "name": "CEO",
    "vector": {
      "coords": [1, 2, 3, 4, 5, 6, 7, 8]
    }
  },
  "id": 6
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "result": {
    "name": "CEO",
    "point": [1, 2, 3, 4, 5, 6, 7, 8],
    "registered": true
  },
  "id": 6
}
```

