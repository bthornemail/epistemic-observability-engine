# Installation Guide

## Prerequisites

- Racket 8.12 or later
- Linux, macOS, or Windows

## Installation

### From Source

1. Clone the repository:
```bash
git clone <repository-url>
cd epistemic-observability-engine
```

2. Install dependencies:
```bash
raco pkg install --auto base rackunit-lib math-lib net-lib web-server-lib json crypto-lib
```

3. Build the project:
```bash
./build.sh
```

4. Run tests:
```bash
raco test tests/
```

5. Start the server:
```bash
racket main.rkt
```

### Using Docker

1. Build the image:
```bash
docker build -t epistemic-engine .
```

2. Run the container:
```bash
docker-compose up
```

Or manually:
```bash
docker run -p 8080:8080 epistemic-engine
```

## Configuration

Copy `config.json.example` to `config.json` and modify as needed:

```bash
cp config.json.example config.json
```

Or set environment variables:
```bash
export EPISTEMIC_PORT=8080
export EPISTEMIC_LOG_LEVEL=info
export EPISTEMIC_LOG_FILE=/path/to/logfile
```

## Quick Start

1. Start the server:
```bash
racket main.rkt
```

2. Test the health endpoint:
```bash
curl http://localhost:8080/health
```

3. Make an RPC call:
```bash
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "canonicalize",
    "params": {
      "vector": {
        "coords": [1, 2, 3, 4, 5, 6, 7, 8]
      }
    },
    "id": 1
  }'
```

## Troubleshooting

### Port Already in Use
If port 8080 is already in use, set a different port:
```bash
export EPISTEMIC_PORT=8081
racket main.rkt
```

### Missing Dependencies
If you get missing dependency errors:
```bash
raco pkg install --auto <package-name>
```

### Test Failures
Run tests with verbose output:
```bash
raco test -v tests/
```


