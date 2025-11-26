#!/bin/bash
# Build script for Epistemic Observability Engine

set -e

echo "Building Epistemic Observability Engine..."

# Compile all Racket files
raco make .

# Run tests
echo "Running tests..."
raco test tests/

echo "Build complete!"


