#!/bin/bash
# Package script for Epistemic Observability Engine

set -e

VERSION=${1:-1.0.0}
PACKAGE_NAME="epistemic-observability-engine-${VERSION}"

echo "Packaging Epistemic Observability Engine v${VERSION}..."

# Create package directory
mkdir -p dist/${PACKAGE_NAME}

# Copy source files
cp -r substrate-* rpc utils tests docs wiki *.rkt *.md LICENSE dist/${PACKAGE_NAME}/

# Create tarball
cd dist
tar -czf ${PACKAGE_NAME}.tar.gz ${PACKAGE_NAME}
cd ..

echo "Package created: dist/${PACKAGE_NAME}.tar.gz"






