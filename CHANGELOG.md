# Changelog

All notable changes to the Epistemic Observability Engine will be documented in this file.

## [1.1.0] - 2025-11-26

### Added
- **Inverse Projection Agent**: Bidirectional mapping between semantic labels and E8-Points
  - `semantic-lookup`: O(1) resolution of human-readable names to canonical E8-Points
  - `register-semantic`: Register semantic labels with provenance tracking
  - `get-role-provenance-path`: Retrieve delegation lineage (Weyl reflection chain)
  - Reflection history tracking during canonicalization
- New RPC methods:
  - `resolve_name`: Resolve semantic name to E8-Point and provenance path
  - `audit_role`: Audit role delegation chain
  - `register_semantic`: Register semantic name for E8-Point
- Enhanced `evaluate_q` to accept semantic role names in addition to E8-Points
- Complete bi-directional isomorphism between semantic and geometric domains

### Changed
- `evaluate_q` RPC method now supports both E8-Point coordinates and semantic role names
- Canonicalization now tracks reflection history for provenance

### Mathematical Completion
- **Final closure of Vision-Epistemic Isomorphism**: System now fully bijective between semantic and geometric domains
- Human usability achieved in fully decentralized system
- Verifiable geometric governance without central authority

## [1.0.0] - 2024-01-XX

### Added
- Core E8 geometry implementation with Weyl group canonicalization
- Epistemic observability parameterization with UK * phi(V) formula
- Dual pair classifier for eager/lazy execution dispatch
- Geometric RBAC access control system
- JSON-RPC 2.0 server interface
- Comprehensive test suite including UK * phi(V) stability proof
- Structured logging system with levels and rotation
- Configuration management with environment variables and JSON files
- Health check and metrics endpoints
- Error handling and input validation
- Docker support with docker-compose
- Build and packaging scripts
- Complete API documentation

### Fixed
- Resolved type system mismatch between typed/racket and racket/base
- Fixed error variable name conflict in qstar.rkt
- Fixed circular dependency in kernel-spec.rkt
- Fixed test compilation errors with let* bindings
- Added missing function exports

### Changed
- Converted kernel-spec.rkt from typed/racket to racket/base for compatibility
- Improved error messages and validation
- Enhanced logging with performance metrics

### Security
- Input validation on all RPC handlers
- Error handling prevents information leakage

