# ADR 0004: Authentication Method - OAuth2 OIDC

## Status

Accepted

## Date

2025-11-30

## Context

The application requires user authentication for:

- Event creation and management
- Admin access to content
- Secure API access

Requirements:

- Industry-standard security
- Single sign-on capability
- Integration with existing identity providers
- Minimal development overhead
- Good user experience

## Decision

We chose OAuth2 with OpenID Connect (OIDC) for authentication.

## Consequences

### Positive

- Industry-standard security protocol
- Single sign-on across multiple applications
- Delegation of authentication to trusted providers
- No password storage or management
- Good integration with PocketBase
- Scalable and future-proof

### Negative

- Dependency on external identity provider
- More complex setup than simple email/password
- User experience depends on provider's interface

## Alternatives Considered

### Email/Password Authentication

- Considered for simplicity
- Rejected due to security concerns and user experience

### Social Login (Google, Facebook)

- Considered for ease of use
- Rejected due to privacy concerns and limited control

### SAML

- Considered for enterprise integration
- Rejected due to complexity for this use case

### PocketBase Built-in Auth

- Considered as fallback
- Rejected in favor of centralized identity management

## Related Decisions

- ADR 0002: Backend Choice (PocketBase)

## Notes

OIDC provides a good balance of security and usability. The implementation uses PocketBase's OAuth2 integration with a custom OIDC provider.
