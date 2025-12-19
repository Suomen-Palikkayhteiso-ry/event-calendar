# ADR 0002: Backend Choice - PocketBase

## Status

Accepted (Updated for elm-pages migration)

## Date

2025-11-30 (Updated: 2025-01-XX)

## Context

The application requires a backend for:

- Data persistence (events, users)
- Authentication and authorization
- File storage (event images)
- Real-time capabilities (optional)
- Admin interface for content management
- API for static generation

Key requirements:

- Easy deployment and maintenance
- Good developer experience
- Secure authentication
- RESTful API
- Elm integration (BackendTask.Http for server-side, elm/http for client-side)
- Cost-effective (preferably self-hosted)

## Decision

We chose **PocketBase** as the backend solution.

## Consequences

### Positive

- Self-hosted with single binary deployment
- Built-in authentication (OAuth2, email/password)
- Admin dashboard for content management
- Real-time subscriptions
- File storage with automatic optimization
- SQLite database (no external DB required)
- REST API with excellent elm-pages integration
- Active development and community

### Negative

- **Integration Pattern**: Requires different approaches for server-side (BackendTask.Http) vs client-side (elm/http) API calls.
- Newer project with evolving API.
- Limited advanced querying compared to PostgreSQL.

## Alternatives Considered

### Supabase

- Considered for its PostgreSQL foundation and features
- Rejected due to preference for self-hosted solution and simpler deployment

### Firebase

- Considered for its real-time capabilities
- Rejected due to vendor lock-in and cost concerns

## Related Decisions

- ADR-0001: Framework Choice (elm-pages)
- ADR-0004: Authentication Method (Server.Session for elm-pages)

## Notes

PocketBase provides an excellent balance of features and simplicity for this project.
With the elm-pages migration, integration happens through:

- **Server-side**: `BackendTask.Http` for data fetching during static generation/server rendering
- **Client-side**: `elm/http` for user interactions after hydration
- **Authentication**: Cookie-based sessions using `Server.Session` API, with JWT fallback for client-side requests

Current `src/PocketBase.elm` module will need refactoring to support both patterns.
