# ADR 0002: Backend Choice - PocketBase

## Status

Accepted

## Date

2025-11-30

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
- TypeScript support
- Cost-effective (preferably self-hosted)

## Decision

We chose PocketBase as the backend solution.

## Consequences

### Positive
- Self-hosted with single binary deployment
- Built-in authentication (OAuth2, email/password)
- Admin dashboard for content management
- Real-time subscriptions
- File storage with automatic optimization
- SQLite database (no external DB required)
- REST API and JavaScript SDK
- TypeScript types generation
- Active development and community

### Negative
- Newer project with evolving API
- Limited advanced querying compared to PostgreSQL
- Single binary may have resource constraints for high traffic

## Alternatives Considered

### Supabase
- Considered for its PostgreSQL foundation and features
- Rejected due to preference for self-hosted solution and simpler deployment

### Firebase
- Considered for its real-time capabilities
- Rejected due to vendor lock-in and cost concerns

### Directus
- Considered for its headless CMS features
- Rejected due to more complex setup compared to PocketBase

### Custom Node.js/Express API
- Considered for maximum control
- Rejected due to increased development and maintenance overhead

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2)
- ADR 0004: Authentication Method (OAuth2 OIDC)

## Notes

PocketBase provides an excellent balance of features and simplicity for this project. The admin interface allows non-technical users to manage content, while the API serves the application's needs.