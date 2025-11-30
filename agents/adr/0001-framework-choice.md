# ADR 0001: Framework Choice - SvelteKit v2

## Status

Accepted

## Context

The project requires a modern web framework for building an event calendar application with the following requirements:
- Server-side rendering (SSR) for SEO and performance
- Static site generation (SSG) for deployment flexibility
- Component-based architecture
- TypeScript support
- Good developer experience
- Active community and ecosystem

The application needs to handle:
- Dynamic calendar views
- Event management (CRUD operations)
- User authentication
- Internationalization
- Static feed generation (RSS, Atom, ICS, etc.)

## Decision

We chose SvelteKit v2 as the web framework for this project.

## Consequences

### Positive
- Excellent performance due to compile-time optimization
- Built-in SSR and SSG capabilities
- Native TypeScript support
- Simple and intuitive component API
- Automatic code splitting
- File-based routing
- Rich ecosystem of integrations

### Negative
- Smaller community compared to React/Next.js
- Learning curve for developers unfamiliar with Svelte
- Some third-party libraries may have less mature Svelte support

## Alternatives Considered

### Next.js (React)
- Considered due to large community and ecosystem
- Rejected because React's runtime overhead and more complex mental model

### Nuxt.js (Vue)
- Considered for its SSR capabilities
- Rejected due to preference for Svelte's simplicity

### Astro
- Considered for its focus on content-heavy sites
- Rejected because SvelteKit provides better integration for interactive components

### Vanilla Svelte with Vite
- Considered for maximum control
- Rejected because SvelteKit's conventions and features (routing, SSR) provide significant value

## Related Decisions

- ADR 0002: Backend Choice (PocketBase)
- ADR 0009: Build and Deployment (Vite, static adapter)

## Notes

SvelteKit v2 was chosen when it became stable, providing improved performance and developer experience over v1.