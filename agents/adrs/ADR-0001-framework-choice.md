# ADR 0001: Framework Choice - SvelteKit v2 & Elm

## Status

Accepted

## Date

2025-12-19

## Context

The project requires a modern web framework.
Originally started as a **SvelteKit v2** application, the project is now migrating to **Elm** (see ADR-0012) for improved type safety and maintainability.

Current requirements:

- Hosting the Elm application (entry point).
- Serving legacy Svelte components during migration.
- Static site generation (SSG) for feeds/assets.

## Decision

We use **SvelteKit v2** as the build/hosting framework, with **Elm** as the primary frontend technology for new development.

## Consequences

### Positive

- **Best of Both**: SvelteKit handles build tooling (Vite), static generation, and routing shell. Elm handles complex business logic and views.
- **Incremental Migration**: Legacy Svelte components run alongside the new Elm app.

### Negative

- **Complexity**: Two build systems (Vite for JS/Svelte, `elm-make` for Elm).
- **Interop**: Passing data between SvelteKit (shell) and Elm (app) requires Ports/Flags.

## Alternatives Considered

### Pure Elm (elm-pages)

Explored as a complete replacement for SvelteKit v2. elm-pages is a statically typed site generator for Elm that provides:

- File-based routing with dynamic segments
- Pre-rendered and server-rendered routes
- BackendTasks for data fetching (HTTP, files, CMS)
- Type-safe forms with progressive enhancement
- Session management for authentication
- SEO optimization
- Static site generation capabilities

**Feature Parity Assessment:**

- ✅ Routing: File-based routing matches SvelteKit's conventions
- ✅ Static Generation: Excellent SSG for feeds (RSS, Atom, ICS)
- ✅ Authentication: Session API supports OIDC integration
- ✅ Forms: Built-in form handling with validations
- ✅ Performance: Pure Elm with optimized static generation
- ✅ Type Safety: Full Elm type system throughout

**Migration Path:**

- Current Elm components (Calendar, Map, Forms) are directly compatible
- PocketBase integration via BackendTasks and server-rendered routes
- Existing Elm code can be migrated with minimal changes
- Build system shifts from Vite + elm-watch to elm-pages CLI

**Considered but requires further evaluation:**

- Integration with existing PocketBase backend
- Handling of complex calendar interactions
- Deployment and hosting options
- Development workflow differences

This is now the **recommended path** for achieving a pure Elm architecture, eliminating SvelteKit, Vite, TypeScript, and pnpm dependencies.

- Considered due to large community and ecosystem
- Rejected because React's runtime overhead and more complex mental model

### Nuxt.js (Vue)

- Considered for its SSR capabilities
- Rejected due to preference for Svelte's simplicity

## Related Decisions

- ADR 0002: Backend Choice (PocketBase)
- ADR 0009: Build and Deployment (Vite, static adapter)
- ADR 0012: Migrate Frontend to Elm

## Notes

SvelteKit v2 provides the infrastructure (Vite, SSG), while Elm provides the application logic.
