# ADR 0012: Migration Plan - Pure Elm with elm-pages

## Status

Proposed

## Date

2025-01-XX

## Context

All existing ADRs have been reviewed and confirmed compatible with elm-pages migration. The current architecture uses SvelteKit as the primary framework with Elm embedded as components. We need a comprehensive migration plan to transition to a pure Elm stack using elm-pages.

## Decision

We will migrate from SvelteKit + Elm hybrid to pure elm-pages architecture following this phased approach:

### Phase 1: Infrastructure Migration

1. **Replace SvelteKit with elm-pages** - Migrate routing, pages, and build system
2. **Update build system** - Remove Vite, pnpm, TypeScript, ESLint, Prettier
3. **Migrate PocketBase integration** - Update from Ports/JS SDK to BackendTask.Http + elm/http
4. **Update testing strategy** - Replace Playwright with elm-program-test

### Phase 2: Component Migration

1. **Migrate Svelte components to Elm** - Convert remaining UI components
2. **Update authentication flow** - Implement Server.Session-based auth
3. **Migrate forms and interactions** - Use Pages.Form API
4. **Update static generation** - Use BackendTask for feeds and data

### Phase 3: Optimization and Cleanup

1. **Remove legacy dependencies** - Clean up package.json, remove unused files
2. **Update deployment** - Configure for elm-pages hosting requirements
3. **Performance optimization** - Leverage elm-pages' static generation benefits

## Consequences

### Positive

- **Pure Elm Architecture**: Eliminates JavaScript/TypeScript complexity and interop issues
- **Type Safety**: Full Elm type system coverage across entire application
- **Simplified Build**: Single toolchain (Nix + elm-pages) instead of multiple tools
- **Better Performance**: Static generation + optimized hydration
- **Easier Maintenance**: Single language, single framework

### Negative

- **Migration Effort**: Significant refactoring required for all components
- **Learning Curve**: Team needs to learn elm-pages patterns and APIs
- **Breaking Changes**: All routes, components, and integrations need updates
- **Temporary Complexity**: Hybrid state during migration period

## Implementation Details

### File Structure Changes

```
Current (SvelteKit):
src/
├── routes/          # SvelteKit routes
├── lib/            # Shared utilities
├── components/     # Svelte components
├── stores/         # Svelte stores
└── app.html        # SvelteKit shell

Target (elm-pages):
src/                 # Elm source (unchanged)
app/Route/          # NEW: elm-pages routes
├── Index.elm
├── Events.elm
└── ...
shared/             # NEW: Shared Elm modules
├── Shared.elm
├── Site.elm
└── ...
```

### Key API Changes

- **HTTP Requests**: `src/PocketBase.elm` needs BackendTask.Http for server-side + elm/http for client-side
- **Authentication**: Replace JWT handling with Server.Session API
- **Forms**: Replace Svelte forms with Pages.Form API
- **Routing**: Replace SvelteKit routing with elm-pages file-based routing
- **Static Generation**: Replace SvelteKit's +page.server.ts with BackendTask

### Testing Strategy

- **Unit Tests**: Keep elm-test for pure functions
- **Integration Tests**: Use elm-program-test for complete application flows
- **Build Validation**: Use `make elm-check` for comprehensive validation

## Migration Checklist

### Infrastructure

- [ ] Set up elm-pages project structure
- [ ] Configure Nix flake for elm-pages
- [ ] Remove Vite, pnpm, TypeScript dependencies
- [ ] Update CI/CD to use elm-pages build commands

### Core Components

- [ ] Migrate main layout and navigation
- [ ] Convert event list/calendar views to Elm
- [ ] Implement elm-pages routing for all pages
- [ ] Update PocketBase integration for dual HTTP patterns

### Features

- [ ] Implement Server.Session authentication
- [ ] Convert forms to Pages.Form API
- [ ] Update map integration (ports remain compatible)
- [ ] Migrate static feed generation to BackendTask

### Quality Assurance

- [ ] Update all tests to work with new architecture
- [ ] Validate SEO and performance requirements
- [ ] Test deployment and hosting configuration
- [ ] Update documentation

## Rollback Plan

If migration encounters blocking issues:

1. Maintain current SvelteKit version as stable fallback
2. Implement elm-pages migration incrementally per route
3. Use feature flags to switch between implementations
4. Plan for complete migration within 3-6 months

## Success Criteria

- All user-facing functionality preserved
- Performance meets or exceeds current benchmarks
- Build and deployment processes simplified
- Codebase fully type-safe with Elm
- Maintenance burden reduced

## Related Decisions

- ADR-0001: Framework Choice (elm-pages)
- ADR-0009: Build System (Nix Flake + devenv)
- ADR-0011: Testing Strategy (elm-program-test)
- ADR-0002: Backend Choice (PocketBase)
- ADR-0004: Authentication Method (Server.Session)

## Notes

This migration represents a significant architectural shift toward Elm purity. The phased approach minimizes risk while providing clear milestones. All reviewed ADRs confirm technical feasibility of the migration.
