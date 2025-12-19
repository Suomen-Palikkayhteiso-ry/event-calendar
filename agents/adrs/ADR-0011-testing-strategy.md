# ADR 0011: Testing Strategy - Pure Elm with elm-pages

## Status

Accepted (Updated for elm-pages migration)

## Date

2025-12-19 (Updated: 2025-01-XX)

## Context

The project requires a robust testing strategy for the pure Elm application using elm-pages framework.
With the migration to elm-pages (ADR-0001), we need to adopt Elm-native testing tools that work with the elm-pages architecture.

## Decision

We adopt the following testing strategy for the pure Elm stack:

1.  **Unit Tests (Elm)**: Use `elm-test` for pure logic (Utils, Update functions, Encoders/Decoders, BackendTask logic).
2.  **Integration Tests (Elm)**: Use `elm-program-test` for testing complete Elm program behavior including HTTP requests, ports, and user interactions.
3.  **CI/CD**: Run `elm-test` and `elm-program-test` on every push using `make elm-check`.

## Consequences

### Positive

- **Pure Elm Ecosystem**: All tests written in Elm, eliminating TypeScript/JavaScript dependencies.
- **Complete Program Testing**: `elm-program-test` can test full application flows including HTTP, ports, and user interactions.
- **Type Safety**: Tests benefit from Elm's type system and compile-time guarantees.
- **Fast Feedback**: Both unit and integration tests run quickly without browser overhead.

### Negative

- **No Real Browser Testing**: Cannot test browser-specific behaviors or CSS interactions.
- **Limited Visual Testing**: Cannot verify actual rendering or accessibility features.

## Alternatives Considered

### Playwright (Previous Choice)

- Previously used for E2E testing in the SvelteKit hybrid architecture.
- Provides "real browser" testing but introduces TypeScript dependency and complexity.
- Not suitable for pure Elm stack as it cannot directly test elm-pages applications.

### Browser Automation Tools

- Considered Cypress, Selenium, or Puppeteer for browser testing.
- All introduce JavaScript/TypeScript dependencies that conflict with pure Elm goal.
- `elm-program-test` provides better Elm integration for testing application logic.

## Related Decisions

- ADR-0001: Framework Choice (elm-pages)
- ADR-0009: Build System (Nix Flake + devenv)
- ADR-0010: Code Quality (elm-format + elm-review)

## Notes

- `elm-test` covers modules like `DateUtils`, `EventUtils`, `KMLUtils`.
- `elm-program-test` covers route navigation, form submissions, HTTP interactions, and port communications.
- Tests reference US and ADR IDs in headers/docstrings.
- Playwright covers user stories like "User logs in", "User creates event".
