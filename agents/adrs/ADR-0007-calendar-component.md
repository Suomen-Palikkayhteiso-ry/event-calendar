# ADR 0007: Calendar Component - Custom Elm Implementation

## Status

Accepted (Updated for elm-pages migration)

## Date

2025-12-19 (Updated: 2025-01-XX)

## Context

The application requires a calendar component for viewing events.
With the migration to elm-pages (ADR-0001), we maintain the custom Elm calendar implementation, which integrates seamlessly with elm-pages' architecture.

## Decision

We chose to implement a **Custom Calendar Component** in Elm (`src/Calendar.elm`).

## Consequences

### Positive

- **Full Control**: We own the rendering logic and state management.
- **Type Safety**: Integration with our `Event` types is seamless and checked by the compiler.
- **No External Dependencies**: Reduces bundle size and third-party risk.
- **Performance**: Optimized for our specific use case (monthly view).
- **elm-pages Compatible**: Pure Elm component works perfectly with elm-pages routing and state management.

### Negative

- **Development Effort**: requires implementing calendar logic (date math, grid layout) manually.
- **Feature Parity**: May lack advanced features present in mature libraries (like complex drag-and-drop) until implemented.

## Alternatives Considered

### Elm Packages (e.g., elm-calendar)

- Considered but found either outdated or too opinionated for our styling needs.

### @event-calendar/core (Legacy)

- Used in SvelteKit version.
- Deprecated by Elm migration.

## Related Decisions

- ADR-0001: Framework Choice (elm-pages)
- ADR-0003: Styling Approach (Tailwind CSS)

## Notes

The custom implementation focuses on a monthly view with event indicators, matching the specific design requirements of the project. The component integrates naturally with elm-pages' route-based architecture and shared state management.
