# ADR 0012: Migrate Frontend to Elm

## Status

Accepted

## Date

2025-12-18

## Context

The current SvelteKit-based frontend, while functional, faces challenges in long-term maintainability and type safety. Runtime errors, complex state management with stores, and the fluidity of the JS ecosystem (Svelte 4 vs 5 shifts) motivate a move to a more stable and robust platform.

## Decision

We will rewrite the frontend application using **Elm**.

Elm provides:
*   **No Runtime Exceptions**: Strong static typing guarantees.
*   **The Elm Architecture (TEA)**: A predictable and unidirectional data flow standard.
*   **Maintainability**: Fearless refactoring due to the compiler.
*   **Performance**: Highly optimized asset size and rendering speed.

The migration will be performed incrementally where possible, but given the fundamental difference in architecture, it effectively involves a rewrite of the client-side logic. The project will exist in a hybrid state where SvelteKit serves the application or legacy components until the Elm application is feature-complete.

## Consequences

### Positive
*   Elimination of runtime exceptions for covered logic.
*   Simplified state management (single source of truth in `Model`).
*   Reduced dependency churn (Elm package ecosystem is stable).

### Negative
*   High initial effort to rewrite existing Svelte components.
*   Learning curve for developers unfamiliar with functional programming.
*   Need to bridge with JavaScript for specific libraries (Leaflet, PocketBase SDK) using Ports.
*   Static generation logic (Node.js scripts) remains outside Elm for now.

### Implementation Strategy
*   `src/Main.elm` becomes the entry point for the new application.
*   Existing Svelte components serve as the reference implementation.
*   `devenv` and `nix` will be used to manage the Elm toolchain.
