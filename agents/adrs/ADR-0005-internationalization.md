# ADR 0005: Internationalization - Elm Custom Module

## Status

Accepted (Updated for elm-pages migration)

## Date

2025-12-19 (Updated: 2025-01-XX)

## Context

The application serves Finnish users and requires a Finnish language interface.
With the migration to elm-pages (ADR-0001), we maintain the custom Elm I18n module approach, which integrates seamlessly with elm-pages' architecture.

## Decision

We chose a custom Elm module (`src/I18n.elm`) for internationalization.
Translations are defined as Elm records, ensuring type safety and compile-time checks for missing keys.

## Consequences

### Positive

- **Type Safety**: The compiler ensures all keys exist. No runtime "missing key" errors.
- **Simplicity**: No external dependencies or complex setup.
- **Performance**: Pure Elm functions, no runtime parsing overhead.
- **Maintainability**: centralized translation file.
- **elm-pages Compatible**: Pure Elm module works perfectly with elm-pages routing and shared state.

### Negative

- **Manual Maintenance**: Adding new keys requires updating the Record type and the implementation.
- **Pluralization**: Must be implemented manually (functions) rather than standard ICU format strings.
- **Dynamic Loading**: Translations are bundled with the app (not lazy-loaded), though fine for this scale.

## Alternatives Considered

### elm-i18n (package)

- Considered for standard features.
- Rejected due to desire for minimal dependencies and simplicity.

### svelte-i18n (Legacy)

- Used in the SvelteKit version.
- Deprecated by Elm migration.

### URL-based i18n with elm-pages

- Could implement `/fi/page` and `/en/page` routes using elm-pages' file-based routing.
- Rejected for this project due to single-language requirement (Finnish only).

## Related Decisions

- ADR-0001: Framework Choice (elm-pages)
- ADR-0003: Styling Approach (Tailwind CSS)

## Notes

Currently hardcoded for Finnish (`fi` record). Extensible to other languages by creating matching records. The module integrates naturally with elm-pages' route-based architecture.
