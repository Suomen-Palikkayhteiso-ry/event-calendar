# ADR 0005: Internationalization - svelte-i18n

## Status

Accepted

## Context

The application serves Finnish users and requires:
- Finnish language interface
- Date/time formatting in Finnish locale
- Extensible for future languages
- Good integration with SvelteKit

## Decision

We chose svelte-i18n for internationalization.

## Consequences

### Positive
- Native Svelte integration with stores
- Reactive language switching
- Date/time formatting utilities
- Pluralization support
- Good TypeScript support
- Active maintenance

### Negative
- Additional bundle size
- Learning curve for ICU message format

## Alternatives Considered

### Custom i18n solution
- Considered for minimal bundle size
- Rejected due to development overhead

### react-i18next
- Considered for feature completeness
- Rejected due to React-specific nature

### Built-in browser i18n APIs
- Considered for zero dependencies
- Rejected due to limited functionality

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2)

## Notes

svelte-i18n provides good integration with Svelte's reactivity system and meets our current needs for Finnish localization.