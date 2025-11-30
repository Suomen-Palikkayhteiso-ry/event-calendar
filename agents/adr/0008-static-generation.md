# ADR 0008: Static Generation Approach

## Status

Accepted

## Context

The application needs to generate static feeds for:
- RSS/Atom feeds for syndication
- ICS calendar files for import
- JSON/GeoJSON for data access
- HTML embed for external sites
- Individual event pages

Requirements:
- Automated generation from PocketBase data
- Multiple output formats
- SEO-friendly
- Fast loading
- Offline-capable

## Decision

We implemented custom static generation scripts using Node.js.

## Consequences

### Positive
- Full control over output formats
- Optimized for specific use cases
- No build-time dependencies
- Fast generation
- Custom formatting and features

### Negative
- Custom development required
- Maintenance overhead
- Potential for inconsistencies

## Alternatives Considered

### SvelteKit's built-in SSG
- Considered for integration
- Rejected due to complex data requirements

### Eleventy or similar SSG
- Considered for content focus
- Rejected due to data-driven nature

### PocketBase webhooks
- Considered for automation
- Rejected due to deployment complexity

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2)
- ADR 0002: Backend Choice (PocketBase)

## Notes

Custom scripts provide flexibility for generating multiple feed formats and handling complex data transformations.