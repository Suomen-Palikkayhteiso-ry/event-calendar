# ADR 0006: Mapping Solution - Leaflet

## Status

Accepted

## Context

The application needs to display event locations on maps for:
- Event creation (location input with geocoding)
- Event display (embedded maps)
- Geographic data export (GeoJSON)

Requirements:
- Open-source and free
- Good performance
- Customizable
- Integration with Svelte
- Mobile-friendly

## Decision

We chose Leaflet as the mapping library.

## Consequences

### Positive
- Lightweight and fast
- Extensive plugin ecosystem
- Good mobile support
- Open-source with permissive license
- Mature and stable
- Good integration options

### Negative
- Requires additional setup for Svelte
- Less "modern" API compared to newer libraries

## Alternatives Considered

### Google Maps
- Considered for ease of use
- Rejected due to cost and terms of service

### Mapbox
- Considered for styling options
- Rejected due to cost concerns

### OpenLayers
- Considered for advanced features
- Rejected due to complexity

### MapLibre
- Considered as modern alternative
- Rejected due to less mature ecosystem

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2)

## Notes

Leaflet with svelte-leaflet provides good integration and meets all mapping requirements without licensing costs.