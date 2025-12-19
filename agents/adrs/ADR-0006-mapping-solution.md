# ADR 0006: Mapping Solution - Leaflet via Elm Ports

## Status

Accepted (Updated for elm-pages migration)

## Date

2025-12-19 (Updated: 2025-01-XX)

## Context

The application needs to display event locations on maps for:

- Event creation (location input with geocoding)
- Event display (embedded maps)
- Geographic data export (GeoJSON)

With the migration to elm-pages (ADR-0001), we maintain Leaflet integration via Elm Ports, which works seamlessly with elm-pages' hydration model.

## Decision

We chose **Leaflet**, integrated via **Elm Ports**.

## Consequences

### Positive

- Lightweight and fast
- Extensive plugin ecosystem
- Good mobile support
- Open-source with permissive license
- Mature and stable
- **elm-pages Compatible**: Ports work normally after client-side hydration

### Negative

- **Complexity**: Requires managing state synchronization between Elm (Model) and JavaScript (Leaflet instance) via Ports.
- **No Native Elm Package**: We are not using `elm-leaflet` (if applicable) but a custom ports-based approach for maximum control.

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

## Related Decisions

- ADR-0001: Framework Choice (elm-pages)
- ADR-0009: Build System (Nix Flake + devenv)

## Notes

The map logic resides in `src/Map.elm` (Model/View) and corresponding JavaScript code in `src/index.js` (Leaflet initialization and Port subscription). This architecture works perfectly with elm-pages since maps are client-side only and ports function normally after hydration.
