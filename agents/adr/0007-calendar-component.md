# ADR 0007: Calendar Component - @event-calendar/core

## Status

Accepted

## Date

2025-11-30

## Context

The application requires a calendar component for:

- Monthly calendar view
- Event display and navigation
- Date selection
- Integration with event data
- Responsive design
- Accessibility

Requirements:

- Svelte compatible
- Customizable styling
- Good performance
- Active maintenance

## Decision

We chose @event-calendar/core for the calendar component.

## Consequences

### Positive

- Native Svelte support
- Highly customizable
- Good performance
- Accessibility features
- Active development
- Multiple view options (month, list)

### Negative

- Relatively new library
- Some TypeScript definitions incomplete
- Learning curve for advanced customization

## Alternatives Considered

### FullCalendar

- Considered for feature completeness
- Rejected due to React/Angular focus and complexity

### Custom calendar implementation

- Considered for full control
- Rejected due to development time

### react-calendar

- Considered for React ecosystem
- Rejected due to framework mismatch

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2)

## Notes

@event-calendar/core provides excellent integration with Svelte and meets all calendar display requirements.
