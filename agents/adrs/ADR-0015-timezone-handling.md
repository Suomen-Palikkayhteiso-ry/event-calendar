# ADR 0015: Timezone Handling - Backend UTC, UI Helsinki

## Status

Accepted

## Date

2025-12-28

## Context

The application handles dates and times for events, and needs to display them correctly to users in Finland. The backend stores timestamps in UTC, but the UI should display times in Helsinki timezone (Europe/Helsinki), which includes Daylight Saving Time (DST) adjustments.

Current implementation approximates Helsinki time as UTC+2, but does not account for DST, leading to incorrect time display during summer months.

## Decision

- Backend must store all timestamps in UTC.
- UI must display all dates and times in Helsinki timezone (Europe/Helsinki), properly handling DST.
- Helsinki timezone rules: UTC+2 in winter (typically October to March), UTC+3 in summer (typically March to October).

## Consequences

### Positive

- Accurate time display for Finnish users.
- Consistent with backend data integrity (UTC storage).
- Compliance with Finnish timezone standards.

### Negative

- Increased complexity in date formatting logic.
- Need to maintain DST rules (though Elm's Time module can help).

### Implementation Notes

- Use Elm's Time module to calculate correct offsets.
- Update DateUtils.formatDateInHelsinki to properly handle DST.
- Ensure all date displays in the UI use this function.

## Alternatives Considered

### Fixed UTC+2 offset

Keep the current simple implementation. Rejected because it shows incorrect times during DST period.

### User-configurable timezone

Allow users to select their timezone. Rejected as overkill for a Finland-focused application.