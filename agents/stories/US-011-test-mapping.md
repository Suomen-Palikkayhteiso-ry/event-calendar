# Test to User Story Mapping

This document maps test cases to their corresponding user stories.

## tests/features/calendar.feature

- `Scenario: Display the calendar page` -> US-002-view-calendar.md
- `Scenario: Navigate calendar dates` -> US-002-view-calendar.md
- `Scenario: Display events on calendar` -> US-002-view-calendar.md
- `Scenario: Handle date selection` -> US-002-view-calendar.md

## tests/features/event-creation.feature

- `Scenario: Redirect non-authenticated users` -> US-001-authentication.md, US-005-create-event.md
- `Scenario: Create event as authenticated user` -> US-005-create-event.md
- `Scenario: Validate required fields` -> US-005-create-event.md

## tests/features/event-editing.feature

- `Scenario: Edit existing event` -> US-006-edit-event.md, US-003-view-event-list.md
- `Scenario: Delete event` -> US-007-delete-event.md, US-003-view-event-list.md

## tests/features/map.feature

- `Scenario: Display map with events` -> US-012-view-events-on-map.md
- `Scenario: Interact with map markers` -> US-012-view-events-on-map.md

## tests/features/event-list.feature

- `Scenario: View event list as authenticated user` -> US-003-view-event-list.md
- `Scenario: Import events from KML` -> US-008-import-events.md

- `calendar-overview` -> US-002-view-calendar.md
- `calendar-mobile` -> US-002-view-calendar.md
- `calendar-navigation` -> US-002-view-calendar.md
- `event-detail` -> US-004-view-event-details.md

## tests/DateUtilsTests.elm

- Date parsing and formatting tests -> US-002-view-calendar.md, US-004-view-event-details.md

## tests/EventUtilsTests.elm

- Event data conversion tests -> US-005-create-event.md, US-006-edit-event.md, US-002-view-calendar.md

## tests/KMLUtilsTests.elm

- KML parsing tests -> US-008-import-events.md
