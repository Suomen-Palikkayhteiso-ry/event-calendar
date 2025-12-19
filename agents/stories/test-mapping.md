# Test to User Story Mapping

This document maps test cases to their corresponding user stories.

## tests/features/calendar.feature

- `Scenario: Display the calendar page` -> view-calendar.md
- `Scenario: Navigate calendar dates` -> view-calendar.md
- `Scenario: Display events on calendar` -> view-calendar.md
- `Scenario: Handle date selection` -> view-calendar.md

## tests/features/event-creation.feature

- `Scenario: Redirect non-authenticated users` -> authentication.md, create-event.md
- `Scenario: Create event as authenticated user` -> create-event.md
- `Scenario: Validate required fields` -> create-event.md

## tests/features/event-editing.feature

- `Scenario: Edit existing event` -> edit-event.md, view-event-list.md
- `Scenario: Delete event` -> delete-event.md, view-event-list.md

## tests/features/map.feature

- `Scenario: Display map with events` -> view-events-on-map.md
- `Scenario: Interact with map markers` -> view-events-on-map.md

## tests/features/event-list.feature

- `Scenario: View event list as authenticated user` -> view-event-list.md
- `Scenario: Import events from KML` -> import-events.md

- `calendar-overview` -> view-calendar.md
- `calendar-mobile` -> view-calendar.md
- `calendar-navigation` -> view-calendar.md
- `event-detail` -> view-event-details.md

## tests/DateUtilsTests.elm

- Date parsing and formatting tests -> view-calendar.md, view-event-details.md

## tests/EventUtilsTests.elm

- Event data conversion tests -> create-event.md, edit-event.md, view-calendar.md

## tests/KMLUtilsTests.elm

- KML parsing tests -> import-events.md
