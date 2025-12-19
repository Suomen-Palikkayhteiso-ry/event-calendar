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

- `Scenario: Edit existing event` -> edit-event.md
- `Scenario: Delete event` -> delete-event.md

## tests/screenshots.spec.ts

- `calendar-overview` -> view-calendar.md
- `calendar-mobile` -> view-calendar.md
- `calendar-navigation` -> view-calendar.md
- `event-detail` -> view-calendar.md
