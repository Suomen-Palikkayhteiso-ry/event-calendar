# Test to User Story Mapping

This document maps test cases to their corresponding user stories.

## calendar.spec.ts
- `should display the calendar page` -> view-calendar.md
- `should navigate calendar dates` -> view-calendar.md
- `should display events on calendar` -> view-calendar.md
- `should handle date selection` -> view-calendar.md

## event-creation.spec.ts
- `should redirect non-authenticated users to home page` -> authentication.md, create-event.md
- `should allow authenticated users to create events` -> create-event.md
- `should validate required fields` -> create-event.md

## event-editing.spec.ts
- `should allow editing existing events` -> edit-event.md
- `should allow deleting events` -> delete-event.md
- `should handle event detail viewing` -> view-calendar.md (implicit)