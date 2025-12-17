# Delete Event

## User Story
As an authenticated member, I want to delete events so that I can remove cancelled or incorrect events.

## Acceptance Criteria
- Authenticated users can change event status to "deleted"
- Deleted events are no longer visible in the calendar
- Success message is shown after deletion
- Deletion is reversible (status change)

## Scenarios
- Auth user creates an event
- Changes status to "deleted" via dropdown
- Event disappears from the list
- Success message is shown