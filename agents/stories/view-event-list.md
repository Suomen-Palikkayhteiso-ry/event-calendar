# View Event List

## User Story

As an authenticated member, I want to view a list of all events so that I can manage and organize events.

## Acceptance Criteria

- Authenticated users can access event list at /events
- List shows all events with title, dates, and status
- Authenticated users see Edit and Delete buttons for each event
- List supports pagination
- Users can import events from KML files
- Events are sorted by date
- Users can filter events by title, date, and status
- Users can sort events by title, date, and status (ascending/descending)

## Scenarios

- Auth user navigates to /events -> sees paginated event list
- Auth user enters text in "Filter by title" -> list updates to show matching events
- Auth user selects a status in filter -> list updates to show events with that status
- Auth user clicks column header (Title/Date/Status) -> list sorts by that column
- Auth user clicks Edit on an event -> navigates to edit form
- Auth user clicks Delete on an event -> event status changes to deleted
- Auth user imports KML file -> events are added to the list
