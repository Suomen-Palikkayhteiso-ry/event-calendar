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

## Scenarios

- Auth user navigates to /events -> sees paginated event list
- Auth user clicks Edit on an event -> navigates to edit form
- Auth user clicks Delete on an event -> event status changes to deleted
- Auth user imports KML file -> events are added to the list