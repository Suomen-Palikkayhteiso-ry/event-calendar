# Create Event

## User Story

As an authenticated member, I want to create new events so that I can add them to the community calendar.

## Acceptance Criteria

- Authenticated users can access the event creation form at /events
- Non-authenticated users are redirected to home page with membership instructions
- The form requires title, location, and description
- User can search for a location address to automatically fill coordinates (Geocoding)
- User can manually enter Latitude and Longitude if geocoding is disabled
- User can toggle geocoding on/off
- Form submission shows success message
- Validation errors are displayed for required fields and invalid coordinates
- Created events appear in the calendar

## Related

- **Tests**: `tests/features/event-creation.feature`
- **ADRs**: ADR-0004 (Authentication), ADR-0006 (Mapping/Geocoding), ADR-0007 (Calendar)

## Scenarios

- Non-auth user tries to access /events -> redirected to /
- Auth user fills and submits form -> success message
- Auth user searches for address -> coordinates filled
- Auth user enters manual coordinates -> form valid
- Auth user submits empty form -> validation errors shown
