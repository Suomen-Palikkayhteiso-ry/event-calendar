# Create Event

## User Story

As an authenticated member, I want to create new events so that I can add them to the community calendar.

## Acceptance Criteria

- Authenticated users can access the event creation form at /events
- Non-authenticated users are redirected to home page with membership instructions
- The form requires title, location, and description
- Form submission shows success message
- Validation errors are displayed for required fields
- Created events appear in the calendar

## Scenarios

- Non-auth user tries to access /events -> redirected to /
- Auth user fills and submits form -> success message
- Auth user submits empty form -> validation errors shown
