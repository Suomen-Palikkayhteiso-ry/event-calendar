# View Event Details

## User Story

As a user (member or non-member), I want to view detailed information about a specific event so that I can learn more about the event.

## Acceptance Criteria

- Users can navigate to event details by clicking on event titles/links
- Event details page shows title, description, dates, location, and image
- Authenticated users see Edit and Delete buttons
- Non-authenticated users do not see management buttons
- Page includes a "Back to Calendar" link
- Event URL is displayed as a clickable link if present
- Dates are formatted in Finnish locale
- Location information is displayed

## Scenarios

- User clicks on an event in the calendar -> navigates to event detail page
- Auth user views event details -> sees Edit and Delete buttons
- Non-auth user views event details -> no management buttons visible
- User clicks "Back to Calendar" -> returns to calendar view