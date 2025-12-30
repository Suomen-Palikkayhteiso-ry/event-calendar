# Import Events

## User Story

As an authenticated member, I want to import events from KML files so that I can bulk-add events to the calendar.

## Acceptance Criteria

- User can upload or input KML data (converted to JSON/structured format).
- System parses KML Placemarks.
- Event Title, Country, and Dates are extracted from the Placemark Name using the format "Title (COUNTRY) DateString" (e.g., "Summer Camp (FIN) 15-20 July").
- Description and Coordinates (Lat/Lon) are extracted from the Placemark.
- Parsed events are initialized in "Draft" state.
- Date parsing supports formats like "mid July", "in July", "July 15-20".
- Failed imports are reported to the user.

## Scenarios

- User provides valid KML data -> Events created as Drafts.
- KML Placemark name missing country/date -> Event created with defaults/warnings.
- Invalid date format -> Event created without dates (or handled as error).
