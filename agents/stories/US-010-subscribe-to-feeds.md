# Subscribe to Feeds

## User Story

As a user, I want to subscribe to event feeds (RSS, Atom, iCal) so that I can receive updates in my preferred feed reader or calendar application.

## Acceptance Criteria

- Links to RSS, Atom, JSON, GeoJSON, and iCal (ICS) feeds are clearly visible (e.g., in the footer or a dedicated "Subscribe" section).
- Feeds are valid and strictly typed (validate against standards).
- iCal feed imports correctly into Google Calendar, Outlook, and Apple Calendar.
- GeoJSON feed works with standard map tools.
- Feeds contain up-to-date event information.

## Scenarios

- User clicks "iCal Feed" -> Browser downloads .ics file or opens Calendar app.
- User copies "RSS Feed" link -> Pastes into Feedly/NewsBlur -> Sees event updates.
