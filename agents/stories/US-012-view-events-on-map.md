# View Events on Map

## User Story

As a user, I want to view events geographically on a map so that I can see event locations and plan attendance.

## Acceptance Criteria

- Map page is accessible at /map route
- Map displays event markers at their geographical coordinates
- Map centers on Helsinki by default
- Users can zoom and pan the map
- Clicking on markers shows event information
- Map integrates with Leaflet library via Ports

## Scenarios

- User navigates to /map -> sees map with event markers
- User zooms/pans map -> map updates accordingly
- User clicks marker -> sees event popup/tooltip
