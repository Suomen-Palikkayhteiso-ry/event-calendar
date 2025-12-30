# Project Status & TODOs

This document tracks the remaining tasks to complete the migration from SvelteKit to Elm.

## 1. Core UI Wiring (High Priority)
The application shell (`View.elm`) is in place, but the sub-views are not fully connected.

- [ ] **Connect Event List (US-003):**
    - Update `View.elm` to render `EventList.view` in the `Routes.EventsRoute` case.
    - Ensure `EventList.Msg` are mapped correctly in `Update.elm`.
- [ ] **Connect Event Detail (US-004):**
    - Update `View.elm` to render `EventDetail.view` in the `Routes.EventDetail` case.
    - Ensure fetching the specific event data works when landing on this route.
- [ ] **Connect Event Form (Create/Edit) (US-005, US-006):**
    - Update `View.elm` to render `EventForm.view` in `Routes.CreateEvent` and `Routes.EditEvent`.
    - Wire up the form submission logic to `PocketBase.createEvent`/`updateEvent`.

## 2. Feature Implementation
Some features have logic but missing UI or full integration.

- [ ] **Import Events UI (US-008):**
    - Create a UI in `EventForm.elm` or a new `ImportView.elm` to accept KML files (or text input).
    - Connect the file input to `Ports.parseKMLContent`.
    - Handle the `kmlContentParsed` subscription in `Update.elm`.
    - Use `KMLUtils.elm` to process the parsed data into `Event` records (Draft state).
    - Display a "Review Imported Events" list where users can edit/confirm before saving to PocketBase.
- [ ] **Map Integration (US-012):**
    - Ensure `Map.view` is correctly rendered in `Routes.MapRoute`.
    - Verify that `Map.elm` sends `initMap` and `updateMap` commands to ports when data changes.
    - Test the `mapMarkerMoved` subscription to ensure dragging a marker updates the event location (in Create/Edit mode).

## 3. Authentication & UX
- [ ] **Login/Logout Flow (US-001):**
    - Verify the Login button in `View.elm` calls `Ports.initiateOAuth2Login`.
    - Ensure the "Logged in" state persists on page refresh (checking `localStorage` via ports).
    - Show/Hide "Create/Edit/Delete" buttons based on `model.auth` state.
- [ ] **Feedback & Error Handling:**
    - Implement a global notification/toast system for success (e.g., "Event Created") and error messages.
    - Handle network errors gracefully in `Events.elm` and `PocketBase.elm`.

## 4. Mobile Responsiveness (US-010)
- [ ] **Navigation:** Implement a hamburger menu for mobile in `View.headerView`.
- [ ] **Calendar View:** Ensure `CalendarView.elm` collapses gracefully on small screens (e.g., stack days or use a different layout).
- [ ] **Map:** Ensure map controls are touch-friendly.

## 5. Testing & Validation
- [ ] **Unit Tests:** Add tests for `EventForm` validation logic.
- [ ] **E2E Tests:** Run `playwright` tests to verify the critical paths (Login -> Create -> View -> Delete).
- [ ] **Feed Verification:** Manually verify that the links in the footer (`/feeds/*`) work (requires running the Haskell generation scripts).

## 6. Cleanup
- [ ] Remove any unused SvelteKit artifacts if present.
- [ ] Ensure `I18n` keys are fully utilized in all new views.
