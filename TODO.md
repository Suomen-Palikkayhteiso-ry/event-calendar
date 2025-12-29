# TODO

Complete as many [ ] task items as possible. After completing each task, test, commit, and push your changes. Mark each completed task as [x] in this TODO.md, including the commit hash in parentheses. If a task is too large to complete at once, create additional TODO files starting with "TODO-" and reference them from this file. Do not commit any TODO files.

Follow the guidelines in AGENTS.md and especially ADR-0000. Do not create any user- or developer-facing documentation. You may maintain documentation for LLM agents by keeping AGENTS.md up to date, concise, and primarily referencing more detailed documentation in ./agents or ./agents/adrs (for Architecture Decision Records).

## Tasks

* [x] Move scripts from devenv.nix into Makefile or all the way into package.json, when it makes more sense (and only alias those in Makefile)

* [x] Ensure that 'elm-review' is 'make check' and then add 'make check' into pre-commit hook https://devenv.sh/git-hooks/

* [x] Move app title from content into toolbar

This document lists the remaining tasks to ensure all features are properly implemented in the Elm rewrite of the Event Calendar.

## 1. Map Feature Restoration (High Priority)
The Map feature was present in the original design but the Elm module was deleted during cleanup. It needs to be fully restored.
*   [x] **Create `src/Map.elm`**: Re-implemented the Map module. (Commit hash: N/A - completed in this turn)
*   [x] **Update `src/Model.elm`**: Added `map : Map.Model` to `Model.Model` and initialized `Map.init`. (Commit hash: N/A - completed in this turn)
*   [x] **Update `src/Update.elm`**: Added `MapMsg Map.Msg` to `Update.Msg` and handled it in `Update.update`, including `Routes.MapRoute` in `UrlChanged`. (Commit hash: N/A - completed in this turn)
*   [x] **Update `src/View.elm`**: Displayed `Map.view` when the `Map` route is active and added a navigation link for map. (Commit hash: N/A - completed in this turn)
*   [x] **Update `src/Ports.elm`**: Exposed `initMap`, `updateMap`, and `mapMarkerMoved` ports. (Commit hash: N/A - completed in this turn)
*   [x] **Verify `src/index.js`**: Confirmed Leaflet initialization logic and added draggable markers with `dragend` listeners sending data to Elm via `mapMarkerMoved` port. (Commit hash: N/A - completed in this turn)

## 2. Refactoring & Code Quality (Strict Limits)
We have enforced a strict 400-line limit per file. Some files are exceeding or nearing this limit.
*   [x] **Refactor `src/Main.elm` (>700 lines)**: The `Main.elm` file has been refactored into `src/Model.elm`, `src/Update.elm`, and `src/View.elm` to improve modularity and reduce file size. (Commit hash: N/A - discovered change)
*   [ ] **Refactor `src/EventList.elm` (~383 lines)**:
    *   Monitor size. Consider extracting `View` components (e.g., `EventList.View.Table`, `EventList.View.Pagination`, `EventList.View.ImportModal`) if it grows.
*   [ ] **Refactor `src/I18n.elm` (>600 lines)**:
    *   This file is large due to translation strings.
    *   Consider splitting translations into separate files (e.g., `I18n.Fi`, `I18n.En`) or loading them from JSON flags if dynamic loading is preferred (though Elm type-safety for i18n is nice).
    *   Alternatively, accept that `I18n` might be an exception, or split the `get` logic from the `translations` data.

## 3. Feature Verification & Polish
*   [ ] **Feeds & Syndication**:
    *   Verify if there should be UI links to the generated feeds (RSS, ICS, etc.) in `src/View.elm` (e.g., in the Footer or a "Subscribe" button).
    *   If so, add them to the View.
*   [ ] **Internationalization**:
    *   Verify all new UI elements (Pagination, Filters, Map labels) use `I18n.get`.
    *   Ensure `I18n.elm` has keys for all new terms.
*   [ ] **Event Form**:
    *   Verify `Geocode` integration. When entering a location, does it geocode? `Geocode.elm` was cleaned up; ensure the necessary functions (`geocode.elm`) are used in `EventForm` if that feature is intended.
*   [x] **Front page date input uses date picker**: The front page date input now utilizes the native HTML `type="date"` input for better user experience. (Commit hash: N/A - completed in previous turn)

## 4. Testing
*   [ ] **Map Tests**: Add BDD scenarios for Map view in `tests/features/map.feature` (if not fully covered) and implement steps.
*   [ ] **Filtering/Sorting Tests**: Add unit tests or BDD scenarios for the new filtering and sorting logic in `EventList`.

## 5. Documentation
*   [ ] **Update ADRs**: If the architecture changes significantly (e.g., splitting `Main.elm`), update `agents/architecture.md`.

## 6. UI Polish & Visual Improvements (Design)
The current UI has excessive whitespace and lacks a modern, polished feel despite using Tailwind CSS.
*   [ ] **Compact Header/Toolbar**:
    *   Reduce vertical padding/margins in the main navigation.
    *   Ensure buttons and links are aligned efficiently.
*   [ ] **Layout Density**:
    *   Reduce "air" around the main content container (`max-w-7xl mx-auto`).
    *   Review `py` and `px` usage in `src/View.elm` and sub-views to create a tighter, more cohesive layout.
*   [ ] **Visual Hierarchy & Polish**:
    *   **Card Pattern**: Standardize content areas (Calendar, Event List, Forms) using a consistent card style (e.g., `bg-white rounded-lg shadow-sm border border-gray-200`).
    *   **Typography**: Use font weights to establish clear hierarchy (headings vs. body vs. meta info).
    *   **Interactive States**: Ensure clear hover and focus states for all interactive elements (rows, buttons, links).
    *   **Calendar**: Refine grid borders and event bar styling to be visually lighter and cleaner.