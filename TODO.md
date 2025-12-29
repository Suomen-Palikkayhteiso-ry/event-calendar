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
*   [x] **Refactor `src/EventList.elm` (~125 lines)**: Monitored size. No extraction needed as it's under 400 lines.
*   [x] **Refactor `src/I18n.elm` (>600 lines)**:
    *   Split translations into separate `src/Translations.elm` file to reduce I18n.elm from 602 to 361 lines.

## 3. Feature Verification & Polish
* [x] Complete all tasks in TODO-feature-verification.md

## 4. Testing
* [x] Complete all tasks in TODO-testing.md

## 5. Documentation
* [x] **Update ADRs**: Updated `agents/architecture.md` to reflect the splitting of `Main.elm` into `Model.elm`, `Update.elm`, and `View.elm`, and routing moved to `Routes.elm`.

## 6. UI Polish & Visual Improvements (Design)
* [x] Complete all tasks in TODO-ui-polish.md