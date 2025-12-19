# Project Analysis & Migration Status

## Comparison: SvelteKit (refactor) vs. Elm (current)

### Feature Parity

| Feature | SvelteKit (`refactor`) | Elm (current) | Status |
| :--- | :--- | :--- | :--- |
| **Routing** | File-based (`routes/`) | `Url.Parser` in `Main.elm` | ✅ Implemented |
| **Home Page** | `routes/+page.svelte` | `Main.elm` (`Home` route) | ✅ Implemented |
| **Map View** | `routes/map/+page.svelte` | `Main.elm` (`MapRoute`) | ✅ Implemented |
| **Event Detail** | `routes/events/[id]/+page.svelte` | `Main.elm` (`EventDetail`) | ✅ Implemented |
| **Edit Event** | `routes/events/[id]/edit/+page.svelte` | `Main.elm` (`EditEvent`) | ✅ Implemented |
| **Create Event** | Likely via `EventForm` | `Main.elm` (Handle `CreateEvent` msg) | ✅ Implemented (Logic present) |
| **Auth** | PocketBase (JS SDK) | `PocketBase.elm` + Ports | ✅ Implemented |
| **KML Import** | `KMLImport.svelte` | `KMLUtils.elm` | ⚠️ Logic exists, UI integration unclear in `Main.elm` |
| **I18n** | `svelte-i18n` | `I18n.elm` | ✅ Implemented |
| **UI Components** | Svelte Components | Elm Modules (`Button`, `Input`, etc.) | ✅ Mostly Ported |
| **Styling** | Tailwind + Flowbite | Tailwind + `app.css` | ✅ configured |

### Missing / Incomplete

1.  **KML Import UI:** `KMLUtils.elm` exists, but there is no obvious "Import KML" button or view logic in `Main.elm`'s `view` function or `Calendar`/`Map` sub-views (needs deep dive into those modules to confirm, but top-level `Main` doesn't show it).
2.  **Error Handling:** `ErrorBoundary.svelte` exists in SvelteKit. Elm uses `Maybe`/`Result`. Global error boundary equivalent (for uncaught runtime errors) is less common in Elm (usually compiler prevents them), but API error handling is present in `Main.elm`.
3.  **Tests:**
    *   Elm has `tests/DateUtilsTests.elm`, `EventUtilsTests.elm`, `KMLUtilsTests.elm`.
    *   Missing counterparts for `Button.test.ts`, `Input.test.ts`, `Map.test.ts` (Unit tests for UI logic).
    *   E2E: `test-bdd.js` and `playwright` config exist.

### Build & Dev Environment

*   **Devenv:** `devenv.nix` exists.
*   **Flake:** `flake.nix` does not exist (relies on implicit `devenv` flake generation or global install).
*   **Makefile:** Exists, mixes `pnpm` and `elm` commands.
*   **Scripts:** `scripts/` contains JS/TS scripts. `generate-statics.js`, `generate-screenshots.ts`.

## Recommendations

1.  **KML Import:** Verify and expose KML import functionality in the UI.
2.  **Tests:** Ensure existing E2E tests cover the critical paths (Create/Edit Event, Map, Auth) to compensate for fewer unit tests on UI components.
3.  **Build:** Consolidate `devenv.nix` to handle Elm tooling properly. Create `flake.nix` for reproducibility.
