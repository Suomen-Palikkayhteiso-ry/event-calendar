# Architecture Overview

## High-Level Architecture

The event calendar application follows a client-side functional architecture (Elm) served via a static host or hybrid backend, integrated with PocketBase for data and authentication.

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Elm Frontend  │    │   PocketBase    │    │  Static Files   │
│   (Client)      │◄──►│   Backend       │───►│  (Feeds, ICS)   │
│                 │    │                 │    │                 │
│ • Calendar UI   │    │ • Event Data    │    │ • RSS/Atom      │
│ • Event Mgmt    │    │ • Auth          │    │ • JSON/GeoJSON  │
│ • User Auth     │    │ • File Storage  │    │ • ICS Calendar  │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Component Architecture

### Frontend (Elm)

- **Architecture**: The Elm Architecture (Model-View-Update)
- **Routing**: `Url.Parser` based routing in `Main.elm`
  - `/` - Home (Calendar)
  - `/map` - Map View
  - `/events/:id` - Event Detail
  - `/events/:id/edit` - Edit Event

- **Modules (src/)**:
  - `Main.elm`: Application entry point and wiring
  - `Calendar.elm`: Calendar grid logic and view
  - `Map.elm`: Leaflet integration (via Ports/Custom Elements)
  - `EventForm.elm`: Create/Edit event logic
  - `PocketBase.elm`: API interaction module (Ports/Http)
  - `Events.elm`: Event data structures and collection management

### Interop (Ports)

The application uses specific ports to communicate with JavaScript for features not available in pure Elm.

**Authentication (`src/Ports.elm` ↔ `src/index.js`)**
*   `storeAuth : Auth -> Cmd msg`: Sends authentication data (token + user) to JS for localStorage persistence.
*   `removeAuth : () -> Cmd msg`: Signals JS to clear auth data from localStorage.
*   `authStored : (Auth -> msg) -> Sub msg`: Subscription receiving auth data restored from localStorage on app init.
*   `authRemoved : (() -> msg) -> Sub msg`: Subscription confirming auth data removal.

**Leaflet Map (`src/Ports.elm` ↔ `src/index.js`)**
*   `initMap : MapConfig -> Cmd msg`: Initializes the Leaflet map with center, zoom, and optional marker.
*   `updateMap : MapConfig -> Cmd msg`: Updates the map view or marker position programmatically.
*   `mapMarkerMoved : ((Float, Float) -> msg) -> Sub msg`: Subscription receiving new coordinates when the user drags the marker.

### Backend (PocketBase)

- **Collections**:
  - `events` - Event data with geolocation
  - `users` - User management

- **Authentication**:
  - OAuth2/OIDC integration
  - Admin authentication for content management

### Legacy SvelteKit (Migration Phase)

During migration, the SvelteKit scaffolding remains to serve the application or provide reference implementation. `src/routes` contains legacy Svelte components.

### Static Generation

Haskell scripts generate multiple output formats from PocketBase data:

- **GenerateFeeds.hs**: RSS/Atom syndication feeds
- **GenerateIcsGeojson.hs**: ICS calendar files and GeoJSON geographic data
- **GenerateStatics.hs**: HTML static pages for embedding
- **GenerateEmbed.hs**: Embeddable calendar widgets
- **GenerateUtils.hs**: Common utilities for PocketBase API interaction and data processing

The Haskell scripts provide type-safe, efficient generation of static content with proper error handling and timezone support.

## Data Flow (TEA)

1. **Msg (Message)**: User interaction or external event (HTTP, Port) triggers a `Msg`.
2. **Update**: The `update` function takes the `Msg` and current `Model`, returning a new `Model` and `Cmd` (side effects).
3. **View**: The `view` function renders the new `Model` to HTML.
4. **Runtime**: The Elm runtime manages the DOM updates and side effect execution.

## Security Architecture

- **Authentication**: OAuth2/OIDC delegation (Token stored via Ports)
- **Authorization**: PocketBase collection rules
- **Type Safety**: Elm ensures no runtime null/undefined errors in frontend logic

## Deployment Architecture

- **Artifacts**: Compiled `elm.js` (or `main.js`) + `index.html` + Static Assets
- **Hosting**: Static hosting compatible
- **PocketBase**: Self-hosted backend service
