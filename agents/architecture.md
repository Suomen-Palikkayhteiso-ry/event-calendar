# Architecture Overview

## High-Level Architecture

The event calendar application follows a hybrid architecture combining dynamic web application features with static site generation for content syndication.

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   SvelteKit     │    │   PocketBase    │    │  Static Files   │
│   Frontend      │◄──►│   Backend       │───►│  (Feeds, ICS)   │
│                 │    │                 │    │                 │
│ • Calendar UI   │    │ • Event Data    │    │ • RSS/Atom      │
│ • Event Mgmt    │    │ • Auth          │    │ • JSON/GeoJSON  │
│ • User Auth     │    │ • File Storage  │    │ • ICS Calendar  │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Component Architecture

### Frontend (SvelteKit)

- **Routes**: File-based routing with SvelteKit
  - `/` - Main calendar view
  - `/events` - Event management (authenticated)
  - `/events/[id]` - Individual event pages

- **Components**:
  - Calendar component (@event-calendar/core)
  - Map component (Leaflet)
  - Form components (Flowbite Svelte)
  - Toast notifications (@zerodevx/svelte-toast)

- **State Management**:
  - Svelte stores for user authentication
  - Reactive statements for UI state
  - Local component state

### Backend (PocketBase)

- **Collections**:
  - `events` - Event data with geolocation
  - `users` - User management

- **Authentication**:
  - OAuth2/OIDC integration
  - Admin authentication for content management

- **File Storage**:
  - Event images
  - Automatic optimization

### Static Generation

Custom Node.js scripts generate multiple output formats:

- **HTML**: Embeddable calendar pages
- **RSS/Atom**: Syndication feeds
- **JSON**: Structured data access
- **GeoJSON**: Geographic event data
- **ICS**: Calendar import files
- **Individual Event Pages**: SEO-friendly event details

## Data Flow

1. **Event Creation**:
   - User authenticates via OAuth2
   - Form data submitted to PocketBase
   - Geocoding performed for location data
   - Event stored with metadata

2. **Calendar Display**:
   - Events fetched from PocketBase API
   - Calendar component renders events
   - Interactive navigation and filtering

3. **Static Generation**:
   - Script fetches all published events
   - Generates multiple output formats
   - Files written to static directory

## Security Architecture

- **Authentication**: OAuth2/OIDC delegation
- **Authorization**: PocketBase collection rules
- **Data Validation**: TypeScript interfaces and PocketBase schemas
- **API Security**: PocketBase's built-in security features

## Performance Considerations

- **Static Generation**: Pre-computed feeds reduce server load
- **Lazy Loading**: Calendar events loaded on demand
- **Asset Optimization**: Vite handles bundling and minification
- **Caching**: Static files served with appropriate headers

## Deployment Architecture

- **Static Hosting**: Generated files served from static host
- **PocketBase**: Self-hosted backend service
- **CI/CD**: Automated build and deployment pipeline
- **Monitoring**: Basic error logging and performance monitoring
