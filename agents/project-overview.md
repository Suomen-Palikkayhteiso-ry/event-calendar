# Project Overview

## Description

This is an event calendar application, currently migrating from SvelteKit to Elm. It serves as the digital calendar for Suomen Palikkayhteisö ry (Finnish LEGO Community Association), providing an interactive calendar interface for browsing events, as well as generating static feeds for syndication.

## Key Features

- **Interactive Calendar**: Monthly calendar view with event navigation
- **Event Management**: CRUD operations for events (authenticated users)
- **Location Mapping**: Integrated Leaflet maps with geocoding
- **Multi-format Feeds**: RSS, Atom, JSON, GeoJSON, ICS calendar exports
- **Static Generation**: Automated generation of static files for feeds and embeds
- **Internationalization**: Finnish language support with extensible i18n
- **Responsive Design**: Mobile-friendly interface
- **Accessibility**: Keyboard navigation and screen reader support

## Technology Stack

- **Frontend Framework**: Elm 0.19.1 (Migrating from SvelteKit)
- **Backend**: PocketBase (self-hosted)
- **Static Generation**: Haskell scripts for feed generation
- **Styling**: Tailwind CSS v4
- **Authentication**: OAuth2 with OIDC
- **Maps**: Leaflet (via Elm Ports)
- **Calendar Component**: Custom Elm implementation
- **Internationalization**: Elm (Custom i18n module)
- **Build Tool**: Vite (via vite-plugin-elm)
- **Deployment**: Static site generation

## Architecture

The application follows a client-side architecture using The Elm Architecture (TEA):

- **Frontend**: Pure Elm application managing state, routing, and UI logic.
- **Interop**: JavaScript Ports handle maps, local storage, and static assets.
- **Static Generation**: Haskell scripts generate feeds and static content from PocketBase data.
- **Backend**: PocketBase provides data persistence, authentication, and file storage.

## Development Environment

The project uses `devenv` for reproducible development environments and `pnpm` for package management. See [Development Setup](./development-setup.md) for detailed instructions.

## Code Quality

- **Linting**: `elm-review`
- **Formatting**: `elm-format`
- **Type Checking**: Elm Compiler (Built-in strict static typing)

## Security

Authentication is handled via OAuth2/OIDC integration with PocketBase. All data access is secured through PocketBase's permission system. See [Security Considerations](./security.md) for details.
