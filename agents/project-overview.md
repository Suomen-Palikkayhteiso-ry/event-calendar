# Project Overview

## Description

This is a SvelteKit application for an event calendar. It serves as the digital calendar for Suomen Palikkayhteisö ry (Finnish LEGO Community Association), providing an interactive calendar interface for browsing events, as well as generating static feeds for syndication.

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

- **Frontend Framework**: SvelteKit v2 with TypeScript
- **Backend**: PocketBase (self-hosted)
- **Styling**: Tailwind CSS v4
- **Authentication**: OAuth2 with OIDC
- **Maps**: Leaflet with geocoding
- **Calendar Component**: @event-calendar/core
- **Internationalization**: svelte-i18n
- **Build Tool**: Vite
- **Deployment**: Static site generation

## Architecture

The application follows a hybrid architecture:

- **Dynamic App**: SvelteKit handles user interactions, authentication, and event management
- **Static Generation**: Custom Node.js scripts generate feeds and static content from PocketBase data
- **Backend**: PocketBase provides data persistence, authentication, and file storage

## Development Environment

The project uses `devenv` for reproducible development environments and `pnpm` for package management. See [Development Setup](./development-setup.md) for detailed instructions.

## Code Quality

- **Linting**: ESLint with Svelte and TypeScript support
- **Formatting**: Prettier with custom configuration
- **Type Checking**: SvelteKit's built-in TypeScript checking

## Security

Authentication is handled via OAuth2/OIDC integration with PocketBase. All data access is secured through PocketBase's permission system. See [Security Considerations](./security.md) for details.
