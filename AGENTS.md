# AGENTS.md

This file provides instructions for AI coding agents on how to work with this project.

## Project Overview

This is a SvelteKit application for an event calendar. It uses TypeScript, Prettier, and ESLint for code quality, and Tailwind CSS for styling. The backend is powered by PocketBase.

## Build and Test Commands

This project uses `pnpm` as the package manager.

- **Install dependencies:** `pnpm install`
- **Run development server:** `pnpm dev`
- **Build for production:** `pnpm build`
- **Lint and format:** `pnpm lint`
- **Check for type errors:** `pnpm check`

## Code Style

- **Tabs:** Use tabs for indentation.
- **Quotes:** Use single quotes.
- **Trailing Comma:** No trailing commas.
- **Print Width:** 100 characters.
- **Formatting:** Use `pnpm format` to format the code.

## Security Considerations

- The application uses PocketBase for the backend, which is hosted at `https://data.suomenpalikkayhteiso.fi`. Ensure that the PocketBase security rules are properly configured to prevent unauthorized access to data.
- Authentication is handled via OAuth2 with an OIDC provider. The authentication logic is in `src/lib/auth.ts`.
- When making changes to the authentication or data access logic, be sure to test thoroughly to prevent security vulnerabilities.