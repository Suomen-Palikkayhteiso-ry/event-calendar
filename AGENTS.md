# AGENTS.md

This file provides instructions for AI coding agents on how to work with this project.

## Quick Reference

- **Project**: SvelteKit event calendar application
- **Backend**: PocketBase
- **Styling**: Tailwind CSS v4
- **Package Manager**: pnpm
- **Environment**: devenv

## Getting Started

See [Development Setup](./agents/development-setup.md) for environment setup and commands.

## Project Documentation

- [Project Overview](./agents/project-overview.md) - Detailed project description and features
- [Architecture](./agents/architecture.md) - High-level system architecture
- [Security](./agents/security.md) - Security considerations and best practices
- [Architectural Decisions](./agents/adr/) - ADR records for major technical decisions

## Key Commands

All commands should be prefixed with `devenv shell --`:

- `pnpm install` - Install dependencies
- `pnpm dev` - Start development server
- `pnpm build` - Build for production
- `pnpm lint` - Lint and format code
- `pnpm check` - Type checking
- `pnpm generate-statics` - Generate static feeds

## Code Style

- **Indentation**: Tabs
- **Quotes**: Single quotes
- **Print Width**: 100 characters
- Formatting automated with Prettier

## Important Notes

- TODO.md tracks active tasks (local, uncommitted)
- Follow ADR process for significant technical decisions
- Test authentication flows thoroughly
- Validate static generation after data changes
