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
- [User Stories](./agents/stories/) - User story documentation
- [Architectural Decisions](./agents/adr/) - ADR records for major technical decisions

## Key Commands

- `pnpm install` - Install dependencies
- `pnpm dev` - Start development server
- `pnpm build` - Build for production
- `pnpm lint` - Lint and format code
- `pnpm check` - Type checking
- `pnpm test` - Run unit tests
- `pnpm test:coverage` - Run tests with coverage report
- `pnpm test:bdd` - Run BDD tests
- `pnpm test:e2e` - Run E2E tests
- `pnpm generate-statics` - Generate static feeds

## Code Style

- **Indentation**: Tabs
- **Quotes**: Single quotes
- **Print Width**: 100 characters
- Formatting automated with Prettier

## Testing Practices

Enforce Behavior-Driven Development (BDD) with Test-Driven Development (TDD) approach:

- Write tests before implementing features
- Maintain 70% minimum test coverage
- Run tests before commits: `pnpm test`
- Each test should be connected to a related user story in `./agents/stories/`
- If a user story does not exist for a test, create one
- Component tests are currently unavailable due to Svelte 5 compatibility issues
- Focus on unit tests for utilities and integration tests for data flow
- Use Playwright for critical E2E workflows

See [ADR 0011](./agents/adr/0011-testing-strategy.md) for detailed testing strategy.

## Important Notes

- TODO.md tracks active tasks (local, uncommitted)
- Follow ADR process for significant technical decisions
- Test authentication flows thoroughly
- Validate static generation after data changes
- Do not commit and push until all QA tasks pass
