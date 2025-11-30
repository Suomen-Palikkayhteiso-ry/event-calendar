# ADR 0010: Code Quality Tools - ESLint and Prettier

## Status

Accepted

## Date

2025-11-30

## Context

The project requires code quality tools for:
- Consistent code formatting
- Linting for potential issues
- TypeScript checking
- Automated quality enforcement

Requirements:
- Svelte support
- TypeScript integration
- Pre-commit hooks capability
- Good developer experience

## Decision

We chose ESLint for linting and Prettier for code formatting.

## Consequences

### Positive
- Industry-standard tools
- Excellent Svelte and TypeScript support
- Automated code quality
- Consistent formatting
- IDE integration
- Pre-commit hook integration

### Negative
- Configuration complexity
- Occasional false positives
- Learning curve for rules

## Alternatives Considered

### TSLint (deprecated)
- Considered for TypeScript
- Rejected due to deprecation

### Standard JS
- Considered for zero config
- Rejected due to less customization

### Custom linting
- Considered for specific needs
- Rejected due to maintenance overhead

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2)

## Notes

ESLint with TypeScript and Svelte plugins, combined with Prettier, provides comprehensive code quality assurance.