# ADR 0013: Dependency Cleanup - Remove pnpm and TypeScript

## Status

Proposed

## Date

2025-01-XX

## Context

With the migration to elm-pages (ADR-0001), the current build system relies on Vite, pnpm, TypeScript, and various JavaScript tooling that will no longer be needed. The pure Elm stack with elm-pages uses Nix + devenv for build management, eliminating the need for Node.js package management and TypeScript compilation.

## Decision

We will remove all pnpm, TypeScript, and related JavaScript tooling dependencies as part of the elm-pages migration:

### Remove Files
- `package.json` and `pnpm-lock.yaml`
- All `*.ts` and `*.js` configuration files (except `src/index.js` for ports)
- TypeScript test files and configurations
- Vite, Playwright, and testing configurations

### Keep Files (Temporarily)
- `src/index.js` - Required for Elm ports (Leaflet integration)
- Build output in `dist/` - May be needed during transition

### Update Build System
- Rely entirely on Nix flake + devenv for dependencies
- Use `make` commands for Elm-specific operations
- Remove all npm/pnpm script references

## Consequences

### Positive

- **Simplified Build**: Single toolchain (Nix) instead of multiple package managers
- **Reduced Complexity**: Eliminates Node.js ecosystem maintenance
- **Faster Builds**: No JavaScript bundling or TypeScript compilation
- **Smaller Footprint**: No node_modules directory or lock files
- **Pure Elm Focus**: Aligns with elm-pages philosophy

### Negative

- **Port Management**: `src/index.js` becomes the only JavaScript file, needs careful maintenance
- **Tooling Gap**: Loss of JavaScript debugging and development tools
- **Migration Effort**: All build scripts and CI/CD need updates

## Current Dependencies to Remove

### DevDependencies (package.json)
```
@cucumber/cucumber, @eslint/compat, @eslint/js, @fontsource/fira-mono,
@playwright/test, @tailwindcss/forms, @tailwindcss/typography,
@tailwindcss/vite, @testing-library/jest-dom, @testing-library/user-event,
@types/leaflet, @types/node, @vitest/coverage-v8, @vitest/ui,
elm-test, eslint, eslint-config-prettier, globals, happy-dom, jsdom,
playwright, playwright-bdd, prettier, prettier-plugin-tailwindcss,
tailwindcss, tsx, typescript, typescript-eslint, vite, vite-plugin-elm-watch, vitest
```

### Dependencies (package.json)
```
feed, flowbite, ical-generator, leaflet, pocketbase, qrcode
```

### Files to Remove
- `playwright-bdd.config.ts`
- `vite.config.ts`
- `vitest.config.ts`
- `playwright.config.ts`
- `eslint.config.js`
- `tailwind.config.js`
- `scripts/generate-screenshots.ts`
- `scripts/generate-statics.js`
- `scripts/generate-utils.js`
- `tests/screenshots.spec.ts`
- `tests/steps/steps.ts`
- `test-bdd.js`

### Files to Keep (Modified)
- `src/index.js` - Elm ports for Leaflet
- `Makefile` - Update to remove npm/pnpm references

## Implementation Plan

### Phase 1: Backup and Analysis
1. Document all current npm scripts and their purposes
2. Identify any critical functionality that needs Elm replacement
3. Backup all configuration files before removal

### Phase 2: Core Removal
1. Delete package.json, pnpm-lock.yaml
2. Remove all TypeScript and JavaScript tooling files
3. Update Makefile to use only Nix/devenv commands
4. Update CI/CD configuration

### Phase 3: Elm-Only Replacements
1. Replace `scripts/generate-statics.js` with Elm BackendTask
2. Replace `scripts/generate-screenshots.ts` with elm-program-test
3. Move static generation logic to elm-pages BackendTask

### Phase 4: Validation
1. Confirm all Elm functionality works with Nix build
2. Test deployment with elm-pages
3. Validate that no critical features were lost

## Migration Dependencies

This ADR depends on:
- ADR-0012: Migration Plan (overall coordination)
- ADR-0009: Build System (Nix flake + devenv)
- ADR-0011: Testing Strategy (elm-program-test replacement)

## Success Criteria

- No JavaScript/TypeScript files except `src/index.js`
- All builds work through Nix + elm-pages
- CI/CD uses only Elm-native tooling
- Application functionality preserved
- Build time improved

## Rollback Plan

If issues arise:
1. Restore package.json from git history
2. Reinstall dependencies with pnpm
3. Gradually migrate back individual components
4. Use hybrid approach during transition

## Related Decisions

- ADR-0001: Framework Choice (elm-pages)
- ADR-0009: Build System (Nix Flake + devenv)
- ADR-0011: Testing Strategy (elm-program-test)
- ADR-0012: Migration Plan

## Notes

The removal of pnpm and TypeScript represents a significant simplification of the build system. The `src/index.js` file will remain as the sole JavaScript dependency for Leaflet integration via ports. All other functionality will be implemented in pure Elm.