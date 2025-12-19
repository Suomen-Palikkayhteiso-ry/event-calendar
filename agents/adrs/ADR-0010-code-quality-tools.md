# ADR 0010: Code Quality Tools - Dual Stack (Elm + JS/TS)

## Status

Accepted (Updated for hybrid SvelteKit/Elm migration)

## Date

2025-12-19

## Context

The project requires code quality tools for a hybrid SvelteKit + Elm environment.
We need:
- Formatting and Linting for JavaScript/TypeScript/Svelte (Legacy/Shell)
- Formatting and Linting for Elm (Application Logic)
- Integration with nix flake/devenv build system

## Decision

We chose a dual-stack approach:
1. **ESLint** and **Prettier** for JS/TS/Svelte
2. **elm-format** and **elm-review** for Elm

All tools integrated into devenv scripts and git hooks.

## Consequences

### Positive

- **Industry Standards**: Using the best-in-class tools for each ecosystem
- **Automation**: All tools are integrated into devenv scripts (elm-check, lint)
- **Consistency**: Both languages enforce strict formatting

### Negative

- **Tooling Overhead**: Developers need extensions/setup for both ecosystems
- **CI Complexity**: Multiple check commands to run

## Alternatives Considered

### Single formatting tool (e.g. Prettier for Elm)

- Considered using Prettier plugin for Elm
- Rejected in favor of elm-format which is the community standard for Elm

### ESLint only

- Considered for unified linting
- Rejected because elm-review provides better Elm-specific analysis

## Implementation

**devenv.nix configuration:**
```nix
{ pkgs, ... }: {
  languages.javascript.enable = true;
  languages.elm.enable = true;
  git-hooks.hooks = {
    elm-format.enable = true;
    elm-review.enable = true;
    eslint.enable = true;
  };
  scripts = {
    "elm-check" = "elm-format --validate src/ && elm-review";
    lint = "eslint . --ext .js,.ts,.svelte";
    format = "prettier --write . && elm-format --yes src/";
  };
}
```

**ESLint configuration (eslint.config.js):**
```javascript
export default [
  {
    files: ['**/*.js', '**/*.ts', '**/*.svelte'],
    rules: {
      'no-unused-vars': 'error',
      'prefer-const': 'error'
    }
  }
];
```

**elm-review configuration:**
```json
{
  "package": "jfmengels/elm-review",
  "source-directories": ["src"],
  "elmjson": "elm.json",
  "config": {
    "rules": {
      "NoUnused.Variables": "error",
      "NoUnused.Modules": "error"
    }
  }
}
```

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2 & Elm)
- ADR 0009: Build and Deployment (Nix Flake + devenv)
