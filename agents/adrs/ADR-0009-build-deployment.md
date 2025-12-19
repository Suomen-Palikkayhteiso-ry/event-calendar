# ADR 0009: Build and Deployment - Nix Flake + devenv for SvelteKit + Elm

## Status

Accepted (Updated for hybrid SvelteKit/Elm migration)

## Date

2025-11-30 (Updated: 2025-12-19)

## Context

The application requires a build system for:

- Fast development server
- Optimized production builds
- Static site generation
- Asset optimization
- Elm compilation and tooling

With the migration to Elm (ADR-0012), we maintain SvelteKit v2 as the build/hosting framework while Elm handles the application logic. We need a build system that supports this hybrid development without abandoning the existing SvelteKit infrastructure.

## Decision

We adopt **Nix Flake + devenv** as the build and deployment system for hybrid SvelteKit + Elm development.

**Nix Flake** provides:
- Reproducible build environments across machines
- Declarative dependency management
- Integration with both SvelteKit and Elm build processes

**devenv** provides:
- Fast, cached development environments
- Dual toolchain management (Node.js/pnpm for SvelteKit, Elm for application)
- Process orchestration for parallel development servers (Vite + elm-watch)
- Git hooks and automation

## Consequences

### Positive

- **Reproducibility**: Identical environments across all development and CI machines
- **Performance**: Cached environments activate in under 100ms
- **Tooling**: Complete dual toolchain (Node.js + Elm) with parallel development
- **Automation**: Scripts, tasks, and git hooks for development workflow
- **Incremental Migration**: Supports running legacy SvelteKit alongside new Elm app

### Negative

- **Learning Curve**: Nix and Flakes have a learning curve
- **Setup Complexity**: Initial flake configuration requires Nix knowledge
- **Platform Support**: Primarily Linux/macOS (Windows support via WSL)
- **Complexity**: Managing two build systems (Vite for JS/Svelte, elm-make for Elm)

## Alternatives Considered

### Pure Elm with elm-pages

- Considered for full Elm experience
- Rejected to allow incremental migration from existing SvelteKit codebase

### Vite + elm-watch only (Legacy)

- Current SvelteKit setup
- Rejected because Nix Flake + devenv provides better environment management and reproducibility

### Pure Nix Flake without devenv

- Considered for minimalism
- Rejected because devenv provides better developer experience and tooling orchestration

## Implementation

**flake.nix** defines:
```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
  };

  outputs = { self, nixpkgs, devenv, ... } @ inputs:
    {
      devShells.default = devenv.lib.mkShell {
        inherit inputs;
        modules = [
          ({ config, pkgs, ... }: {
            languages.javascript.enable = true;
            languages.elm.enable = true;
            packages = [ /* additional packages */ ];
            scripts.build.exec = "pnpm build";
            scripts.dev.exec = "pnpm dev";
          })
        ];
      };
    };
}
```

**devenv.nix** configures the development environment:
```nix
{ pkgs, ... }: {
  languages.javascript.enable = true;
  languages.elm.enable = true;
  packages = [ /* additional packages */ ];
  scripts = {
    "elm-build" = "elm make src/Main.elm --output=elm.js";
    "elm-check" = "elm-format --validate src/ && elm-review";
    build = "pnpm build";
    dev = "pnpm dev";
    check = "pnpm lint && elm-format --validate src/ && elm-review";
  };
  git-hooks.hooks = {
    elm-format.enable = true;
    elm-review.enable = true;
    eslint.enable = true;
  };
}
```

The `devenv` environment orchestrates the processes: it runs `vite dev` and `elm-watch` in parallel for hot reloading of both SvelteKit and Elm code.

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2 & Elm)
- ADR 0012: Migrate Frontend to Elm
