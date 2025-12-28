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
{ pkgs, ... }: 

let
  # Playwright browser compatibility shim for multiple Playwright versions
  # Nix-provided browsers may have different revision numbers than expected
  # by various Playwright versions used in the project
  playwrightBrowsersCompat = pkgs.runCommand "playwright-browsers-compat" {} ''
    mkdir -p $out
    for dir in ${pkgs.playwright-driver.browsers}/*; do
      base="$(basename "$dir")"
      ln -s "$dir" "$out/$base"
    done

    # Create symlinks for missing browser revisions
    # Vitest 3.2.4 + Playwright 1.56 expect revision 1194
    ln -s ${pkgs.playwright-driver.browsers}/chromium-1181 $out/chromium-1194
    ln -s ${pkgs.playwright-driver.browsers}/chromium_headless_shell-1181 $out/chromium_headless_shell-1194
    
    # Playwright 1.57.0 expects firefox-1497
    ln -s ${pkgs.playwright-driver.browsers}/firefox-1489 $out/firefox-1497
    
    # elm-spec-runner (Playwright 1.11.1) expects chromium_headless_shell-1200
    ln -s ${pkgs.playwright-driver.browsers}/chromium_headless_shell-1181 $out/chromium_headless_shell-1200
  '';
in
{
  languages.javascript.enable = true;
  languages.elm.enable = true;
  
  packages = with pkgs; [
    playwright-driver
    playwright-driver.browsers
    playwrightBrowsersCompat
    /* additional packages */
  ];
  
  # Point Playwright to Nix-provided browsers with compatibility layer
  env.PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
  env.PLAYWRIGHT_BROWSERS_PATH = "${playwrightBrowsersCompat}";
  
  scripts = {
    "elm-build" = "elm make src/Main.elm --output=elm.js";
    "elm-check" = "elm-format --validate src/ && elm-review";
    "elm-spec" = "pnpm test:spec";
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

### Playwright Browser Compatibility Layer

The `playwrightBrowsersCompat` derivation creates a compatibility layer for multiple Playwright versions:

- **Problem**: Different tools use different Playwright versions with different browser revision expectations
  - Vitest 3.2.4 uses Playwright 1.56 (expects chromium-1194, firefox-1497)
  - Playwright E2E tests use Playwright 1.57.0 (expects firefox-1497)
  - elm-spec-runner uses Playwright 1.11.1 (expects chromium_headless_shell-1200)
  - Nix nixpkgs provides chromium-1181, firefox-1489 (stable versions)

- **Solution**: Create symlinks from expected revision numbers to available Nix-provided browsers
  - Allows all tools to work with single set of Nix-provided browsers
  - Avoids downloading duplicate browsers for each tool
  - Ensures reproducible browser versions across environments

- **Maintenance**: When Playwright versions are updated, add corresponding symlinks
  - Monitor test failures related to "browser not found"
  - Add new symlinks following the existing pattern
  - Remove obsolete symlinks when tools are updated

The `devenv` environment orchestrates the processes: it runs `vite dev` and `elm-watch` in parallel for hot reloading of both SvelteKit and Elm code.

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2 & Elm)
- ADR 0012: Migrate Frontend to Elm
