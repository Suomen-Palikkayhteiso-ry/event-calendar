# Playwright Setup on NixOS

This document explains how to set up and maintain Playwright for E2E testing on NixOS using devenv.

## Overview

On NixOS, we cannot use `playwright install` to download browsers because:
1. Downloaded binaries won't work due to missing dynamic libraries
2. NixOS requires all binaries to be managed through Nix

Instead, we use `playwright-driver` from nixpkgs, which provides pre-packaged browsers.

## Architecture

The setup involves three components:

1. **nixpkgs `playwright-driver`**: Provides Playwright and browsers at a specific version
2. **package.json Playwright version**: Must match the nixpkgs version
3. **Browser revision symlinks**: Map version-specific browser revisions when needed

## How It Works

```
┌─────────────────────────────────────────────────────────────────┐
│                        devenv.nix                                │
├─────────────────────────────────────────────────────────────────┤
│  playwright-driver (version X.Y.Z)                               │
│  playwright-driver.browsers (chromium-AAAA, firefox-BBBB, etc.)  │
│                           │                                      │
│                           ▼                                      │
│  playwrightBrowsersCompat (symlinks for version compatibility)   │
│                           │                                      │
│                           ▼                                      │
│  PLAYWRIGHT_BROWSERS_PATH → points to compat directory           │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                       package.json                               │
├─────────────────────────────────────────────────────────────────┤
│  "@playwright/test": "X.Y.Z"  ← Must match nixpkgs version       │
│  "playwright": "X.Y.Z"        ← Must match nixpkgs version       │
└─────────────────────────────────────────────────────────────────┘
```

## Verifying Version Compatibility

### Step 1: Check nixpkgs Playwright Version

```bash
# Get the playwright-driver version from nixpkgs
devenv shell -- nix-instantiate --eval -E '(import <nixpkgs> {}).playwright-driver.version'
```

This returns the version available in your nixpkgs channel (e.g., `"1.50.1"`).

### Step 2: Check Available Browser Revisions

```bash
# List browsers provided by nixpkgs
devenv shell -- ls -la $PLAYWRIGHT_BROWSERS_PATH
```

Example output:
```
chromium-1181
chromium_headless_shell-1181
firefox-1489
webkit-2191
```

### Step 3: Check package.json Playwright Version

```bash
# Check current package.json versions
grep -E '"(@playwright/test|playwright)"' package.json
```

### Step 4: Check Expected Browser Revisions

Run a test to see what revisions Playwright expects:

```bash
devenv shell -- pnpm test:e2e --project=chromium 2>&1 | head -20
```

If you see errors like:
```
Executable doesn't exist at .../chromium_headless_shell-1155/...
```

This means Playwright expects revision `1155` but nixpkgs has `1181`.

## Fixing Version Mismatches

### Option A: Match package.json to nixpkgs (Recommended)

Update package.json to use the exact version from nixpkgs:

```json
{
  "devDependencies": {
    "@playwright/test": "1.50.1",
    "playwright": "1.50.1"
  }
}
```

**Important**: Use exact versions without `^` to prevent automatic updates.

Then reinstall:
```bash
devenv shell -- pnpm install
```

### Option B: Add Browser Revision Symlinks

If the versions still need browser revision mapping, update `devenv.nix`:

```nix
let
  playwrightBrowsersCompat = pkgs.runCommand "playwright-browsers-compat" {} ''
    mkdir -p $out

    # Link all existing browsers
    for dir in ${pkgs.playwright-driver.browsers}/*; do
      base="$(basename "$dir")"
      ln -s "$dir" "$out/$base"
    done

    # Add symlinks for expected → available revision mappings
    # Example: Playwright expects 1155, nixpkgs has 1181
    ln -sf ${pkgs.playwright-driver.browsers}/chromium_headless_shell-1181 $out/chromium_headless_shell-1155
    ln -sf ${pkgs.playwright-driver.browsers}/firefox-1489 $out/firefox-1471
    ln -sf ${pkgs.playwright-driver.browsers}/webkit-2191 $out/webkit_ubuntu20.04_x64_special-2092
  '';
in
{
  # ... rest of config
  env.PLAYWRIGHT_BROWSERS_PATH = "${playwrightBrowsersCompat}";
}
```

## Automated Verification Script

Run the verification script to check compatibility:

```bash
./scripts/check-playwright-compat.sh
```

This script will:
1. Check the nixpkgs playwright-driver version
2. Check package.json versions
3. List available browser revisions
4. Report any mismatches

## Troubleshooting

### Error: "Executable doesn't exist at ..."

This means Playwright expects a browser revision that doesn't exist in your browsers path.

**Solution**: Either:
1. Pin package.json Playwright version to match nixpkgs
2. Add a symlink in devenv.nix mapping expected → available revision

### Error: "browserType.launch: Browser was not installed"

The `PLAYWRIGHT_BROWSERS_PATH` environment variable might not be set correctly.

**Solution**: Verify the path is set in devenv shell:
```bash
devenv shell -- echo $PLAYWRIGHT_BROWSERS_PATH
devenv shell -- ls $PLAYWRIGHT_BROWSERS_PATH
```

### Tests work locally but fail in CI

Ensure CI also uses devenv or has matching Playwright/browser versions.

## Updating Playwright

When updating Playwright:

1. Check nixpkgs for available versions:
   ```bash
   # Current channel
   devenv shell -- nix-instantiate --eval -E '(import <nixpkgs> {}).playwright-driver.version'
   ```

2. Update package.json to match:
   ```bash
   # Edit package.json with the version from step 1
   ```

3. Update devenv.nix symlinks if needed (run tests to discover required revisions)

4. Test all browsers:
   ```bash
   devenv shell -- pnpm test:e2e
   ```

## Current Configuration

As of this writing:
- **nixpkgs playwright-driver**: 1.50.1
- **package.json versions**: 1.50.1
- **Browser revisions available**: chromium-1181, firefox-1489, webkit-2191
- **Symlinks needed**: Yes (mapping older expected revisions to available ones)
