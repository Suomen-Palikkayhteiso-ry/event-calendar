# elm-spec with Playwright Setup

This project has been configured to use elm-spec with Playwright (and jsdom) for browser-based testing of Elm applications.

## Overview

- **elm-spec**: Behavior-driven testing framework for Elm (v3.2.0)
- **elm-spec-runner**: Test runner that integrates with Playwright and jsdom (v2.5.1)
- **Playwright**: Browser automation for running specs in real browsers (v1.57.0)
- **jsdom**: Lightweight DOM implementation for faster headless testing

## Quick Start

The setup is complete and ready to use:

```bash
# Install dependencies (if not already done)
pnpm install

# Run specs with jsdom (fastest, default)
pnpm test:spec

# Run specs with Chromium browser
pnpm test:spec:chromium

# Run specs with visible browser window (for debugging)
pnpm test:spec:headed

# Run specs with Firefox
pnpm test:spec:firefox

# Run specs with WebKit
pnpm test:spec:webkit
```

Or using Make:

```bash
make elm-spec          # jsdom (default)
make elm-spec-headed   # Chromium with visible window
```

Or via devenv scripts:

```bash
devenv script elm-spec          # jsdom
devenv script elm-spec-headed   # Chromium with visible window
```

## Directory Structure

```
event-calendar/
├── elm-spec-core/          # Separate elm.json for elm-spec runner
│   └── elm.json            # Includes elm-spec as direct dependency
├── tests/
│   ├── *Spec.elm           # Your spec files go here
│   ├── *Tests.elm          # elm-test unit tests (separate)
│   └── features/           # BDD/Playwright E2E tests (separate)
└── src/                    # Your application code
```

## Writing Specs

Specs are Elm files that end with `Spec.elm` in the `tests/` directory.

### Important: Refer to elm-spec Documentation

The elm-spec API is extensive and this guide provides only basic guidance. For detailed documentation on writing specs, please refer to:

- **[elm-spec GitHub](https://github.com/brian-watkins/elm-spec)** - Official repository with examples
- **[elm-spec package docs](https://package.elm-lang.org/packages/brian-watkins/elm-spec/latest/)** - API documentation

### Basic Spec Structure

```elm
module MyFeatureSpec exposing (..)

import Spec exposing (..)
import Spec.Setup as Setup
import Spec.Observer as Observer
import Spec.Claim as Claim


type alias Model =
    { message : String }


type Msg
    = NoOp


spec : Spec Model Msg
spec =
    describe "My Feature"
        [ scenario "some behavior"
            (given
                (Setup.initWithModel { message = "Hello" }
                    |> Setup.withView view
                )
                |> it "has the expected state"
                    (Observer.observeModel
                        |> Observer.focus .message
                        |> expect (Claim.isEqual Debug.toString "Hello")
                    )
            )
        ]
```

### Key elm-spec Modules

- **Spec**: Core functions (`describe`, `scenario`, `given`, `when`, `it`, `expect`)
- **Spec.Setup**: Setting up Elm programs (model, update, view, subscriptions)
- **Spec.Observer**: Observing program state (model, markup/HTML elements)
- **Spec.Claim**: Making assertions about observations
- **Spec.Markup**: Querying and interacting with HTML
- **Spec.Markup.Selector**: CSS-like selectors for finding elements
- **Spec.Markup.Event**: Simulating user events (clicks, input, etc.)
- **Spec.Http**: Stubbing HTTP requests/responses

### Example: Testing with User Interactions

```elm
module CounterSpec exposing (..)

import Spec exposing (..)
import Spec.Setup as Setup
import Spec.Markup as Markup
import Spec.Markup.Selector exposing (..)
import Spec.Markup.Event as Event
import Spec.Observer as Observer
import Spec.Claim exposing (isEqual)


spec : Spec Model Msg
spec =
    describe "Counter"
        [ scenario "incrementing"
            (given
                (Setup.init (\_ -> init)
                    |> Setup.withUpdate update
                    |> Setup.withView view
                )
                |> when "user clicks increment button"
                    [ Markup.target << by [ id "increment" ]
                    , Event.click
                    ]
                |> it "increases the count"
                    (Observer.observeModel
                        |> Observer.focus .count
                        |> expect (isEqual Debug.toString 1)
                    )
            )
        ]
```

**Note**: The exact API may vary depending on the elm-spec version. Always refer to the official documentation for accurate usage.

## Configuration

### Package Scripts

The following npm scripts are configured in `package.json`:

- `test:spec` - Run with jsdom (fastest, no real browser)
- `test:spec:chromium` - Run with Chromium browser
- `test:spec:headed` - Run with Chromium, visible window (debugging)
- `test:spec:firefox` - Run with Firefox browser
- `test:spec:webkit` - Run with WebKit browser

### elm-spec-runner CLI

The specs are run using the elm-spec-runner CLI with these options:

```bash
npx elm-spec \
  --specRoot=./elm-spec-core \      # Directory with elm.json for specs
  --specs='../tests/**/*Spec.elm' \ # Pattern for finding spec files
  --browser=jsdom                   # Browser environment (jsdom/chromium/firefox/webkit)
```

Add `--visible` flag to see the browser window during test execution.

### elm-spec-core/elm.json

This is a separate `elm.json` that includes `elm-spec` as a direct dependency. The runner compiles specs using this configuration, which references your source directories (`../src` and `../tests`).

This separation is necessary because:

1. elm-spec needs to be a direct dependency for the runner
2. It keeps your main application's dependencies separate from test dependencies
3. The runner needs access to both your source code and test files

## Integration with Existing Tests

This setup works alongside:

- **elm-test**: Unit tests for pure Elm functions (`tests/*Tests.elm`) - Run with `pnpm elm-test`
- **Playwright E2E**: End-to-end tests (`tests/*.spec.ts`) - Run with `pnpm test:e2e`
- **Playwright BDD**: Cucumber-style tests (`tests/features/*.feature`) - Run with `pnpm test:bdd`

Each test type serves a different purpose:

- **elm-test**: Fast unit tests for logic and pure functions
- **elm-spec**: Component-level behavior testing with simulated or real browser
- **Playwright E2E/BDD**: Full application integration testing

## Browser Environments

### jsdom (Default, Fastest)

- Lightweight JavaScript implementation of web standards
- No real browser required
- Fast execution
- Good for most component testing
- Limitations: No actual rendering, CSS, or complex browser APIs

```bash
pnpm test:spec  # Uses jsdom by default
```

### Real Browsers (Chromium, Firefox, WebKit)

- Full browser environment with rendering
- Slower than jsdom but more realistic
- Required for testing visual behavior, CSS, animations
- Better for debugging with `--visible` flag

```bash
pnpm test:spec:chromium       # Headless Chromium
pnpm test:spec:headed         # Chromium with visible window
pnpm test:spec:firefox        # Headless Firefox
pnpm test:spec:webkit         # Headless WebKit
```

### Nix/devenv Integration

The devenv.nix configuration provides Playwright browsers via Nix packages, with compatibility shims for version mismatches between nixpkgs and the Playwright versions used by elm-spec-runner and other tools.

If you encounter browser compatibility issues:

1. Prefer jsdom for routine testing (no browser required)
2. Use real browsers only when needed for visual/CSS testing
3. Restart your devenv shell after updating devenv.nix

## Debugging

### Run with Visible Browser

Use the `--visible` flag to see the browser window (requires Chromium/Firefox/WebKit):

```bash
pnpm test:spec:headed     # Chromium with visible window
# or
make elm-spec-headed
```

### Verbose Output

The elm-spec-runner provides detailed output by default, showing:

- Compilation progress
- Test scenarios and results
- Detailed error messages for failures

### Common Issues

1. **"No spec modules found!"**
   - Ensure your spec files end with `Spec.elm`
   - Check they're in the `tests/` directory
   - Verify the glob pattern in package.json: `'../tests/**/*Spec.elm'`

2. **Compilation errors**
   - Refer to [elm-spec documentation](https://github.com/brian-watkins/elm-spec) for correct API usage
   - Ensure `elm-spec-core/elm.json` includes all dependencies needed by your specs
   - Check that you're using elm-spec 3.2.0 compatible APIs

3. **Browser not found (Chromium/Firefox/WebKit only)**
   - These errors only occur when using real browsers (not jsdom)
   - If using devenv: Restart your shell to pick up browser compatibility changes
   - If not using devenv: Run `pnpm exec playwright install`
   - Fallback: Use jsdom with `pnpm test:spec` (no browser required)

## CI/CD Integration

The specs will run in jsdom mode by default (no browser download required), which is suitable for CI environments:

```bash
pnpm test:spec  # Fast, no browser needed
```

For full browser testing in CI, ensure Playwright browsers are available. The GitHub Actions workflow should install them automatically.

## Troubleshooting

### Browser Compatibility with Nix

elm-spec-runner (v2.5.1) uses Playwright 1.11.1 internally, which may have version conflicts with Nix-provided browsers. The devenv.nix includes compatibility shims, but if issues persist:

**Solution**: Use jsdom for routine testing:

```bash
pnpm test:spec  # No browser required, fastest
```

Only use real browsers when you specifically need to test visual rendering or browser-specific behavior.

### Timeout Errors

If specs take longer than expected, you can adjust the timeout using the CLI:

```bash
npx elm-spec --specRoot=./elm-spec-core --specs='../tests/**/*Spec.elm' --timeout=60000
```

Or update the script in `package.json` to include `--timeout=<milliseconds>`.

### Compilation Errors in Specs

1. Check that you're using the correct elm-spec 3.2.0 API
2. Ensure all modules are properly imported
3. Verify that `elm-spec-core/elm.json` includes dependencies needed by your specs (both application and test dependencies)
4. Refer to the [elm-spec documentation](https://github.com/brian-watkins/elm-spec) for examples

## Resources

- **[elm-spec GitHub](https://github.com/brian-watkins/elm-spec)** - Official repository with comprehensive examples
- **[elm-spec package docs](https://package.elm-lang.org/packages/brian-watkins/elm-spec/latest/)** - Complete API documentation
- **[Playwright documentation](https://playwright.dev/)** - Browser automation reference
