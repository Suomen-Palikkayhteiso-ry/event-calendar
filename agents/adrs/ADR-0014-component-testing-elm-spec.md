# ADR 0014: Component Testing with elm-spec and Playwright Integration

## Status

Accepted

## Date

2025-12-20

## Context

The project requires component-level behavior testing for Elm UI components and modules. While ADR-0011 established elm-test for unit tests and elm-program-test for integration tests, there's a gap for testing component behavior in browser-like environments with user interactions.

We need:

- Component-level behavior-driven testing (BDD) for Elm UI components
- Ability to test user interactions (clicks, input, events) on individual components
- Option to test in both lightweight environments (fast) and real browsers (accurate)
- Integration with existing test infrastructure (elm-test, Playwright E2E, BDD tests)
- Compatibility with Nix/devenv development environment
- CI/CD friendly test execution

## Decision

We adopt **elm-spec** with **Playwright integration** as our component testing solution:

### Core Components

1. **elm-spec v3.2.0**: Behavior-driven testing framework for Elm
   - Direct dependency in elm.json test-dependencies
   - Provides BDD-style API for describing component behavior
   - Supports observation of model state and markup/DOM

2. **elm-spec-runner v2.5.1**: Test runner supporting multiple environments
   - Added to package.json devDependencies
   - Supports jsdom (fast, lightweight) and Playwright browsers (accurate)
   - CLI-based execution integrated into npm scripts

3. **Separate elm-spec-core Configuration**:
   - Dedicated `elm-spec-core/elm.json` for spec compilation
   - Source directories: `../src` and `../tests`
   - Includes elm-spec as direct dependency alongside application dependencies

### Test Execution Modes

**Default: jsdom (Fast)**

- Lightweight JavaScript DOM implementation
- No browser required
- Fast execution for routine testing
- Command: `pnpm test:spec` or `make elm-spec`

**Optional: Real Browsers (Accurate)**

- Chromium, Firefox, WebKit via Playwright
- Full browser environment with rendering
- For visual, CSS, and browser-specific testing
- Commands:
  - `pnpm test:spec:chromium` - Headless Chromium
  - `pnpm test:spec:headed` - Visible Chromium (debugging)
  - `pnpm test:spec:firefox` - Headless Firefox
  - `pnpm test:spec:webkit` - Headless WebKit

### Integration Points

**package.json Scripts:**

```json
{
	"test:spec": "npx elm-spec --specRoot=./elm-spec-core --specs='../tests/**/*Spec.elm'",
	"test:spec:chromium": "npx elm-spec --specRoot=./elm-spec-core --specs='../tests/**/*Spec.elm' --browser=chromium",
	"test:spec:headed": "npx elm-spec --specRoot=./elm-spec-core --specs='../tests/**/*Spec.elm' --browser=chromium --visible",
	"test:spec:firefox": "npx elm-spec --specRoot=./elm-spec-core --specs='../tests/**/*Spec.elm' --browser=firefox",
	"test:spec:webkit": "npx elm-spec --specRoot=./elm-spec-core --specs='../tests/**/*Spec.elm' --browser=webkit"
}
```

**devenv.nix Scripts:**

```nix
scripts = {
  elm-spec.exec = "pnpm test:spec";
  elm-spec-headed.exec = "pnpm test:spec:headed";
};
```

**Makefile Targets:**

```makefile
elm-spec: node_modules
	pnpm test:spec

elm-spec-headed: node_modules
	pnpm test:spec:headed
```

**Playwright Browser Compatibility:**

The project uses a `playwrightBrowsersCompat` compatibility layer in devenv.nix to handle multiple Playwright versions:

```nix
let
  # Playwright browser compatibility for multiple versions
  # - Vitest 3.2.4 + Playwright 1.56
  # - Playwright 1.57.0 (E2E tests)
  # - elm-spec-runner (Playwright 1.11.1)
  playwrightBrowsersCompat = pkgs.runCommand "playwright-browsers-compat" {} ''
    mkdir -p $out
    for dir in ${pkgs.playwright-driver.browsers}/*; do
      base="$(basename "$dir")"
      ln -s "$dir" "$out/$base"
    done

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
  # ... rest of devenv config

  packages = with pkgs; [
    playwright-driver
    playwright-driver.browsers
    playwrightBrowsersCompat  # Include compatibility layer
  ];

  # Point Playwright to compatibility layer
  env.PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
  env.PLAYWRIGHT_BROWSERS_PATH = "${playwrightBrowsersCompat}";
}
```

**Why this is needed:**

- elm-spec-runner (v2.5.1) uses Playwright 1.11.1 internally
- E2E tests use Playwright 1.57.0
- Vitest uses Playwright 1.56
- Each expects different browser revision numbers
- Nix nixpkgs provides stable browser versions (chromium-1181, firefox-1489)
- Symlinks allow all tools to use the same Nix-provided browsers
- Avoids downloading duplicate browsers, ensures reproducibility

### File Organization

```
tests/
  ├── *Spec.elm              # elm-spec component behavior tests
  ├── *Tests.elm             # elm-test unit tests
  ├── *.spec.ts              # Playwright E2E tests
  └── features/
      └── *.feature          # Playwright BDD tests

elm-spec-core/
  └── elm.json               # Separate config for spec compilation
```

### Test Type Hierarchy

1. **elm-test** (`*Tests.elm`): Pure logic, data transformations, algorithms
2. **elm-spec** (`*Spec.elm`): Component behavior, user interactions, UI state
3. **Playwright E2E** (`*.spec.ts`): Full application workflows, cross-page flows
4. **Playwright BDD** (`*.feature`): User story acceptance tests

## Consequences

### Positive

- **Component-Level BDD**: Natural language descriptions of component behavior
- **Fast Default Testing**: jsdom provides quick feedback without browser overhead
- **Real Browser Option**: Playwright browsers available when needed for accuracy
- **Type-Safe Tests**: Specs written in Elm with compile-time guarantees
- **Flexible Environments**: Choose speed (jsdom) or accuracy (browsers) per test run
- **Nix Integration**: Browsers provided via devenv.nix with compatibility shims
- **CI/CD Friendly**: jsdom mode requires no browser installation
- **Complements Existing Tests**: Works alongside elm-test, Playwright E2E, and BDD
- **Reusable Setup**: Configuration can be replicated in other Elm projects

### Negative

- **Additional Test Type**: Developers must understand when to use elm-spec vs elm-test vs E2E
- **API Learning Curve**: elm-spec has its own API distinct from elm-test
- **Browser Version Complexity**: Nix-provided browsers may have version mismatches requiring compatibility shims
- **Separate Configuration**: Maintaining elm-spec-core/elm.json alongside main elm.json
- **Limited Documentation**: elm-spec documentation requires external reference
- **jsdom Limitations**: Default mode doesn't test actual rendering, CSS, or complex browser APIs

### Neutral

- **Test File Naming**: Specs use `*Spec.elm` suffix to distinguish from `*Tests.elm`
- **Compilation Target**: Specs compiled separately from main application
- **Multiple Commands**: Different commands for different execution modes

## Implementation Details

### elm.json Updates

```json
{
	"test-dependencies": {
		"direct": {
			"brian-watkins/elm-spec": "3.2.0",
			"elm-explorations/test": "2.2.0"
		},
		"indirect": {
			"elm/random": "1.0.0",
			"elm/regex": "1.0.0"
		}
	}
}
```

### elm-spec-core/elm.json

```json
{
	"type": "application",
	"source-directories": ["../src", "../tests"],
	"elm-version": "0.19.1",
	"dependencies": {
		"direct": {
			"brian-watkins/elm-spec": "3.2.0",
			"elm/browser": "1.0.2",
			"elm/core": "1.0.5",
			"elm/file": "1.0.5",
			"elm/html": "1.0.1",
			"elm/http": "2.0.0",
			"elm/json": "1.1.4",
			"elm/regex": "1.0.0",
			"elm/time": "1.0.0",
			"elm/url": "1.0.0",
			"elm-community/json-extra": "4.3.0"
		},
		"indirect": {
			"elm/bytes": "1.0.8",
			"elm/parser": "1.1.0",
			"elm/random": "1.0.0",
			"elm/virtual-dom": "1.0.5",
			"rtfeldman/elm-iso8601-date-strings": "1.1.4"
		}
	},
	"test-dependencies": {
		"direct": {},
		"indirect": {}
	}
}
```

### .gitignore Updates

```gitignore
# Elm
elm-stuff/
elm-spec-core/elm-stuff/
elm.js
elm.json
```

### Example Spec Structure

```elm
module ComponentSpec exposing (..)

import Spec exposing (..)
import Spec.Setup as Setup
import Spec.Observer as Observer
import Spec.Markup as Markup
import Spec.Markup.Selector exposing (..)
import Spec.Markup.Event as Event
import Spec.Claim as Claim


spec : Spec Model Msg
spec =
    describe "Component Name"
        [ scenario "user interaction behavior"
            (given
                (Setup.initWithModel initialModel
                    |> Setup.withUpdate update
                    |> Setup.withView view
                )
                |> when "user performs action"
                    [ Markup.target << by [ id "element-id" ]
                    , Event.click
                    ]
                |> it "produces expected result"
                    (Observer.observeModel
                        |> Observer.focus .someField
                        |> expect (Claim.isEqual Debug.toString expectedValue)
                    )
            )
        ]
```

## Usage Guidelines

### When to Use elm-spec

**Use elm-spec for:**

- Testing UI component behavior with user interactions
- Verifying view rendering based on model state
- Testing form interactions and validation
- Component-level state management testing
- Testing event handlers and DOM updates

**Don't use elm-spec for:**

- Pure logic and data transformations (use elm-test)
- Full application E2E workflows (use Playwright E2E)
- User story acceptance testing (use Playwright BDD)
- Performance-critical algorithms (use elm-test)

### Browser Environment Selection

**Use jsdom (default) when:**

- Testing component logic and state
- Testing user interactions (clicks, input)
- Running in CI/CD
- Fast feedback loop is priority
- Visual rendering is not critical

**Use real browsers when:**

- Testing CSS-dependent behavior
- Testing animations or transitions
- Verifying browser-specific features
- Debugging visual issues
- Testing accessibility with screen readers

### Spec File Conventions

- **Naming**: `ComponentSpec.elm` (PascalCase + "Spec" suffix)
- **Location**: `tests/` directory
- **Module Name**: Must match filename
- **Export**: `spec : Spec Model Msg` function
- **Comments**: Reference related US (User Story) and ADR IDs

## Alternatives Considered

### elm-program-test (ADR-0011)

- **Pros**: Comprehensive program testing, HTTP stubbing, port testing
- **Cons**: More complex setup, tests entire application rather than components
- **Decision**: Use elm-program-test for full program integration, elm-spec for components

### Playwright Component Testing

- **Pros**: Real browser environment, visual testing, existing Playwright infrastructure
- **Cons**: Requires TypeScript/JavaScript, cannot directly test Elm modules, slower
- **Decision**: Use Playwright for E2E, elm-spec for component testing in Elm

### Pure elm-test with Manual Rendering

- **Pros**: Simple, type-safe, fast
- **Cons**: Cannot test user interactions, cannot verify DOM output
- **Decision**: Use elm-test for logic, elm-spec for behavior/interaction testing

### Cypress Component Testing

- **Pros**: Visual testing, time travel debugging
- **Cons**: JavaScript-based, large dependency, slow, not Elm-native
- **Decision**: Rejected in favor of Elm-native solution

## Migration Path

For existing projects adopting this setup:

1. **Install Dependencies**:

   ```bash
   pnpm add -D elm-spec-runner@^2.5.1
   ```

2. **Update elm.json**:
   Add `brian-watkins/elm-spec` to test-dependencies

3. **Create elm-spec-core/elm.json**:
   Copy structure from this ADR, update dependencies to match project

4. **Add Scripts**:
   Copy npm scripts, Makefile targets, and devenv scripts

5. **Update devenv.nix**:
   Add browser compatibility symlinks if using Nix

6. **Create First Spec**:
   Start with simple component to verify setup

7. **Update CI/CD**:
   Add `pnpm test:spec` to test pipeline

## Related Decisions

- **ADR-0001**: Framework Choice (Elm) - Establishes Elm as primary language
- **ADR-0009**: Build Deployment (Nix + devenv) - Provides development environment
- **ADR-0010**: Code Quality Tools - Establishes elm-format and elm-review
- **ADR-0011**: Testing Strategy - Defines overall test hierarchy

## References

- [elm-spec GitHub Repository](https://github.com/brian-watkins/elm-spec) - Official documentation and examples
- [elm-spec Package Documentation](https://package.elm-lang.org/packages/brian-watkins/elm-spec/latest/) - API reference
- [Playwright Documentation](https://playwright.dev/) - Browser automation reference
- [jsdom Documentation](https://github.com/jsdom/jsdom) - Lightweight DOM implementation

## Documentation

Detailed setup and usage documentation: `agents/elm-spec-setup.md`

Quick reference: `agents/README-elm-spec.md`

## Notes

- This ADR documents the actual implemented setup as of 2025-12-20
- Browser compatibility shims in devenv.nix may need updates as Playwright versions change
- elm-spec-runner uses Playwright 1.11.1 internally (older version)
- Project uses Playwright 1.57.0 for E2E tests
- Compatibility is maintained through Nix symlinks in devenv.nix
- jsdom mode is recommended as default to avoid browser version complexity
- Real browsers should be used selectively when visual/CSS testing is required
