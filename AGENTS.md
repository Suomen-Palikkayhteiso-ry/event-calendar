# AGENTS.md

## Purpose

This file defines how LLM coding agents should understand, navigate, and safely modify this repository. It provides an explicit self-guidance model to reduce hallucination, ensure architectural compliance, and maintain product intent.

## Source of Truth (Precedence Order)

1. **Tests** = Truth: Tests encode observable, verifiable behavior.
2. **ADRs** = Constraints: Architectural rules define limits.
3. **User Stories** = Intent: Stories explain why behavior exists.
4. **Inline Comments** = Implementation notes.
5. **Assumptions** = Last resort.

**Rule:** Tests > ADRs > User Stories > Inline Comments > Assumptions

## Quick Reference

- **Project**: Event calendar application (Elm SPA)
- **Frontend**: Elm (Active Development)
- **Backend**: PocketBase (External)
- **Styling**: Tailwind CSS v4
- **Package Manager**: pnpm (Node/Build), elm (Application)
- **Environment**: devenv (Nix)

## Repository Map

```
./AGENTS.md                # This file - Entry point for LLM agents
./agents/
  /adrs/                   # Architectural Decision Records (constraints)
    ADR-0000-agent-guidance.md  # Agent self-guidance model
  /stories/                # User stories (product intent)
  README.md                # Repository orientation index
  development-setup.md     # Environment setup
  project-overview.md      # Detailed project description
  architecture.md          # System architecture
  security.md              # Security considerations
/tests/                    # Behavioral truth
  /features/               # Playwright BDD scenarios
  /*Tests.elm              # Elm Unit Tests
/src/                      # Implementation
  Main.elm                 # Core Elm application
  index.js                 # JS Entry point & Ports
  elm.js                   # Compiled Elm application (artifact)
/scripts/                  # Node.js Build/Utility scripts
```

## Agent Work Loop

Before making ANY changes, agents MUST:

1. **State the Goal**: Clearly identify the perceived task or requirement.
2. **Locate Context**: Find relevant tests, user stories, and ADRs.
3. **Identify Constraints**: List architectural and behavioral constraints from ADRs.
4. **Propose Strategy**: Outline minimal change approach.
5. **Test First**: Add or update tests for new behavior.
6. **Implement**: Make code changes.
7. **Re-evaluate**: Verify changes against stories, ADRs, and tests.

**If any step fails or is ambiguous, ESCALATE rather than guessing.**

## Change Rules

- **DO NOT** change public APIs without updating tests and stories.
- **DO NOT** violate ADRs without creating a new ADR.
- **DO NOT** commit until all QA tasks pass (Elm Check + E2E).
- **WHEN UNCERTAIN**, add a failing test demonstrating expected behavior.

## Escalation Rules

Stop and escalate when:

- Tests conflict with user stories.
- ADR constraints would be violated.
- Behavior is unclear or ambiguous.
- Multiple plausible interpretations exist.

### Escalation Actions

1. Add failing tests to clarify intent.
2. Propose new or updated ADRs.
3. Document uncertainties in TODO.md.
4. Request human guidance.

## Key Commands

### Elm (Primary)
- `devenv script elm-build` (or `make elm-build`) - Compile Elm application
- `devenv script elm-check` (or `make elm-check`) - Format, Review, and Test Elm code
- `make watch` - Start Elm development server with hot-reload (elm-live)

### Build / Test
- `pnpm install` - Install Node dependencies
- `pnpm dev` - Start Vite development server
- `pnpm build` - Build application for production
- `pnpm test:bdd` - Run BDD tests (Playwright)
- `pnpm test:e2e` - Run End-to-End tests

## Code Style

- **Elm**: Standard `elm-format`.
- **Indentation**: 4 spaces (Elm standard).
- **Quotes**: Double quotes (Elm standard).
- **Naming**: camelCase for functions/variables, PascalCase for Types/Modules.
- **Msg Conventions**:
    - **Top-Level**: `PageMsg SubModule.Msg` (e.g., `CalendarMsg Calendar.Msg`).
    - **Events**: `VerbSubject` (e.g., `ClickSave`, `ReceiveEvents`).
    - **Http**: `FetchedResource (Result Http.Error DataType)`.

## Testing Practices

- **Unit Tests**: Use `elm-test` for all pure logic (Utils, Update functions).
- **BDD/E2E**: Use Playwright for user-visible workflows and cross-module behavior.
- **Test Naming**: Name tests to reflect behavior, not implementation.
- **Test Headers**: Reference US (User Story) and ADR IDs in test comments.

### BDD Guidelines

- **Use For**: User-visible workflows, cross-module behavior, API-level rules.
- **Avoid For**: Algorithms, parsing, data transformations, performance-critical logic.
- **Rules**:
  - Steps must be concrete and self-contained.
  - Avoid abstract steps; redundancy is acceptable.
  - Step definitions must contain minimal logic.

**Example:**
```gherkin
# Covers US-014
# Constrained by ADR-003, ADR-0000
Scenario: Creating a user with existing email
  Given a user exists with email "a@example.com"
  When another user is created with email "a@example.com"
  Then the request fails with a conflict error
```

## Commit Message Format

All commits MUST follow [Conventional Commits 1.0.0](https://www.conventionalcommits.org/en/v1.0.0/).

### Format
```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

### Types
- `feat:` - New feature (MINOR in SemVer)
- `fix:` - Bug fix (PATCH in SemVer)
- `docs:` - Documentation only
- `style:` - Code style changes (formatting)
- `refactor:` - Code refactoring without behavior change
- `perf:` - Performance improvements
- `test:` - Adding or modifying tests
- `build:` - Build system or dependency changes
- `ci:` - CI/CD configuration changes
- `chore:` - Other changes (maintenance)
- `revert:` - Reverts a previous commit

### Breaking Changes
- Append `!` after type/scope: `feat!:` or `feat(api)!:`
- Or add `BREAKING CHANGE:` footer

### Requirements
1. Use lowercase for type and scope
2. Use imperative mood ("add" not "added")
3. Keep description under 72 characters
4. Reference ADRs, user stories, or issues in footer
5. Add `BREAKING CHANGE:` footer when changing public APIs

### Examples
```bash
feat(auth): add OAuth2 authentication support

fix: prevent race condition in request handler

feat!: drop support for Node 14

docs(adr): add ADR-0000 for agent guidance

test(calendar): add BDD scenario for event creation
Covers US-014
Refs: ADR-003
```

## Migration Context

This project has been migrated from SvelteKit to Elm.

- **Source of Truth**: `src/Main.elm` is the core application.
- **Legacy Reference**: The `refactor` branch contains the previous SvelteKit implementation if reference is needed.
- **Do Not**: Introduce Svelte components or SvelteKit routing logic.

## Artifact Roles & Cross-Linking

- **Tests**: Reference US and ADR IDs in headers/docstrings. Named to reflect behavior.
- **User Stories**: Reference tests and ADRs. Acceptance criteria must be executable.
- **ADRs**: Define constraints, reference affected modules, tests, and stories. Prescriptive.
- **AGENTS.md**: Defines reading order, work loop, precedence, and escalation rules.

## Important Notes

- **TODO.md** tracks active tasks (local, uncommitted)
- Follow ADR process for significant technical decisions
- All tests must pass before committing

## Agent TODO Template

Use this format for compliance and review tasks:

```
* [ ] Identify missing or outdated user stories under ./agents/stories
* [ ] Map existing tests to user stories and ADRs
* [ ] Add references in test headers to US and ADR IDs
* [ ] Review ADRs for missing constraints or outdated decisions
* [ ] Convert suitable high-level tests to BDD scenarios
* [ ] Simplify or refactor over-abstracted BDD steps
* [ ] Add tests where acceptance criteria are not executable
* [ ] Flag conflicts between tests, stories, and ADRs
* [ ] Propose new ADRs where architectural intent is implicit
```

## Getting Started

1. Read this file (AGENTS.md)
2. Review [ADR-0000](./agents/adrs/ADR-0000-agent-guidance.md) for self-guidance model
3. Check [User Stories](./agents/stories/) for product intent
4. Review [ADRs](./agents/adrs/) for architectural constraints
5. Examine [Tests](./tests/) for behavioral truth
6. Follow [Development Setup](./agents/development-setup.md) for environment

## Related Documentation

- [Project Overview](./agents/project-overview.md) - Detailed project description
- [Architecture](./agents/architecture.md) - High-level system architecture
- [Security](./agents/security.md) - Security considerations
- [Development Setup](./agents/development-setup.md) - Environment setup
