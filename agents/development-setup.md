# Development Setup

## Prerequisites

- [devenv](https://devenv.sh/) for development environment
- [pnpm](https://pnpm.io/) for package management
- [Haskell Stack](https://docs.haskellstack.org/) for Haskell scripts
- Git

## Environment Setup

This project uses `devenv` to provide a reproducible development environment.

1. **Install devenv** (if not already installed):

   ```bash
   # Using nix (recommended)
   nix-env -iA devenv
   ```

2. **Clone the repository**:

   ```bash
   git clone <repository-url>
   cd event-calendar
   ```

3. **Enter development environment**:
   ```bash
   devenv shell
   ```

## Package Management

All package management commands should be run within the devenv shell.

### Install Dependencies

```bash
pnpm install
```

### Development Server

```bash
pnpm dev
```

Starts the development server at `http://localhost:5173`

### Build for Production

```bash
pnpm build
```

### Static Generation

```bash
pnpm generate-statics
```

Generates RSS, Atom, JSON, GeoJSON, ICS, and HTML feeds using Haskell scripts.

#### Haskell Scripts

The project includes Haskell scripts for static generation:

- `GenerateUtils.hs` - Common utilities for PocketBase API interaction
- `GenerateFeeds.hs` - RSS/Atom feed generation
- `GenerateIcsGeojson.hs` - ICS calendar and GeoJSON generation
- `GenerateStatics.hs` - HTML static page generation
- `GenerateEmbed.hs` - Embeddable calendar generation

**Building Haskell scripts:**

```bash
# Compile Haskell scripts (if needed)
stack build
```

**Running Haskell scripts directly:**

```bash
# Generate feeds
stack exec generate-feeds

# Generate ICS and GeoJSON
stack exec generate-ics-geojson

# Generate static HTML
stack exec generate-statics

# Generate embeddable content
stack exec generate-embed
```

### Code Quality

```bash
# Lint and format
pnpm lint

# Type checking
pnpm check

# Format code
pnpm format
```

## Environment Variables

The application requires the following environment variables:

- `PUBLIC_POCKETBASE_URL`: URL of the PocketBase instance (default: `https://data.suomenpalikkayhteiso.fi`)

## Development Workflow

1. Enter devenv shell: `devenv shell`
2. Install dependencies: `pnpm install`
3. Start development server: `pnpm dev`
4. Make changes and test
5. Run quality checks: `pnpm lint && pnpm check`
6. Format code: `pnpm format`
7. Build and test: `pnpm build`

## Project Structure

```
src/
├── Main.elm       # Application Entry Point
├── Types.elm      # Core Data Types
├── Ports.elm      # JS Interop Definitions
├── Calendar.elm   # Calendar Feature Module
├── Map.elm        # Map Feature Module
├── ...            # Other Elm Modules
├── index.js       # JS Entry Point & Port Implementation
└── style.css      # Global Styles

scripts/           # Static generation scripts (Haskell/Node.js/Elm)
├── GenerateUtils.hs     # Haskell utilities for PocketBase API
├── GenerateFeeds.hs     # Haskell RSS/Atom feed generation
├── GenerateIcsGeojson.hs # Haskell ICS/GeoJSON generation
├── GenerateStatics.hs   # Haskell HTML static generation
├── GenerateEmbed.hs     # Haskell embeddable content generation
├── generate_*.py        # Python equivalents (legacy)
├── generate_*.js        # Node.js equivalents (legacy)
agents/            # Documentation and ADRs
docs/              # Project documentation
```

## Code Style Guidelines

- **Language**: Elm (Strict functional typing)
- **Indentation**: 4 Spaces (Strictly enforced by `elm-format`)
- **Quotes**: Double quotes (`"`)
- **Formatting**: Automated with `elm-format` (Elm) and Prettier (JS/CSS)

## Testing

### Elm Unit Tests

```bash
# Run tests
devenv script elm-test (or `make elm-test`)

# Run with watch mode (requires elm-test-rs or similar, otherwise use loop)
devenv script elm-check
```

### Legacy/E2E Tests

```bash
pnpm test           # Run once and exit
pnpm test:watch     # Watch mode for development
pnpm test:coverage  # Run with coverage report
pnpm test:ui        # Interactive UI mode
```

### E2E Tests (Playwright)

```bash
pnpm test:e2e       # Run all E2E tests
pnpm test:e2e:ui    # Interactive UI mode
```

### Playwright on NixOS

This project uses Playwright for E2E testing. On NixOS, browsers are provided by nixpkgs rather than downloaded by Playwright.

**Verify Playwright setup:**

```bash
devenv shell -- ./scripts/check-playwright-compat.sh
```

See [Playwright Setup](./playwright-setup.md) for detailed configuration and troubleshooting.

## Database Setup for Contributors

This project uses PocketBase as the backend database. For development and testing, a local PocketBase instance is automatically set up.

### Local Database Setup

The development environment automatically provides PocketBase. To set up the test database:

```bash
# Enter development environment
devenv shell

# Set up test database (creates admin user and sample data)
devenv run test-db-setup

# Check database health
devenv run check-db-health
```

### Database Health Checks

Before running tests, ensure the database is healthy:

```bash
# Check database connectivity and data integrity
pnpm check-db-health
```

The health check verifies:

- PocketBase server is running and accessible
- Required collections exist (events, users)
- Test data is present and valid
- Events have future dates for testing

### Environment Variables

- `POCKETBASE_URL`: Database URL (default: `http://127.0.0.1:8090` for local, `https://data.suomenpalikkayhteiso.fi` for production)
- `POCKETBASE_TOKEN`: Admin token for database operations (optional, used for health checks)

### Running Tests with Local Database

```bash
# Run all tests with local database
devenv run test

# Or manually:
devenv run test-db-setup  # Set up database
devenv run check-db-health  # Verify health
make test-e2e             # Run E2E tests
```
