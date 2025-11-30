# Development Setup

## Prerequisites

- [devenv](https://devenv.sh/) for development environment
- [pnpm](https://pnpm.io/) for package management
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
Generates RSS, Atom, JSON, GeoJSON, ICS, and HTML feeds.

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
├── lib/           # Shared utilities and components
│   ├── auth.ts    # Authentication logic
│   ├── pocketbase.ts # PocketBase client
│   ├── types.ts   # TypeScript interfaces
│   └── ...
├── routes/        # SvelteKit routes
│   ├── +page.svelte     # Main calendar page
│   ├── events/          # Event management
│   └── ...
└── app.css        # Global styles and Tailwind theme

scripts/           # Static generation scripts
agents/            # Documentation and ADRs
```

## Code Style Guidelines

- **Indentation**: Tabs
- **Quotes**: Single quotes
- **Trailing Commas**: No trailing commas
- **Print Width**: 100 characters
- **Formatting**: Automated with Prettier

## Testing

Currently, the project does not have automated tests. Manual testing is performed by:
- Running the development server
- Testing user interactions
- Verifying static generation
- Checking build output