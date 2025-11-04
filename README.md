# Event Calendar

This is a simple event calendar application built with SvelteKit, PocketBase, and Tailwind CSS. It allows users to view and manage events, and it generates RSS and iCal feeds for the events.

## Features

- View a list of upcoming events
- View event details
- RSS and iCal feed generation
- User authentication with PocketBase

## Tech Stack

- **Frontend:** SvelteKit, Tailwind CSS
- **Backend:** PocketBase
- **Deployment:** Static site hosting

## Development

### Prerequisites

- Node.js and pnpm
- A running PocketBase instance

### Getting Started

1. **Clone the repository:**

   ```sh
   git clone https://github.com/your-username/event-calendar.git
   cd event-calendar
   ```

2. **Install dependencies:**

   ```sh
   pnpm install
   ```

3. **Configure PocketBase:**

   - Rename `.env.example` to `.env` and update the `POCKETBASE_URL` variable with the URL of your PocketBase instance.
   - The PocketBase configuration is located in `src/lib/pocketbase.ts`.

4. **Run the development server:**

   ```sh
   pnpm run dev
   ```

   The application will be available at `http://localhost:5173`.

### Building for Production

To create a production version of the app, run:

```sh
pnpm run build
```

This will create a `build` directory with the static files that can be deployed to any static site hosting service. The build process also generates the RSS and iCal feeds.

### Other Commands

- **Type checking:**

  ```sh
  pnpm run check
  ```

- **Linting:**

  ```sh
  pnpm run lint
  ```

- **Formatting:**

  ```sh
  pnpm run format
  ```