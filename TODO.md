# TODO

Use this scratchpad to track active tasks for the current session. Update entries as you progress; this file stays local, uncommitted, and ready for the next agent.

Complete a task at time. Follow the practices defined in AGENTS.md. Commit and push. Mark task completed (with the commit hash in parenthesis).

Commit without chaining shell commands with "&&".

## Tasks

- [x] Fix GitHub Actions workflow: change `make clean dist` to `make clean build` since Makefile has no `dist` target (acb3508)
- [x] Fix Github Actions workflow on tests: make: *** No rule to make target 'dist'.  Stop. (3353ce6)

* [x] Write TODO items for doing complete review for every code file in the project and writing tasks for refacoring them into smaller files, component reuse, easier maintenance and other best practices (completed initial review)
* [x] Extract calendar logic from +page.svelte into a separate Calendar component (eb1bf4c)
* [x] Create a reusable EventForm component to eliminate duplication between create and edit forms
* [x] Extract date/time picker logic into a custom DateTimePicker component (a48425e)
* [x] Fix Github Actions to include to run tests on every branch
* [x] Break down the large events/+page.svelte into smaller components (EventForm, EventList, KMLImport)
* [x] Create utility functions for form data handling and validation (98c028d)
* [x] Implement proper error handling and user feedback across components (98c028d)
* [x] Add loading states and skeleton components for better UX (98c028d)
* [x] Extract business logic from components into custom stores or hooks
* [x] Improve accessibility with better ARIA labels and keyboard navigation (c93cd6a)
* [x] Add pagination or virtual scrolling for event lists to improve performance (05efd52)
* [x] Create shared types and interfaces for form data and API responses (defined in src/lib/types.ts)
- [x] Implement consistent error boundaries and fallback UI (9d4c60a)
* [x] Add more comprehensive unit tests for utility functions and components (added tests for kml-utils.ts)
* [x] Optimize bundle size by lazy loading heavy components like maps (Map component is lazy loaded in EventForm when location is entered)
- [x] Create a theme system for consistent styling across components (2180314)
- [x] Add proper TypeScript types for all component props and events (a3b4733)
* [ ] Implement proper state management for complex forms with validation
- [x] Create reusable UI components (buttons, inputs, modals) in a design system
- [x] Add internationalization support for error messages and validation (e4a8c52, tests fixed)
- [x] Fix Tailwind CSS v4 compatibility issues with @reference directives and opacity syntax (37a5e88)
- [x] Implement caching strategies for API calls and static data (a5f7cd7)
* [ ] Add proper logging and monitoring for debugging and analytics
- [x] Move toast into bottom center of the browser. Double their font size from the current. (450537a)
- [x] Add new item button from the front page should navigate through goto(resolve(..)) to properly use hash routing (3f0ec0a)

## Specific Refactoring Tasks from Code Review

### Component Extraction
* [x] Extract Calendar component from `src/routes/+page.svelte` (lines 1-200+)
* [x] Create EventForm component to replace duplicated form logic in `src/routes/events/+page.svelte` and `src/routes/events/[id]/edit/+page.svelte`
* [x] Extract EventList component from `src/routes/events/+page.svelte` (table display logic)
* [x] Extract KMLImport component from `src/routes/events/+page.svelte` (KML parsing and import logic)
* [x] Create DateTimePicker component to handle the complex date/time picker logic used in forms

### Utility Functions
* [x] Create form validation utilities in `src/lib/form-utils.ts`
* [x] Create event data transformation utilities in `src/lib/event-utils.ts`
* [x] Extract KML parsing logic into `src/lib/kml-utils.ts`
* [x] Create API error handling utilities in `src/lib/api-utils.ts`

### Type Definitions
* [x] Define EventFormData interface in `src/lib/types.ts`
* [x] Define CalendarOptions interface for calendar configuration
* [x] Define KML parsing types
* [x] Define API response types for better type safety

### State Management
* [x] Create event store using Svelte 5 runes for event CRUD operations
* [x] Create form store for complex form state management (8d19c36)
* [x] Extract calendar state into a store (d389f6a)

### Performance Optimizations
* [x] Implement lazy loading for Map component (b7596e9)
* [ ] Add virtual scrolling for large event lists
* [ ] Optimize calendar event rendering with memoization

### Accessibility Improvements
* [x] Add proper ARIA labels to form fields (7753244)
* [ ] Improve keyboard navigation in calendar
* [ ] Add screen reader support for event details
* [ ] Implement focus management for modals and forms

### Error Handling
* [x] Add global error boundary component (9d4c60a)
* [ ] Implement form validation with user-friendly error messages
* [ ] Add retry logic for failed API calls
* [ ] Create error logging system

### Testing
* [x] Add unit tests for date-utils functions
* [x] Add unit tests for form validation utilities
* [x] Add unit tests for event data transformation utilities
* [x] Add integration tests for event CRUD operations
* [ ] Add component tests for new extracted components
* [x] Fix timezone issue in event-utils.test.ts causing 'should format timed event' to fail (expects '12.30' but gets '10.30') (6f99bf1)
* [x] Fix Svelte rune error 'rune_outside_svelte' in src/lib/stores/event-form.ts line 34 - $state used outside .svelte files (no longer present)
* [x] Fix event edit page tests failing due to form not loading (stuck on 'loading_event' instead of rendering form) (9adeca8)

### Code Quality
* [ ] Remove duplicate code between create and edit forms
* [ ] Standardize error handling patterns across components
- [x] Add proper TypeScript types to all component props
* [ ] Implement consistent naming conventions
- [x] Add JSDoc comments to utility functions (9aca891)
