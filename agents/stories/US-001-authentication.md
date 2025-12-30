# Authentication

## User Story

As a member of Suomen Palikkayhteisö ry, I want to authenticate so that I can create and manage events.

## Acceptance Criteria

- Authentication is handled via PocketBase 'oidc' auth
- Auth state is stored in localStorage
- Authenticated users can access event management features
- Non-authenticated users see mailto:-link with instructions for submitting their events
- Auth token is included in API requests

## Scenarios

- User logs in via PocketBase auth
- Auth state persists in localStorage
- Protected routes redirect non-auth users
- Auth users can create/edit/delete events
