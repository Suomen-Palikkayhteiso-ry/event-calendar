# ADR 0004: Authentication Method - OIDC via SvelteKit with Elm Integration

## Status

Accepted (Updated for hybrid SvelteKit/Elm migration)

## Date

2025-12-19

## Context

The application requires user authentication for:

- Event creation and management
- Admin access to content
- Secure API access

With the migration to Elm (ADR-0012), maintaining SvelteKit v2 as the shell, we need authentication that integrates SvelteKit's OIDC capabilities with the Elm application.

## Decision

We adopt **OIDC authentication handled by SvelteKit**, with **user state passed to Elm via Flags/Ports**.

The authentication flow is:
1. User clicks "Login" in Elm frontend
2. Elm sends message to JavaScript via Port
3. JavaScript redirects to SvelteKit auth route
4. SvelteKit performs OIDC handshake with PocketBase
5. On success, SvelteKit stores session and redirects back to Elm app
6. User data passed to Elm via Flags on app initialization
7. Elm app receives user data and updates model accordingly

## Consequences

### Positive

- **Leverages Existing**: Uses proven SvelteKit OIDC implementation
- **Security**: HTTP-only cookies and server-side session management
- **Type Safety**: User data typed in Elm via custom types
- **Incremental**: Allows gradual migration without rewriting auth logic
- **Separation**: Auth logic in SvelteKit, business logic in Elm

### Negative

- **Interop Complexity**: Requires Ports/Flags for state synchronization
- **Dual State**: User state managed in both SvelteKit (session) and Elm (model)
- **Not Pure Elm**: Authentication not entirely in Elm

## Alternatives Considered

### Client-Side OIDC Libraries

- Considered pure client-side OAuth2 in Elm
- Rejected due to security risks and complexity of managing secrets client-side

### Server.Session in elm-pages

- Considered for pure Elm server-rendered auth
- Rejected to maintain incremental migration path

### SvelteKit Auth (Legacy)

- Current implementation
- Accepted as foundation, with Elm integration

## Implementation

**SvelteKit auth route (server-side):**
```typescript
// routes/auth/callback/+server.ts
import { redirect } from '@sveltejs/kit';
import { pb } from '$lib/pocketbase';

export async function GET({ url, cookies }) {
  const code = url.searchParams.get('code');
  // OIDC handshake with PocketBase
  const user = await pb.authStore.loadFromCookie(cookies.get('pb_auth'));
  // Store session
  cookies.set('session', encodeSession(user), { httpOnly: true });
  throw redirect(302, '/');
}
```

**Elm Port for login:**
```elm
port login : () -> Cmd msg

-- In update
LoginClicked ->
    ( model, login () )
```

**JavaScript Port subscription:**
```javascript
// src/index.js
app.ports.login.subscribe(() => {
  window.location.href = '/auth/login';
});
```

**Elm Flags for user data:**
```elm
type alias Flags =
    { user : Maybe User
    }

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { user = flags.user, ... }, Cmd.none )
```

## Related Decisions

- ADR 0001: Framework Choice (SvelteKit v2 & Elm)
- ADR 0002: Backend Choice (PocketBase)
- ADR 0012: Migrate Frontend to Elm

## Notes

The `User` type in Elm mirrors the PocketBase user schema.
