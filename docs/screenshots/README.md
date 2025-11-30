# Application Screenshots

These screenshots are automatically generated for documentation purposes.

## Public Views

| Screenshot | Description |
|------------|-------------|
| ![calendar-overview](./calendar-overview.png) | Main calendar view showing monthly overview |
| ![calendar-mobile](./calendar-mobile.png) | Calendar view on mobile viewport |
| ![calendar-navigation](./calendar-navigation.png) | Calendar with navigation controls highlighted |

## Authenticated Views

These views require user authentication.

| Screenshot | Description |
|------------|-------------|
| ![event-form](./event-form.png) | Event creation form |
| ![event-list](./event-list.png) | List of existing events (admin view) |

## Regenerating Screenshots

Run the following command to regenerate all screenshots:

```bash
devenv shell -- pnpm generate-screenshots
```

**Note:** The development server must be running (`pnpm dev`) and authentication
screenshots require a valid authenticated session.

## Viewport Sizes

- Desktop: 1280x900
- Mobile: 375x667
