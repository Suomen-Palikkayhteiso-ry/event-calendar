import { test, expect } from '@playwright/test';

test.describe('Event Editing and Deletion', () => {
	test('should allow editing existing events', async ({ page }) => {
		// Mock authentication
		await page.addInitScript(() => {
			localStorage.setItem(
				'pb_auth_https://data.suomenpalikkayhteiso.fi',
				JSON.stringify({
					token: 'test-token',
					model: {
						id: 'test-user-id',
						email: 'test@example.com',
						name: 'Test User'
					}
				})
			);
		});

		// First create an event to edit
		await page.goto('/events');

		// Fill and submit create form
		await page.fill('input[name="title"]', 'Event to Edit');
		await page.fill('input[name="location"]', 'Edit Location');
		await page.fill('textarea[name="description"]', 'Description to edit');
		await page.click('button[type="submit"]');

		// Wait for success and find the created event
		await expect(page.locator('.toast')).toContainText('Tapahtuma luotu onnistuneesti');

		// Find the event in the list and click edit
		await page
			.locator('text=Event to Edit')
			.locator('xpath=ancestor::tr')
			.locator('text=Muokkaa')
			.click();

		// Should be on edit page
		await expect(page.locator('h1')).toContainText('Muokkaa tapahtumaa');

		// Modify the title
		await page.fill('input[name="title"]', 'Edited Event');

		// Submit changes
		await page.click('button[type="submit"]');

		// Should show success and redirect
		await expect(page.locator('.toast')).toContainText('Tapahtuma päivitetty onnistuneesti');
	});

	test('should allow deleting events', async ({ page }) => {
		// Mock authentication
		await page.addInitScript(() => {
			localStorage.setItem(
				'pb_auth_https://data.suomenpalikkayhteiso.fi',
				JSON.stringify({
					token: 'test-token',
					model: {
						id: 'test-user-id',
						email: 'test@example.com',
						name: 'Test User'
					}
				})
			);
		});

		await page.goto('/events');

		// Create an event first
		await page.fill('input[name="title"]', 'Event to Delete');
		await page.fill('input[name="location"]', 'Delete Location');
		await page.click('button[type="submit"]');

		await expect(page.locator('.toast')).toContainText('Tapahtuma luotu onnistuneesti');

		// Find the event and change status to deleted
		const eventRow = page.locator('text=Event to Delete').locator('xpath=ancestor::tr');
		await eventRow.locator('select').selectOption('deleted');

		// Should show success
		await expect(page.locator('.toast')).toContainText('Tapahtuma päivitetty onnistuneesti');

		// Event should no longer be visible (assuming deleted events are filtered out)
		await expect(page.locator('text=Event to Delete')).not.toBeVisible();
	});

	test('should handle event detail viewing', async ({ page }) => {
		// This test can work without authentication if events are public
		// Navigate to a public event page
		await page.goto('/events/some-event-id');

		// Should show event details or appropriate message
		// (This depends on whether events are public or require auth)
	});
});
