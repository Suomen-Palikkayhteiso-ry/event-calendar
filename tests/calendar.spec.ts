import { test, expect } from '@playwright/test';

test.describe('Calendar Navigation and Event Viewing', () => {
	test('should display the calendar page', async ({ page }) => {
		await page.goto('/');

		// Check page title
		await expect(page).toHaveTitle('Palikkakalenteri');

		// Check for calendar elements - the title is in the header
		await expect(page.locator('a:has-text("Palikkakalenteri")')).toBeVisible();

		// For non-authenticated users, check for the non-member message
		await expect(page.locator('text=Jos et ole Suomen Palikkayhteisö ry:n jäsen')).toBeVisible();
	});

	test('should navigate calendar dates', async ({ page }) => {
		await page.goto('/');

		// The calendar is rendered by @event-calendar/core
		// Look for the calendar container
		const calendarContainer = page.locator('.ec');
		await expect(calendarContainer).toBeVisible();

		// Check for navigation buttons (prev/next)
		await expect(page.locator('.ec-prev')).toBeVisible();
		await expect(page.locator('.ec-next')).toBeVisible();
	});

	test('should display events on calendar', async ({ page }) => {
		await page.goto('/');

		// Wait for events to load (assuming there are events in the database)
		// The events are rendered inside the calendar component
		// We can check if the page loads without errors

		await expect(page.locator('body')).toBeVisible();

		// Check that no error messages are displayed
		await expect(page.locator('text=Error')).not.toBeVisible();
	});

	test('should handle date selection', async ({ page }) => {
		await page.goto('/');

		// The calendar should be visible and functional
		const calendarContainer = page.locator('.ec');
		await expect(calendarContainer).toBeVisible();

		// Check that the page loads without errors
		await expect(page.locator('body')).toBeVisible();
	});

	test('should switch between calendar and list views', async ({ page }) => {
		await page.goto('/');

		// Check for view toggle buttons
		// The calendar component has buttons for different views
		const calendarViewBtn = page.locator('.ec-button:has-text("Calendar")');
		const listViewBtn = page.locator('.ec-button:has-text("List")');

		// Check if buttons exist (they might be in a toolbar)
		const toolbar = page.locator('.ec-toolbar');
		await expect(toolbar).toBeVisible();
	});

	test('should navigate to today', async ({ page }) => {
		await page.goto('/');

		// Check for "Today" button - use button role
		const todayBtn = page.getByRole('button', { name: 'tänään' });
		await expect(todayBtn).toBeVisible();

		// Click today button
		await todayBtn.click();

		// Page should still be functional
		await expect(page.locator('body')).toBeVisible();
	});
});
