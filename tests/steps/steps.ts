import { Given, When, Then, Before, After } from 'playwright-bdd';
import { expect } from '@playwright/test';

Given('I am not authenticated', async ({ page }) => {
  // Clear any authentication
  await page.context().clearCookies();
  await page.evaluate(() => localStorage.clear());
});

Given('I am authenticated', async ({ page }) => {
  // Mock authentication - set user in localStorage
  await page.evaluate(() => {
    localStorage.setItem('pb_auth', JSON.stringify({
      token: 'mock-token',
      record: {
        id: 'user-id',
        email: 'user@example.com',
        name: 'Test User'
      }
    }));
  });
});

When('I navigate to the event creation page', async ({ page }) => {
  await page.goto('/events/create');
});

When('I fill in the event form with valid data', async ({ page }) => {
  await page.fill('input[name="title"]', 'Test Event');
  await page.fill('textarea[name="description"]', 'Test Description');
  await page.fill('input[name="startDate"]', '2025-01-01');
  await page.fill('input[name="startTime"]', '10:00');
  await page.fill('input[name="endDate"]', '2025-01-01');
  await page.fill('input[name="endTime"]', '12:00');
  await page.fill('input[name="location"]', 'Test Location');
});

When('I submit the form', async ({ page }) => {
  await page.click('button[type="submit"]');
});

When('I submit the form without filling required fields', async ({ page }) => {
  await page.click('button[type="submit"]');
});

Then('I should be redirected to the home page', async ({ page }) => {
  await expect(page).toHaveURL('/');
});

Then('I should see membership instructions', async ({ page }) => {
  await expect(page.locator('text=Membership required')).toBeVisible();
});

Then('I should see a success message', async ({ page }) => {
  await expect(page.locator('text=Event created successfully')).toBeVisible();
});

Then('the event should be created', async ({ page }) => {
  // Check if redirected to event page or success state
  await expect(page).toHaveURL(/\/events\//);
});

Then('I should see validation errors', async ({ page }) => {
  await expect(page.locator('.error')).toBeVisible();
});