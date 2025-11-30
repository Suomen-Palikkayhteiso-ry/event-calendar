import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen } from '@testing-library/svelte';
import { userEvent } from '@testing-library/user-event';
import { writable, type Writable } from 'svelte/store';

// Create mock stores before vi.mock calls
const createMockUser = () => writable<{ name: string } | null>(null);

// Module-level variables for the mock implementations
let mockUserStore: Writable<{ name: string } | null>;
let mockLogin: ReturnType<typeof vi.fn>;
let mockLogout: ReturnType<typeof vi.fn>;

// Mock svelte-i18n
vi.mock('svelte-i18n', () => ({
	_: writable((key: string) => key)
}));

// Mock $app/paths
vi.mock('$app/paths', () => ({
	resolve: (path: string) => path
}));

// Mock auth module - vi.mock is hoisted, so we use dynamic import pattern
vi.mock('$lib/auth', async () => {
	const { writable } = await import('svelte/store');
	const user = writable<{ name: string } | null>(null);
	return {
		user,
		login: vi.fn(),
		logout: vi.fn()
	};
});

import Header from './Header.svelte';
import * as authModule from '$lib/auth';

describe('Header', () => {
	beforeEach(async () => {
		vi.clearAllMocks();
		// Reset the user store to null before each test
		(authModule.user as Writable<{ name: string } | null>).set(null);
	});

	it('renders the calendar title link', () => {
		render(Header);

		const titleLink = screen.getByRole('link', { name: 'calendar_title' });
		expect(titleLink).toBeInTheDocument();
		expect(titleLink).toHaveAttribute('href', '/');
	});

	it('shows login button when user is not logged in', () => {
		(authModule.user as Writable<{ name: string } | null>).set(null);
		render(Header);

		const loginButton = screen.getByRole('button', { name: 'login' });
		expect(loginButton).toBeInTheDocument();
	});

	it('shows logout button and user name when user is logged in', () => {
		(authModule.user as Writable<{ name: string } | null>).set({ name: 'Test User' });
		render(Header);

		const userNameElement = screen.getByText('Test User');
		expect(userNameElement).toBeInTheDocument();

		const logoutButton = screen.getByRole('button', { name: 'logout' });
		expect(logoutButton).toBeInTheDocument();
	});

	it('calls login function when login button is clicked', async () => {
		const user = userEvent.setup();
		(authModule.user as Writable<{ name: string } | null>).set(null);
		render(Header);

		const loginButton = screen.getByRole('button', { name: 'login' });
		await user.click(loginButton);

		expect(authModule.login).toHaveBeenCalledOnce();
	});

	it('calls logout function when logout button is clicked', async () => {
		const user = userEvent.setup();
		(authModule.user as Writable<{ name: string } | null>).set({ name: 'Test User' });
		render(Header);

		const logoutButton = screen.getByRole('button', { name: 'logout' });
		await user.click(logoutButton);

		expect(authModule.logout).toHaveBeenCalledOnce();
	});
});

