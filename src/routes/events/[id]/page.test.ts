import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, waitFor } from '@testing-library/svelte';
import { writable } from 'svelte/store';
import userEvent from '@testing-library/user-event';
import { tick } from 'svelte';

// Mock PocketBase
vi.mock('$lib/pocketbase', () => {
	const mockEvent = {
		id: 'test-event-id',
		title: 'Test Event',
		description: 'Test description',
		location: 'Helsinki',
		start_date: '2023-12-01T10:00:00Z',
		end_date: '2023-12-01T12:00:00Z',
		all_day: false,
		url: 'https://example.com',
		image: 'test-image.jpg',
		image_description: 'Test image',
		point: { lat: 60.1699, lon: 24.9384 },
		state: 'published'
	};
	const mockGetOne = vi.fn().mockResolvedValue(mockEvent);
	const mockUpdate = vi.fn().mockResolvedValue({});
	const mockGetUrl = vi.fn().mockReturnValue('mock-image-url');
	const collection = vi.fn(() => ({ getOne: mockGetOne, update: mockUpdate }));
	return {
		pb: {
			collection,
			files: {
				getUrl: mockGetUrl
			}
		}
	};
});

// Mock svelte-i18n
vi.mock('svelte-i18n', () => ({
	_: writable((key: string) => key)
}));

// Mock $app/navigation
vi.mock('$app/navigation', () => ({
	goto: vi.fn()
}));

// Mock $app/paths
vi.mock('$app/paths', () => ({
	resolve: vi.fn((path: string) => path)
}));

// Mock $app/stores
vi.mock('$app/stores', () => ({
	page: writable({
		params: { id: 'test-event-id' }
	})
}));

// Mock $lib/auth
vi.mock('$lib/auth', () => ({
	user: writable({ name: 'Test User' })
}));

// Mock @zerodevx/svelte-toast
vi.mock('@zerodevx/svelte-toast', () => ({
	toast: { push: vi.fn() }
}));

// Mock date-utils
vi.mock('$lib/date-utils', () => ({
	formatDateInHelsinki: vi.fn((date: string, allDay: boolean) => date)
}));

import EventPage from './+page.svelte';
import { goto } from '$app/navigation';
import { toast } from '@zerodevx/svelte-toast';

describe('events/[id]/+page.svelte event display', () => {
	beforeEach(() => {
		vi.clearAllMocks();
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	it('loads and displays event details', async () => {
		render(EventPage);

		await waitFor(() => {
			expect(screen.getByRole('heading', { name: 'Test Event' })).toBeInTheDocument();
		});

		expect(screen.getByText('Test description')).toBeInTheDocument();
		expect(screen.getByText('Helsinki')).toBeInTheDocument();
		expect(screen.getByText('https://example.com')).toBeInTheDocument();
	});

	it('shows edit and delete buttons for logged in user', async () => {
		render(EventPage);

		await waitFor(() => {
			expect(screen.getByRole('button', { name: 'edit' })).toBeInTheDocument();
			expect(screen.getByRole('button', { name: 'delete' })).toBeInTheDocument();
		});
	});

	it('navigates to edit page when edit button is clicked', async () => {
		const user = userEvent.setup();

		render(EventPage);

		await waitFor(() => {
			const editButton = screen.getByRole('button', { name: 'edit' });
			user.click(editButton);
		});

		await waitFor(() => {
			expect(vi.mocked(goto)).toHaveBeenCalledWith('/events/test-event-id/edit');
		});
	});

	it('deletes event when delete is confirmed', async () => {
		const user = userEvent.setup();
		const mockConfirm = vi.fn(() => true);
		window.confirm = mockConfirm;

		render(EventPage);

		await waitFor(() => {
			const deleteButton = screen.getByRole('button', { name: 'delete' });
			user.click(deleteButton);
		});

		await waitFor(() => {
			expect(mockConfirm).toHaveBeenCalledWith('confirm_delete_event');
			expect(vi.mocked(toast.push)).toHaveBeenCalledWith('event_deleted_successfully');
			expect(vi.mocked(goto)).toHaveBeenCalledWith('/');
		});
	});

	it('navigates back to calendar on Escape key press', async () => {
		render(EventPage);

		await waitFor(() => {
			expect(screen.getByRole('heading', { name: 'Test Event' })).toBeInTheDocument();
		});

		// Simulate Escape key press
		const escapeEvent = new KeyboardEvent('keydown', { key: 'Escape' });
		document.dispatchEvent(escapeEvent);

		await waitFor(() => {
			expect(vi.mocked(goto)).toHaveBeenCalledWith('/');
		});
	});

	it('navigates to edit page on E key press when logged in', async () => {
		render(EventPage);

		await waitFor(() => {
			expect(screen.getByRole('heading', { name: 'Test Event' })).toBeInTheDocument();
		});

		// Simulate 'e' key press
		const eEvent = new KeyboardEvent('keydown', { key: 'e' });
		document.dispatchEvent(eEvent);

		await waitFor(() => {
			expect(vi.mocked(goto)).toHaveBeenCalledWith('/events/test-event-id/edit');
		});
	});

	it('displays location as link when coordinates are present', async () => {
		render(EventPage);

		await waitFor(() => {
			const locationLink = screen.getByRole('link', { name: 'Helsinki' });
			expect(locationLink).toBeInTheDocument();
			expect(locationLink).toHaveAttribute(
				'href',
				'https://www.openstreetmap.org/?mlat=60.1699&mlon=24.9384&zoom=15'
			);
			expect(locationLink).toHaveAttribute('target', '_blank');
		});
	});

	it('shows loading message when event is not loaded', async () => {
		// Mock getOne to never resolve to simulate loading state
		const { pb } = await import('$lib/pocketbase');
		vi.mocked(pb.collection).mockReturnValue({
			getOne: vi.fn(() => new Promise(() => {})), // Never resolves
			update: vi.fn()
		} as any);

		render(EventPage);

		expect(screen.getByText('loading_event')).toBeInTheDocument();
	});
});
