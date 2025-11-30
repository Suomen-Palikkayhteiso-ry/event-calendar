import { describe, it, expect, vi } from 'vitest';
import { user, login, logout } from '$lib/auth';
import { get } from 'svelte/store';

// Define mock type
interface MockPb {
	authStore: {
		model: { id: string; name: string } | null;
		onChange: ReturnType<typeof vi.fn>;
		clear: ReturnType<typeof vi.fn>;
	};
	collection: ReturnType<typeof vi.fn>;
}

// Mock the pocketbase module
vi.mock('$lib/pocketbase', () => {
	// Store auth callback reference in globalThis
	const mockPb: MockPb = {
		authStore: {
			model: null,
			onChange: vi.fn().mockImplementation((callback: () => void) => {
				// Store the callback for testing in globalThis
				(globalThis as Record<string, unknown>).__authChangeCallback = callback;
				return () => {}; // Return unsubscribe function
			}),
			clear: vi.fn()
		},
		collection: vi.fn().mockReturnValue({
			authWithOAuth2: vi.fn()
		})
	};

	return {
		pb: mockPb
	};
});

describe('auth', () => {
	const getAuthCallback = () =>
		(globalThis as Record<string, unknown>).__authChangeCallback as (() => void) | undefined;

	describe('user store', () => {
		it('should initialize with null user', () => {
			expect(get(user)).toBeNull();
		});

		it('should update when authStore changes', async () => {
			const { pb } = (await import('$lib/pocketbase')) as unknown as { pb: MockPb };
			const mockUser = { id: 'test-user', name: 'Test User' };

			// Simulate authStore change
			pb.authStore.model = mockUser;
			const callback = getAuthCallback();
			if (callback) callback(); // Trigger the onChange callback

			expect(get(user)).toEqual(mockUser);
		});
	});

	describe('login', () => {
		it('should call authWithOAuth2 with correct provider', async () => {
			const { pb } = (await import('$lib/pocketbase')) as unknown as { pb: MockPb };
			const mockAuthWithOAuth2 = vi.fn().mockResolvedValue({});
			pb.collection.mockReturnValue({
				authWithOAuth2: mockAuthWithOAuth2
			});

			await login();

			expect(mockAuthWithOAuth2).toHaveBeenCalledWith({ provider: 'oidc' });
		});
	});

	describe('logout', () => {
		it('should call authStore.clear', async () => {
			const { pb } = (await import('$lib/pocketbase')) as unknown as { pb: MockPb };

			logout();

			expect(pb.authStore.clear).toHaveBeenCalled();
		});
	});
});