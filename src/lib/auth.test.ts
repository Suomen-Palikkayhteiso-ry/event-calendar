import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { user, login, logout } from '$lib/auth';
import { get } from 'svelte/store';

// Mock the pocketbase module
vi.mock('$lib/pocketbase', () => {
	const mockPb = {
		authStore: {
			model: null,
			onChange: vi.fn().mockImplementation((callback) => {
				// Store the callback for testing
				mockPb.authStore._callback = callback;
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
	describe('user store', () => {
		it('should initialize with null user', () => {
			expect(get(user)).toBeNull();
		});
	});

	describe('login', () => {
		it('should call authWithOAuth2 with correct provider', async () => {
			const { pb } = await import('$lib/pocketbase');
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
			const { pb } = await import('$lib/pocketbase');

			logout();

			expect(pb.authStore.clear).toHaveBeenCalled();
		});
	});
});