import { writable } from 'svelte/store';
import { pb } from './pocketbase';

export const user = writable(pb.authStore.model);

pb.authStore.onChange(() => {
	user.set(pb.authStore.model);
});

/**
 * Initiates OAuth2 login with OIDC provider
 */
export async function login() {
	await pb.collection('users').authWithOAuth2({ provider: 'oidc' });
}

/**
 * Logs out the current user and clears auth store
 */
export function logout() {
	pb.authStore.clear();
}
