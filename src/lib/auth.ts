import { writable } from 'svelte/store';
import { pb } from './pocketbase';

export const user = writable(pb.authStore.model);

pb.authStore.onChange(() => {
	user.set(pb.authStore.model);
});

export async function login() {
	await pb.collection('users').authWithOAuth2({ provider: 'oidc' });
}

export function logout() {
	pb.authStore.clear();
}