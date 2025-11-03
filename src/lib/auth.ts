import { writable } from 'svelte/store';
import { UserManager, User } from 'oidc-client-ts';

export const user = writable<User | null>(null);

const userManager = new UserManager({
	authority: import.meta.env.VITE_OIDC_AUTHORITY,
	client_id: import.meta.env.VITE_OIDC_CLIENT_ID,
	redirect_uri: 'http://localhost:5173/callback',
	post_logout_redirect_uri: 'http://localhost:5173'
});

export async function login() {
	await userManager.signinRedirect();
}

export async function logout() {
	await userManager.signoutRedirect();
}

export async function handleCallback() {
	const oidcUser = await userManager.signinRedirectCallback();
	user.set(oidcUser);
}
