import adapter from '@sveltejs/adapter-static';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';

/** @type {import('@sveltejs/kit').Config} */
const config = {
	// Consult https://svelte.dev/docs/kit/integrations
	// for more information about preprocessors
	preprocess: vitePreprocess(),
	kit: {
		adapter: adapter({
			fallback: 'index.html'
		}),
		paths: {
			base: ''
		},
		prerender: {
			entries: [],
			handleUnseenRoutes: 'ignore',
			handleHttpError: ({ status, path }) => {
				if (path === '/favicon.png' || (status === 404 && path === '/')) return;
				throw new Error(`${status} ${path}`);
			}
		}
	}
};

export default config;
