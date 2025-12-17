import tailwindcss from '@tailwindcss/vite';
import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig } from 'vite';
import { plugin as elmPlugin } from 'vite-plugin-elm-watch';

export default defineConfig({
	plugins: [tailwindcss(), sveltekit(), elmPlugin()],
	base: '/'
});
