import tailwindcss from '@tailwindcss/vite';
import { defineConfig } from 'vite';
import elm from 'vite-plugin-elm-watch';

export default defineConfig({
	plugins: [tailwindcss(), elm()],
	base: '/',
	server: {
		proxy: {
			'/api': {
				target: 'http://localhost:8090',
				changeOrigin: true
			}
		}
	}
});
