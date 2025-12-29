/// <reference types="vitest" />
import { defineConfig } from 'vite';

export default defineConfig({
	test: {
		include: ['src/**/*.{test,spec}.{js,ts}'],
		environment: 'jsdom',
		coverage: {
			reporter: ['text', 'json', 'html'],
			exclude: [
				'node_modules/',
				'**/*.d.ts',
				'**/*.config.*',
				'dist/',
				'static/',
				'agents/',
				'scripts/'
			],
			thresholds: {
				global: {
					branches: 70,
					functions: 70,
					lines: 70,
					statements: 70
				}
			}
		}
	}
});
