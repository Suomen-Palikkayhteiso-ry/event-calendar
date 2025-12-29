/// <reference types="vitest" />
import { defineConfig } from 'vite';

export default defineConfig({
	test: {
		include: ['src/**/*.{test,spec}.{js,ts}', 'scripts/**/*.{test,spec}.{js,ts}'],
		exclude: ['tests/**', 'node_modules', 'dist', '.idea', '.git', '.cache'],
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
				'scripts/',
				'tests/'
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
