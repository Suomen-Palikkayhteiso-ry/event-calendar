import * as matchers from '@testing-library/jest-dom/matchers';
import { expect, vi } from 'vitest';
import type { TestingLibraryMatchers } from '@testing-library/jest-dom/matchers';

declare module 'vitest' {
	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	interface Assertion<T = any> extends TestingLibraryMatchers<T, void> {}
	interface AsymmetricMatchersContaining extends TestingLibraryMatchers<unknown, void> {}
}

expect.extend(matchers);

// Mock matchMedia for Svelte 5
Object.defineProperty(window, 'matchMedia', {
	writable: true,
	value: vi.fn().mockImplementation((query) => ({
		matches: false,
		media: query,
		onchange: null,
		addListener: vi.fn(), // deprecated
		removeListener: vi.fn(), // deprecated
		addEventListener: vi.fn(),
		removeEventListener: vi.fn(),
		dispatchEvent: vi.fn()
	}))
});
