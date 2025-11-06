declare module 'svelte-toast' {
	export interface ToastOptions {
		duration?: number;
		initial?: number;
		next?: number;
		pausable?: boolean;
		dismissable?: boolean;
		reversed?: boolean;
		intro?: { x?: number; y?: number };
		theme?: Record<string, string>;
		classes?: string[];
	}

	export interface Toast {
		id: string;
		duration: number;
		initial: number;
		msg: string;
		theme: Record<string, string>;
		classes: string[];
		intro: { x: number; y: number };
	}

	export const toast: {
		push: (msg: string, options?: ToastOptions) => string;
		pop: (id?: string) => void;
		set: (id: string, options: Partial<ToastOptions>) => void;
	};
}
