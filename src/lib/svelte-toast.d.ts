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

	declare const Toast: (msg: string, options?: ToastOptions) => string;
	export default Toast;
}
