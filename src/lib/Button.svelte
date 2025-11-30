<script lang="ts">
	interface Props {
		variant?: 'primary' | 'secondary' | 'icon';
		size?: 'sm' | 'md' | 'lg';
		disabled?: boolean;
		type?: 'button' | 'submit' | 'reset';
		ariaLabel?: string;
		children?: any;
		onclick?: (event: MouseEvent) => void;
	}

	let {
		variant = 'primary',
		size = 'md',
		disabled = false,
		type = 'button',
		ariaLabel,
		children,
		onclick
	}: Props = $props();
</script>

<button
	{type}
	{disabled}
	aria-label={ariaLabel}
	class="btn btn-{variant} btn-{size}"
	class:disabled
	{onclick}
>
	{@render children?.()}
</button>

<style>
	@reference "tailwindcss";
	.btn {
		@apply cursor-pointer rounded border border-solid transition-colors focus:ring-2 focus:outline-none disabled:cursor-not-allowed;
	}

	.btn-primary {
		@apply border-blue-500 bg-blue-500 text-white hover:bg-blue-600 focus:ring-blue-500 disabled:border-gray-400 disabled:bg-gray-200 disabled:text-gray-600 disabled:hover:bg-gray-300;
	}

	.btn-secondary {
		@apply border-gray-600 bg-gray-600 text-white hover:bg-gray-700 focus:ring-gray-600 disabled:bg-gray-400;
	}

	.btn-icon {
		@apply flex h-12 w-12 flex-shrink-0 items-center justify-center border-none bg-blue-500 text-xl font-bold text-white hover:bg-blue-600 focus:ring-blue-500;
	}

	.btn-sm {
		@apply px-3 py-2 text-sm;
	}

	.btn-md {
		@apply px-6 py-3 text-base;
	}

	.btn-lg {
		@apply px-8 py-4 text-lg;
	}

	.btn-icon.btn-sm {
		@apply h-8 w-8 text-lg;
	}

	.btn-icon.btn-md {
		@apply h-12 w-12 text-xl;
	}

	.btn-icon.btn-lg {
		@apply h-16 w-16 text-2xl;
	}
</style>
