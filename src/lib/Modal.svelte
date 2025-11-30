<script lang="ts">
	import { onMount } from 'svelte';

	interface Props {
		open: boolean;
		title?: string;
		size?: 'sm' | 'md' | 'lg' | 'xl';
		closeOnBackdrop?: boolean;
		closeOnEscape?: boolean;
		children?: any;
	}

	let {
		open,
		title,
		size = 'md',
		closeOnBackdrop = true,
		closeOnEscape = true,
		children
	}: Props = $props();

	let modalElement: HTMLElement;
	let previousFocus: HTMLElement | null = null;

	// Handle escape key
	$effect(() => {
		if (open && closeOnEscape) {
			const handleKeydown = (event: KeyboardEvent) => {
				if (event.key === 'Escape') {
					open = false;
				}
			};
			document.addEventListener('keydown', handleKeydown);
			return () => document.removeEventListener('keydown', handleKeydown);
		}
	});

	// Focus management
	$effect(() => {
		if (open) {
			previousFocus = document.activeElement as HTMLElement;
			// Focus the modal after a short delay to ensure it's rendered
			setTimeout(() => {
				if (modalElement) {
					modalElement.focus();
				}
			}, 10);
		} else if (previousFocus) {
			previousFocus.focus();
			previousFocus = null;
		}
	});

	// Prevent body scroll when modal is open
	$effect(() => {
		if (open) {
			document.body.style.overflow = 'hidden';
		} else {
			document.body.style.overflow = '';
		}
	});

	function handleBackdropClick(event: MouseEvent) {
		if (closeOnBackdrop && event.target === event.currentTarget) {
			open = false;
		}
	}
</script>

{#if open}
	<!-- svelte-ignore a11y-no-noninteractive-element-interactions -->
	<div
		class="modal-backdrop"
		role="dialog"
		aria-modal="true"
		aria-labelledby={title ? 'modal-title' : undefined}
		tabindex="-1"
		bind:this={modalElement}
		onclick={handleBackdropClick}
	>
		<div class="modal-content modal-{size}">
			{#if title}
				<div class="modal-header">
					<h2 id="modal-title" class="modal-title">{title}</h2>
					<button
						type="button"
						class="modal-close"
						aria-label="Close modal"
						onclick={() => (open = false)}
					>
						×
					</button>
				</div>
			{/if}
			<div class="modal-body">
				{@render children?.()}
			</div>
		</div>
	</div>
{/if}

<style>
	.modal-backdrop {
		@apply bg-opacity-50 fixed inset-0 z-50 flex items-center justify-center bg-black p-4;
	}

	.modal-content {
		@apply relative max-h-full overflow-auto rounded-lg bg-white shadow-xl;
	}

	.modal-sm {
		@apply w-full max-w-sm;
	}

	.modal-md {
		@apply w-full max-w-md;
	}

	.modal-lg {
		@apply w-full max-w-lg;
	}

	.modal-xl {
		@apply w-full max-w-xl;
	}

	.modal-header {
		@apply flex items-center justify-between border-b border-gray-200 p-4;
	}

	.modal-title {
		@apply text-lg font-semibold text-gray-900;
	}

	.modal-close {
		@apply flex h-8 w-8 items-center justify-center rounded-full text-gray-400 hover:bg-gray-100 hover:text-gray-600 focus:ring-2 focus:ring-brand-primary focus:outline-none;
	}

	.modal-body {
		@apply p-4;
	}
</style>
