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

	let modalElement = $state<HTMLElement>();
	let modalBody = $state<HTMLElement>();
	let previousFocus: HTMLElement | null = null;

	// Handle escape key and focus trapping
	$effect(() => {
		if (open) {
			const handleKeydown = (event: KeyboardEvent) => {
				if (event.key === 'Escape' && closeOnEscape) {
					open = false;
				} else if (event.key === 'Tab') {
					// Focus trapping
					if (modalBody) {
						const focusableElements = modalBody.querySelectorAll(
							'button, [href], input, select, textarea, [tabindex]:not([tabindex="-1"])'
						);
						const firstFocusable = focusableElements[0] as HTMLElement;
						const lastFocusable = focusableElements[focusableElements.length - 1] as HTMLElement;

						if (event.shiftKey) {
							// Shift+Tab
							if (document.activeElement === firstFocusable) {
								event.preventDefault();
								lastFocusable.focus();
							}
						} else {
							// Tab
							if (document.activeElement === lastFocusable) {
								event.preventDefault();
								firstFocusable.focus();
							}
						}
					}
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
			// Focus the first focusable element in the modal body after a short delay
			setTimeout(() => {
				if (modalBody) {
					const focusableElements = modalBody.querySelectorAll(
						'button, [href], input, select, textarea, [tabindex]:not([tabindex="-1"])'
					);
					const firstFocusable = focusableElements[0] as HTMLElement;
					if (firstFocusable) {
						firstFocusable.focus();
					} else {
						// Fallback to modal element
						modalElement?.focus();
					}
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
	<!-- svelte-ignore a11y_no_noninteractive_element_interactions a11y_click_events_have_key_events -->
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
			<div class="modal-body" bind:this={modalBody}>
				{@render children?.()}
			</div>
		</div>
	</div>
{/if}

<style>
	@reference "tailwindcss";
	.modal-backdrop {
		@apply fixed inset-0 z-50 flex items-center justify-center bg-black/50 p-4;
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
		@apply flex h-8 w-8 items-center justify-center rounded-full text-gray-400 hover:bg-gray-100 hover:text-gray-600 focus:ring-2 focus:ring-blue-500 focus:outline-none;
	}

	.modal-body {
		@apply p-4;
	}
</style>
