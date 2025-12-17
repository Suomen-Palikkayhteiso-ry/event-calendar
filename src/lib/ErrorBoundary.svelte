<script lang="ts">
	import { _ } from 'svelte-i18n';

	let { children, fallback, error: externalError = null } = $props();

	let hasError = $state(false);
	let error: Error | null = $state(externalError);

	// Watch for external error changes
	$effect(() => {
		if (externalError) {
			hasError = true;
			error = externalError;
		}
	});

	function resetError() {
		hasError = false;
		error = null;
	}
</script>

{#if hasError}
	{#if fallback}
		{@render fallback({ error, resetError })}
	{:else}
		<div class="error-boundary rounded-lg border border-red-200 bg-red-50 p-4">
			<div class="flex items-center">
				<svg
					class="mr-3 h-5 w-5 text-red-400"
					fill="none"
					stroke="currentColor"
					viewBox="0 0 24 24"
				>
					<path
						stroke-linecap="round"
						stroke-linejoin="round"
						stroke-width="2"
						d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-2.5L13.732 4c-.77-.833-1.964-.833-2.732 0L3.732 16.5c-.77.833.192 2.5 1.732 2.5z"
					></path>
				</svg>
				<div>
					<h3 class="text-sm font-medium text-red-800">
						{$_('component_error_title')}
					</h3>
					<p class="text-sm text-red-700">
						{$_('component_error_description')}
					</p>
					{#if error?.message}
						<p class="mt-1 text-xs text-red-600">
							{error.message}
						</p>
					{/if}
				</div>
			</div>
			<div class="mt-3">
				<button
					onclick={resetError}
					class="rounded-md bg-red-100 px-3 py-1 text-xs font-medium text-red-800 hover:bg-red-200"
				>
					{$_('try_again')}
				</button>
			</div>
		</div>
	{/if}
{:else}
	{@render children()}
{/if}

<style>
	.error-boundary {
		margin: 1rem 0;
	}
</style>
