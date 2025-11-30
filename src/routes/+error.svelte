<script lang="ts">
	// eslint-disable-next-line svelte/valid-prop-names-in-kit-pages
	let { status, error } = $props();

	function goHome() {
		// eslint-disable-next-line svelte/no-navigation-without-resolve
		goto('/');
	}
</script>

<svelte:head>
	<title>{status} - {$_('error_page_title')}</title>
</svelte:head>

<div class="error-page flex min-h-[50vh] flex-col items-center justify-center text-center">
	<div class="mb-8">
		<svg
			class="mx-auto h-24 w-24 text-red-500"
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
	</div>

	<h1 class="mb-4 text-4xl font-bold text-gray-900">
		{status === 404 ? $_('page_not_found') : $_('something_went_wrong')}
	</h1>

	<p class="mb-8 max-w-md text-lg text-gray-600">
		{status === 404 ? $_('page_not_found_description') : $_('error_page_description')}
	</p>

	{#if error?.message}
		<div class="mb-8 rounded-lg bg-red-50 p-4 text-left">
			<p class="text-sm text-red-800">
				<strong>{$_('error_details')}:</strong>
				{error.message}
			</p>
		</div>
	{/if}

	<div class="flex gap-4">
		<button
			onclick={goHome}
			class="hover:bg-brand-primary-dark rounded-lg bg-brand-primary px-6 py-2 text-white transition-colors"
		>
			{$_('back_to_calendar')}
		</button>

		<button
			onclick={() => window.location.reload()}
			class="rounded-lg border border-gray-300 bg-white px-6 py-2 text-gray-700 transition-colors hover:bg-gray-50"
		>
			{$_('reload_page')}
		</button>
	</div>
</div>

<style>
	.error-page {
		margin: 2rem auto;
		max-width: 600px;
	}
</style>
