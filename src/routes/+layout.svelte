<script lang="ts">
	/* eslint svelte/no-navigation-without-resolve: "off" -- static file links don't need resolve() */
	import Header from './Header.svelte';
	import '../app.css';
	import 'flowbite/dist/flowbite.min.css';
	import '../lib/i18n';
	import { _ } from 'svelte-i18n';
	import { SvelteToast } from '@zerodevx/svelte-toast';

	let { children } = $props();
</script>

<div class="app flex min-h-screen flex-col">
	<Header />

	<main class="mx-auto box-border flex w-full max-w-4xl flex-1 flex-col p-4">
		<SvelteToast />
		{@render children()}
	</main>

	<footer class="mt-auto border-t border-gray-200 bg-gray-50 p-4">
		<div class="footer-content mx-auto grid max-w-4xl grid-cols-1 gap-6 md:grid-cols-3">
			<a
				href={`webcal://${window ? window.location.host : ''}/kalenteri.ics`}
				target="_blank"
				class="block rounded-lg p-4 text-left transition-colors hover:bg-gray-100"
			>
				<div class="mb-3 flex items-center">
					<svg
						class="mr-2 h-8 w-8 text-brand-primary"
						fill="none"
						stroke="currentColor"
						viewBox="0 0 24 24"
					>
						<path
							stroke-linecap="round"
							stroke-linejoin="round"
							stroke-width="2"
							d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"
						></path>
					</svg>
					<h3 class="text-lg font-semibold">{$_('ical_feed')}</h3>
				</div>
				<p class="text-sm text-gray-600">{$_('ical_description')}</p>
			</a>
			<a
				href="/kalenteri.html"
				target="_blank"
				class="block rounded-lg p-4 text-left transition-colors hover:bg-gray-100"
			>
				<div class="mb-3 flex items-center">
					<svg
						class="mr-2 h-8 w-8 text-brand-primary"
						fill="none"
						stroke="currentColor"
						viewBox="0 0 24 24"
					>
						<path
							stroke-linecap="round"
							stroke-linejoin="round"
							stroke-width="2"
							d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"
						></path>
					</svg>
					<h3 class="text-lg font-semibold">{$_('html_feed')}</h3>
				</div>
				<p class="text-sm text-gray-600">{$_('html_description')}</p>
			</a>
			<div class="p-4 text-left">
				<div class="mb-3 flex items-center">
					<svg
						class="mr-2 h-8 w-8 text-brand-primary"
						fill="none"
						stroke="currentColor"
						viewBox="0 0 24 24"
					>
						<path
							stroke-linecap="round"
							stroke-linejoin="round"
							stroke-width="2"
							d="M6 5c7.18 0 13 5.82 13 13M6 11a7 7 0 017 7m-6 0a1 1 0 11-2 0 1 1 0 012 0m6 0a1 1 0 11-2 0 1 1 0 012 0m6 0a1 1 0 11-2 0 1 1 0 012 0"
						></path>
					</svg>
					<h3 class="text-lg font-semibold">Syötteet</h3>
				</div>
				<p class="mb-3 text-sm text-gray-600">{$_('feeds_description')}</p>
				<div class="text-center">
					<a
						href="/kalenteri.atom"
						target="_blank"
						class="mx-1 text-brand-primary no-underline hover:underline">{$_('atom_feed')}</a
					>
					|
					<a
						href="/kalenteri.rss"
						target="_blank"
						class="mx-1 text-brand-primary no-underline hover:underline">{$_('rss_feed')}</a
					>
					|
					<a
						href="/kalenteri.json"
						target="_blank"
						class="mx-1 text-brand-primary no-underline hover:underline">{$_('json_feed')}</a
					>
					|
					<a
						href="/kalenteri.geo.json"
						target="_blank"
						class="mx-1 text-brand-primary no-underline hover:underline">{$_('geojson_feed')}</a
					>
				</div>
			</div>
		</div>
	</footer>
</div>
