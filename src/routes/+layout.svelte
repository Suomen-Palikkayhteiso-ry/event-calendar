<script lang="ts">
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

	<footer class="mt-auto border-t border-gray-200 bg-gray-50 p-4 text-center">
		<div class="footer-content">
			<p>
				<!-- eslint-disable-next-line svelte/no-navigation-without-resolve --><!--These are external links, so resolve is not needed.-->
				<a
					href="/kalenteri.html"
					target="_blank"
					class="mx-2 text-brand-primary no-underline hover:underline">{$_('html_feed')}</a
				>
				|
				<!-- eslint-disable-next-line svelte/no-navigation-without-resolve --><!--These are external links, so resolve is not needed.-->
				<a
					href={`webcal://${window ? window.location.host : ''}/kalenteri.ics`}
					target="_blank"
					class="mx-2 text-brand-primary no-underline hover:underline">{$_('ical_feed')}</a
				>
				|
				<!-- eslint-disable-next-line svelte/no-navigation-without-resolve --><!--These are external links, so resolve is not needed.-->
				<a
					href="/kalenteri.atom"
					target="_blank"
					class="mx-2 text-brand-primary no-underline hover:underline">{$_('atom_feed')}</a
				>
				|
				<!-- eslint-disable-next-line svelte/no-navigation-without-resolve --><!--These are external links, so resolve is not needed.-->
				<a
					href="/kalenteri.rss"
					target="_blank"
					class="mx-2 text-brand-primary no-underline hover:underline">{$_('rss_feed')}</a
				>
				|
				<!-- eslint-disable-next-line svelte/no-navigation-without-resolve --><!--These are external links, so resolve is not needed.-->
				<a
					href="/kalenteri.json"
					target="_blank"
					class="mx-2 text-brand-primary no-underline hover:underline">{$_('json_feed')}</a
				>
			</p>
		</div>
	</footer>
</div>
