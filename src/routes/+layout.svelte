<script lang="ts">
	import Header from './Header.svelte';
	import '../app.css';
	import 'flowbite/dist/flowbite.min.css';
	import '../lib/i18n';
	import { user } from '$lib/auth';

	let { children } = $props();
</script>

<div class="app">
	<Header {user} />

	<main>
		{@render children()}
	</main>

	<footer>
		<div class="footer-content">
			<p>
				<a href="/feed.rss" target="_blank">RSS Feed</a> |
				<a href="/feed.ical" target="_blank">iCal Feed</a>
			</p>
		</div>
	</footer>
</div>

<style>
	.app {
		display: flex;
		flex-direction: column;
		min-height: 100vh;
	}

	main {
		flex: 1;
		display: flex;
		flex-direction: column;
		padding: 1rem;
		width: 100%;
		max-width: 64rem;
		margin: 0 auto;
		box-sizing: border-box;
	}

	footer {
		background-color: #f8f9fa;
		border-top: 1px solid #e9ecef;
		padding: 1rem;
		text-align: center;
		margin-top: auto;
	}

	.footer-content a {
		color: var(--color-theme);
		text-decoration: none;
		margin: 0 0.5rem;
	}

	.footer-content a:hover {
		text-decoration: underline;
	}
</style>
