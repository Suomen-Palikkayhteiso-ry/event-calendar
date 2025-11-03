<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { _ } from 'svelte-i18n';
	import { formatDateInHelsinki } from '$lib/date-utils';

	let event: Event;

	onMount(() => {
		const eventId = $page.params.id;
		if (!eventId) return;

		// Load event
		pb.collection('events')
			.getOne(eventId)
			.then((loadedEvent) => {
				event = loadedEvent as unknown as Event;
			});

		// Add ESC key listener
		const handleKeydown = (event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				goto('/');
			}
		};
		document.addEventListener('keydown', handleKeydown);

		return () => {
			document.removeEventListener('keydown', handleKeydown);
		};
	});
</script>

{#if event}
	<h1>{event.title}</h1>
	{#if event.location}
		<p><strong>{$_('location')}</strong> {event.location}</p>
	{/if}
	<p>
		<strong>{$_('start')}</strong>
		{formatDateInHelsinki(event.start_date, event.all_day)}
	</p>
	{#if event.end_date && event.end_date !== event.start_date}
		<p>
			<strong>{$_('end')}</strong>
			{formatDateInHelsinki(event.end_date, event.all_day)}
		</p>
	{/if}
	{#if event.all_day}
		<p><em>{$_('all_day_event')}</em></p>
	{/if}
	{#if event.description}
		<p><strong>{$_('description')}</strong></p>
		<p>{event.description}</p>
	{/if}
	<button
		class="mt-4 rounded px-4 py-2 text-white transition-opacity hover:opacity-90"
		style="background-color: var(--color-theme); cursor: pointer;"
		on:click={() => goto('/')}>{$_('back_to_calendar')}</button
	>
{:else}
	<p>Loading event...</p>
{/if}
