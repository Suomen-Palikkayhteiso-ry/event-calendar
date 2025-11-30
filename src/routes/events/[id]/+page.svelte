<script lang="ts">
	/* eslint svelte/no-navigation-without-resolve: "off" -- external URLs don't need resolve() */
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { _ } from 'svelte-i18n';
	import { formatDateInHelsinki } from '$lib/date-utils';
	import { user } from '$lib/auth';
	import { toast } from '@zerodevx/svelte-toast';
	import { eventsStore } from '$lib/stores/events';

	let event: Event;
	let isDeleting = false;

	// Check authentication
	// $: if (!$user) {
	// 	goto(resolve('/'));
	// }

	onMount(() => {
		// if (!$user) return;

		const eventId = $page.params.id;
		if (!eventId) return;

		eventsStore.getEventById(eventId).then((loadedEvent) => {
			event = loadedEvent;
		}).catch(() => {
			goto(resolve('/events'));
		});

		// Add key listeners
		const handleKeydown = (e: KeyboardEvent) => {
			if (e.key === 'Escape') {
				goto(resolve('/'));
			} else if ((e.key === 'e' || e.key === 'E') && event && $user) {
				goto(resolve(`/events/${event.id}/edit`));
			}
		};
		document.addEventListener('keydown', handleKeydown);

		return () => {
			document.removeEventListener('keydown', handleKeydown);
		};
	});

	async function deleteEvent() {
		if (!$user || !event) return;

		isDeleting = true;
		try {
			await eventsStore.deleteEvent(event.id);
			toast.push($_('event_deleted_successfully'));
			goto(resolve('/'));
		} catch (error) {
			toast.push($_('failed_delete_event'));
		} finally {
			isDeleting = false;
		}
	}
</script>

{#if event}
	<div class="my-8 flex items-start justify-between">
		<h1 class="m-0 flex-1">{event.title}</h1>

		<div class="flex gap-2">
			{#if $user}
				<button
					class="cursor-pointer rounded border-none bg-gray-600 px-6 py-3 text-base text-white transition-colors hover:bg-gray-700 disabled:cursor-not-allowed disabled:bg-gray-400"
					on:click={() => goto(resolve(`/events/${event.id}/edit`))}>{$_('edit')}</button
				>
				<button
					class="cursor-pointer rounded border-none bg-red-600 px-6 py-3 text-base text-white transition-colors hover:bg-red-700 disabled:cursor-not-allowed disabled:bg-gray-400"
					on:click={() => {
						if (confirm($_('confirm_delete_event'))) {
							deleteEvent();
						}
					}}
					disabled={isDeleting}
				>
					{isDeleting ? $_('deleting') : $_('delete')}
				</button>
			{/if}
		</div>
	</div>

	<div class="mb-8 rounded-lg bg-white p-8">
		{#if event.description}
			<p class="my-3 text-xl leading-relaxed">{event.description}</p>
		{/if}
		{#if event.location}
			<p class="my-3 leading-relaxed">
				<strong class="text-gray-900">{$_('location')}</strong>
				{#if event.point && event.point.lat !== 0 && event.point.lon !== 0}
					<a
						href="https://www.openstreetmap.org/?mlat={event.point.lat}&mlon={event.point
							.lon}&zoom=15"
						target="_blank"
						rel="noopener noreferrer"
						class="text-brand-primary hover:underline">{event.location}</a
					>
				{:else}
					{event.location}
				{/if}
			</p>
		{/if}
		<p class="my-3 leading-relaxed">
			<strong class="text-gray-900">{$_('start')}</strong>
			{formatDateInHelsinki(event.start_date, event.all_day)}
		</p>
		{#if event.end_date && event.end_date !== event.start_date}
			<p class="my-3 leading-relaxed">
				<strong class="text-gray-900">{$_('end')}</strong>
				{formatDateInHelsinki(event.end_date, event.all_day)}
			</p>
		{/if}
		{#if event.url}
			<p class="my-3 leading-relaxed">
				<strong class="text-gray-900">{$_('url_label')}</strong>
				<a
					href={event.url}
					target="_blank"
					rel="noopener noreferrer"
					class="text-brand-primary hover:underline">{event.url}</a
				>
			</p>
		{/if}
		{#if event.image}
			<img
				src={pb.files.getUrl(event, event.image)}
				alt={event.image_description || event.title}
				class="my-3 h-auto max-w-full rounded-lg shadow-lg"
			/>
			{#if event.image_description}
				<p class="my-2 text-sm text-gray-600 italic">{event.image_description}</p>
			{/if}
		{/if}
	</div>

	<div class="flex gap-2">
		<button
			class="cursor-pointer rounded border-none bg-gray-600 px-6 py-3 text-base text-white transition-colors hover:bg-gray-700 disabled:cursor-not-allowed disabled:bg-gray-400"
			on:click={() => goto(resolve('/'))}>{$_('back_to_calendar_detail')}</button
		>
	</div>
{:else}
	<p>{$_('loading_event')}</p>
{/if}
