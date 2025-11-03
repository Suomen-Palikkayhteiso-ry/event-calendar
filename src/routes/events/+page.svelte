<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	import { formatDateInHelsinki, localDateToUTC } from '$lib/date-utils';

	let events: Event[] = [];
	let newEventTitle = '';
	let newEventStartDate = '';
	let newEventEndDate = '';
	let newEventAllDay = false;
	let newEventLocation = '';
	let newEventDescription = '';

	onMount(async () => {
		events = await pb.collection('events').getFullList({
			sort: 'start_date'
		});
	});

	async function createEvent() {
		await pb.collection('events').create({
			title: newEventTitle,
			location: newEventLocation || undefined,
			description: newEventDescription || undefined,
			start_date: localDateToUTC(newEventStartDate),
			end_date: newEventEndDate ? localDateToUTC(newEventEndDate) : localDateToUTC(newEventStartDate),
			all_day: newEventAllDay
		});
		newEventTitle = '';
		newEventLocation = '';
		newEventDescription = '';
		newEventStartDate = '';
		newEventEndDate = '';
		newEventAllDay = false;
		events = await pb.collection('events').getFullList({
			sort: 'start_date'
		});
	}
</script>

<h1>Events</h1>

<form on:submit|preventDefault={createEvent}>
	<input type="text" bind:value={newEventTitle} placeholder="Event Title" required />
	<input type="text" bind:value={newEventLocation} placeholder="Location (optional)" />
	<textarea bind:value={newEventDescription} placeholder="Description (optional)" rows="3"
	></textarea>
	<input type="date" bind:value={newEventStartDate} placeholder="Start Date" required />
	<input type="date" bind:value={newEventEndDate} placeholder="End Date" />
	<label><input type="checkbox" bind:checked={newEventAllDay} /> All Day</label>
	<button type="submit">Add Event</button>
</form>

{#each events as event}
	<div>
		<h2>{event.location ? `${event.title} / ${event.location}` : event.title}</h2>
		{#if event.description}
			<p>{event.description}</p>
		{/if}
		<p>{formatDateInHelsinki(event.start_date, event.all_day)} {#if event.end_date && event.end_date !== event.start_date}- {formatDateInHelsinki(event.end_date, event.all_day)}{/if} {event.all_day ? '(All Day)' : ''}</p>
		<a href="/events/{event.id}">View/Edit</a>
	</div>
{/each}
