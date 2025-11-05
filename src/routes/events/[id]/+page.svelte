<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { _ } from 'svelte-i18n';
	import { formatDateInHelsinki } from '$lib/date-utils';
	import { user } from '$lib/auth';

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

		// Load event
		pb.collection('events')
			.getOne(eventId)
			.then((loadedEvent) => {
				event = loadedEvent as unknown as Event;
			})
			.catch(() => {
				goto(resolve('/events'));
			});

		// Add ESC key listener
		const handleKeydown = (event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				goto(resolve('/events'));
			}
		};
		document.addEventListener('keydown', handleKeydown);

		return () => {
			document.removeEventListener('keydown', handleKeydown);
		};
	});

	async function deleteEvent() {
		if (!$user || !event) return;

		if (!confirm('Are you sure you want to delete this event? This action cannot be undone.')) {
			return;
		}

		isDeleting = true;
		try {
			await pb.collection('events').delete(event.id);
			goto(resolve('/events'));
		} catch (error) {
			console.error('Error deleting event:', error);
			alert('Failed to delete event. Please try again.');
		} finally {
			isDeleting = false;
		}
	}
</script>

{#if event}
	<div class="event-header">
		<h1>{event.title}</h1>

		<div class="event-actions">
			{#if $user}
				<button class="btn-secondary" on:click={() => goto(resolve(`/events/${event.id}/edit`))}
					>Edit</button
				>
				<button class="btn-danger" on:click={deleteEvent} disabled={isDeleting}>
					{isDeleting ? 'Deleting...' : 'Delete'}
				</button>
			{/if}
		</div>
	</div>

	<div class="event-details">
		{#if event.description}
			<p class="event-description">{event.description}</p>
		{/if}
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
		{#if event.image}
			<img src={pb.files.getUrl(event, event.image)} alt={event.image_description || event.title} class="event-image" />
		{/if}
		{#if event.image_description}
			<p>{event.image_description}</p>
		{/if}
	</div>

	<div class="navigation">
		<button class="btn-secondary" on:click={() => goto(resolve('/'))}>Back to Calendar</button>
	</div>
{:else}
	<p>Loading event...</p>
{/if}

<style>
	.event-header {
		display: flex;
		justify-content: space-between;
		align-items: flex-start;
		margin: 2rem 0;
	}

	.event-header h1 {
		margin: 0;
		flex: 1;
	}

	.event-actions {
		display: flex;
		gap: 0.5rem;
	}

	.btn-secondary {
		background-color: #6c757d;
		color: white;
		border: none;
		padding: 0.75rem 1.5rem;
		border-radius: 4px;
		font-size: 1rem;
		cursor: pointer;
		transition: background-color 0.2s;
	}

	.btn-secondary:hover:not(:disabled) {
		background-color: #545b62;
	}

	.btn-secondary:disabled {
		background-color: #ccc;
		cursor: not-allowed;
	}

	.btn-danger {
		background-color: #dc3545;
		color: white;
		border: none;
		padding: 0.75rem 1.5rem;
		border-radius: 4px;
		font-size: 1rem;
		cursor: pointer;
		transition: background-color 0.2s;
	}

	.btn-danger:hover:not(:disabled) {
		background-color: #c82333;
	}

	.btn-danger:disabled {
		background-color: #ccc;
		cursor: not-allowed;
	}

	.event-details {
		background: white;
		padding: 2rem;
		border-radius: 8px;
		margin-bottom: 2rem;
	}

	.event-details > * {
		margin: 0.75rem 0;
	}

	.event-details > *:first-child {
		margin-top: 0;
	}

	.event-details > *:last-child {
		margin-bottom: 0;
	}

	.event-details p {
		line-height: 1.4;
	}

	.event-details strong {
		color: #333;
	}

	.event-description {
		font-size: 1.2em;
		line-height: 1.5;
	}

	.event-image {
		max-width: 100%;
		height: auto;
		border-radius: 8px;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
	}

	.navigation {
		display: flex;
		gap: 0.5rem;
	}
</style>
