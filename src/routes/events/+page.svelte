<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	import { formatDateInHelsinki, localDateToUTC } from '$lib/date-utils';
	import { user } from '$lib/auth';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';

	let events: Event[] = [];
	let newEventTitle = '';
	let newEventStartDate = '';
	let newEventEndDate = '';
	let newEventAllDay = false;
	let newEventLocation = '';
	let newEventDescription = '';
	let isSubmitting = false;

	// Check authentication
	$: if (!$user) {
		goto(resolve('/'));
	}

	onMount(async () => {
		if (!$user) return;

		events = await pb.collection('events').getFullList({
			sort: 'start_date'
		});
	});

	async function createEvent() {
		if (!$user) return;

		isSubmitting = true;
		try {
			await pb.collection('events').create({
				title: newEventTitle,
				location: newEventLocation || undefined,
				description: newEventDescription || undefined,
				start_date: localDateToUTC(newEventStartDate),
				end_date: newEventEndDate
					? localDateToUTC(newEventEndDate)
					: localDateToUTC(newEventStartDate),
				all_day: newEventAllDay,
				state: 'submitted' // Default state for new events
			});

			// Reset form
			newEventTitle = '';
			newEventLocation = '';
			newEventDescription = '';
			newEventStartDate = '';
			newEventEndDate = '';
			newEventAllDay = false;

			// Refresh events list
			events = await pb.collection('events').getFullList({
				sort: 'start_date'
			});
		} catch (error) {
			console.error('Error creating event:', error);
			alert('Failed to create event. Please try again.');
		} finally {
			isSubmitting = false;
		}
	}
</script>

<h1>Manage Events</h1>

{#if !$user}
	<p>You must be logged in to manage events.</p>
{:else}
	<div class="create-form">
		<h2>Create New Event</h2>
		<form on:submit|preventDefault={createEvent}>
			<div class="form-group">
				<label for="title">Title *</label>
				<input
					type="text"
					id="title"
					bind:value={newEventTitle}
					placeholder="Event Title"
					required
					disabled={isSubmitting}
				/>
			</div>

			<div class="form-group">
				<label for="location">Location</label>
				<input
					type="text"
					id="location"
					bind:value={newEventLocation}
					placeholder="Location (optional)"
					disabled={isSubmitting}
				/>
			</div>

			<div class="form-group">
				<label for="description">Description</label>
				<textarea
					id="description"
					bind:value={newEventDescription}
					placeholder="Description (optional)"
					rows="3"
					disabled={isSubmitting}
				></textarea>
			</div>

			<div class="form-row">
				<div class="form-group">
					<label for="startDate">Start Date *</label>
					<input
						type="date"
						id="startDate"
						bind:value={newEventStartDate}
						required
						disabled={isSubmitting}
					/>
				</div>

				<div class="form-group">
					<label for="endDate">End Date</label>
					<input type="date" id="endDate" bind:value={newEventEndDate} disabled={isSubmitting} />
				</div>
			</div>

			<div class="form-group">
				<label class="checkbox-label">
					<input type="checkbox" bind:checked={newEventAllDay} disabled={isSubmitting} />
					All Day Event
				</label>
			</div>

			<button type="submit" disabled={isSubmitting || !newEventTitle || !newEventStartDate}>
				{isSubmitting ? 'Creating...' : 'Create Event'}
			</button>
		</form>
	</div>

	<div class="events-list">
		<h2>Existing Events</h2>
		{#each events as event (event.id)}
			<div class="event-item">
				<div class="event-header">
					<h3>{event.location ? `${event.title} / ${event.location}` : event.title}</h3>
					<div class="event-actions">
						<a href={resolve(`/events/${event.id}`)} class="edit-link">Edit</a>
					</div>
				</div>
				{#if event.description}
					<p class="event-description">{event.description}</p>
				{/if}
				<p class="event-date">
					{formatDateInHelsinki(event.start_date, event.all_day)}
					{#if event.end_date && event.end_date !== event.start_date}
						- {formatDateInHelsinki(event.end_date, event.all_day)}
					{/if}
					{event.all_day ? '(All Day)' : ''}
				</p>
				<p class="event-status">Status: {event.state}</p>
			</div>
		{/each}
	</div>
{/if}

<style>
	.create-form {
		background: #f8f9fa;
		padding: 2rem;
		border-radius: 8px;
		margin-bottom: 2rem;
	}

	.create-form h2 {
		margin-top: 0;
		margin-bottom: 1.5rem;
		color: #333;
	}

	.form-group {
		margin-bottom: 1rem;
	}

	.form-row {
		display: flex;
		gap: 1rem;
	}

	.form-row .form-group {
		flex: 1;
	}

	label {
		display: block;
		margin-bottom: 0.5rem;
		font-weight: 500;
		color: #555;
	}

	input[type='text'],
	input[type='date'],
	textarea {
		width: 100%;
		padding: 0.75rem;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 1rem;
		box-sizing: border-box;
	}

	input[type='text']:focus,
	input[type='date']:focus,
	textarea:focus {
		outline: none;
		border-color: var(--color-theme);
		box-shadow: 0 0 0 2px rgba(0, 123, 255, 0.25);
	}

	.checkbox-label {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		font-weight: normal;
		cursor: pointer;
	}

	button[type='submit'] {
		background-color: var(--color-theme);
		color: white;
		border: none;
		padding: 0.75rem 1.5rem;
		border-radius: 4px;
		font-size: 1rem;
		cursor: pointer;
		transition: background-color 0.2s;
	}

	button[type='submit']:hover:not(:disabled) {
		background-color: #004080;
	}

	button[type='submit']:disabled {
		background-color: #ccc;
		cursor: not-allowed;
	}

	.events-list h2 {
		margin-bottom: 1rem;
		color: #333;
	}

	.event-item {
		border: 1px solid #ddd;
		border-radius: 8px;
		padding: 1.5rem;
		margin-bottom: 1rem;
		background: white;
	}

	.event-header {
		display: flex;
		justify-content: space-between;
		align-items: flex-start;
		margin-bottom: 0.5rem;
	}

	.event-header h3 {
		margin: 0;
		color: #333;
		flex: 1;
	}

	.event-actions {
		margin-left: 1rem;
	}

	.edit-link {
		color: var(--color-theme);
		text-decoration: none;
		font-weight: 500;
		padding: 0.25rem 0.5rem;
		border-radius: 4px;
		transition: background-color 0.2s;
	}

	.edit-link:hover {
		background-color: rgba(0, 123, 255, 0.1);
	}

	.event-description {
		color: #666;
		margin: 0.5rem 0;
		line-height: 1.4;
	}

	.event-date {
		color: #888;
		font-size: 0.9rem;
		margin: 0.25rem 0;
	}

	.event-status {
		color: #666;
		font-size: 0.9rem;
		margin: 0.25rem 0;
		font-weight: 500;
	}
</style>
