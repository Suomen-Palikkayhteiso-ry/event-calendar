<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	import { formatDateInHelsinki, localDateToUTC, localDateTimeToUTC } from '$lib/date-utils';
	import { user } from '$lib/auth';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { Datepicker, Timepicker } from 'flowbite-svelte';

	let events: Event[] = [];
	let currentPage = 1;
	let pageSize = 10;
	let isSubmitting = false;

	// Form data object
	let formData = $state({
		title: '',
		start_date: '',
		end_date: '',
		all_day: true,
		location: '',
		description: '',
		image: null as File | null,
		image_description: ''
	});

	// Date/Time picker values (Date objects for components)
	let startDateObj = $state(new Date());
	let startTimeObj = $state(new Date(1970, 0, 1, 9, 0)); // Default to 9:00 AM
	let endDateObj = $state(new Date());
	let endTimeObj = $state(new Date(1970, 0, 1, 17, 0)); // Default to 5:00 PM

	// String values for Timepicker components
	let startTimeString = $derived(
		String(startTimeObj.getHours()).padStart(2, '0') + ':' + 
		String(startTimeObj.getMinutes()).padStart(2, '0')
	);
	let endTimeString = $derived(
		String(endTimeObj.getHours()).padStart(2, '0') + ':' + 
		String(endTimeObj.getMinutes()).padStart(2, '0')
	);

	// Helper function to format Date objects for API
	function formatDateTimeForAPI(dateObj: Date, timeObj: Date): string {
		const date = dateObj.toISOString().split('T')[0];
		const time = String(timeObj.getHours()).padStart(2, '0') + ':' + 
		             String(timeObj.getMinutes()).padStart(2, '0');
		return date + 'T' + time;
	}

	// Reactive statements for date/time synchronization
	$effect(() => {
		// For timed events (not all-day), force end date to match start date
		if (!formData.all_day) {
			endDateObj = new Date(startDateObj);
		}
		// For all-day events, allow different end dates
	});

	$effect(() => {
		// Update form data when Date objects change
		formData.start_date = formatDateTimeForAPI(startDateObj, startTimeObj);
		formData.end_date = formatDateTimeForAPI(endDateObj, endTimeObj);
	});

	// Sync time strings back to Date objects
	$effect(() => {
		const [hours, minutes] = startTimeString.split(':').map(Number);
		startTimeObj.setHours(hours, minutes);
	});

	$effect(() => {
		const [hours, minutes] = endTimeString.split(':').map(Number);
		endTimeObj.setHours(hours, minutes);
	});

	// Sync end date with start date when all day is enabled
	// Removed: Allow different days for all-day events

	let totalEvents = 0;

	// Check authentication
	$effect(() => {
		if (!$user) {
			goto(resolve('/'));
		}
	});

	async function fetchEvents(page = 1) {
		if (!$user) return;

		const result = await pb.collection('events').getList(page, pageSize, {
			sort: 'start_date',
			filter: '' // Add any filters if needed
		});

		events = result.items as unknown as Event[];
		totalEvents = result.totalItems;
		currentPage = page;
	}

	onMount(async () => {
		await fetchEvents();
	});	async function createEvent() {
		if (!$user) return;

		isSubmitting = true;
		try {
			const submitData = new FormData();
			submitData.append('title', formData.title);
			if (formData.location) submitData.append('location', formData.location);
			if (formData.description) submitData.append('description', formData.description);
			submitData.append('start_date', formData.all_day ? localDateToUTC(formData.start_date) : localDateTimeToUTC(formData.start_date));
			submitData.append('end_date', formData.end_date ? (formData.all_day ? localDateToUTC(formData.end_date) : localDateTimeToUTC(formData.end_date)) : (formData.all_day ? localDateToUTC(formData.start_date) : localDateTimeToUTC(formData.start_date)));
			submitData.append('all_day', formData.all_day.toString());
			if (formData.image) submitData.append('image', formData.image);
			if (formData.image_description) submitData.append('image_description', formData.image_description);
			submitData.append('state', 'submitted');

			await pb.collection('events').create(submitData);

			// Reset form
			formData.title = '';
			formData.location = '';
			formData.description = '';
			formData.image = null;
			formData.image_description = '';
			// Reset Date objects to defaults
			startDateObj = new Date();
			startTimeObj = new Date(1970, 0, 1, 9, 0);
			endDateObj = new Date();
			endTimeObj = new Date(1970, 0, 1, 17, 0);

			// Refresh events list
			await fetchEvents(currentPage);
		} catch (error) {
			console.error('Error creating event:', error);
			alert('Failed to create event. Please try again.');
		} finally {
			isSubmitting = false;
		}
	}

	function cancelAdd() {
		goto('/');
	}

	async function goToPage(page: number) {
		if (page >= 1 && page <= Math.ceil(totalEvents / pageSize)) {
			await fetchEvents(page);
		}
	}

	async function nextPage() {
		const maxPage = Math.ceil(totalEvents / pageSize);
		if (currentPage < maxPage) {
			await fetchEvents(currentPage + 1);
		}
	}

	async function prevPage() {
		if (currentPage > 1) {
			await fetchEvents(currentPage - 1);
		}
	}
</script>

<h1>Add New Event</h1>

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
					bind:value={formData.title}
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
					bind:value={formData.location}
					placeholder="Location (optional)"
					disabled={isSubmitting}
				/>
			</div>

			<div class="form-group">
				<label for="description">Description</label>
				<textarea
					id="description"
					bind:value={formData.description}
					placeholder="Description (optional)"
					rows="3"
					disabled={isSubmitting}
				></textarea>
			</div>

			<div class="form-group">
				<label for="image">Image</label>
				<input
					type="file"
					id="image"
					accept="image/*"
					disabled={isSubmitting}
					on:change={(e) => {
						const target = e.target as HTMLInputElement;
						formData.image = target.files?.[0] || null;
					}}
				/>
			</div>

			<div class="form-group">
				<label for="imageDescription">Image Description</label>
				<input
					type="text"
					id="imageDescription"
					bind:value={formData.image_description}
					placeholder="Image description (optional)"
					disabled={isSubmitting}
				/>
			</div>

			<div class="form-row">
				<div class="form-group">
					<label for="startDate">Start Date *</label>
					<Datepicker 
						id="startDate" 
						bind:value={startDateObj} 
						locale="fi" 
						firstDayOfWeek={1}
						disabled={isSubmitting}
					/>
					{#if !formData.all_day}
						<Timepicker 
							id="startTime"
							bind:value={startTimeString}
							disabled={isSubmitting}
						/>
					{/if}
				</div>

				<div class="form-group">
					<label for="endDate">End Date</label>
					<Datepicker 
						id="endDate" 
						bind:value={endDateObj} 
						locale="fi" 
						firstDayOfWeek={1}
						disabled={isSubmitting || !formData.all_day} 
					/>
					{#if !formData.all_day}
						<Timepicker 
							id="endTime"
							bind:value={endTimeString}
							disabled={isSubmitting}
						/>
					{/if}
				</div>
			</div>

			<div class="form-group">
				<label class="checkbox-label">
					<input type="checkbox" bind:checked={formData.all_day} disabled={isSubmitting} />
					All Day Event
				</label>
			</div>

			<div class="form-actions">
				<button type="submit" disabled={isSubmitting || !formData.title || !formData.start_date}>
					{isSubmitting ? 'Creating...' : 'Create Event'}
				</button>
				<button type="button" class="btn-secondary" on:click={cancelAdd} disabled={isSubmitting}>
					Cancel
				</button>
			</div>
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

		{#if totalEvents > pageSize}
			<div class="pagination">
				<button 
					class="pagination-btn" 
					disabled={currentPage === 1} 
					on:click={prevPage}
				>
					Previous
				</button>
				
				<span class="pagination-info">
					Page {currentPage} of {Math.ceil(totalEvents / pageSize)} 
					({totalEvents} total events)
				</span>
				
				<button 
					class="pagination-btn" 
					disabled={currentPage === Math.ceil(totalEvents / pageSize)} 
					on:click={nextPage}
				>
					Next
				</button>
			</div>
		{/if}
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

	h1 {
		margin-bottom: 1rem 0 2rem 0;
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

	input[type='time'] {
		width: 100%;
		min-width: 140px;
		padding: 0.75rem 2.5rem 0.75rem 0.75rem; /* Extra right padding for clock icon */
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 1rem;
		box-sizing: border-box;
	}

	input[type='text'],
	input[type='date'],
	input[type='datetime-local'],
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
	input[type='datetime-local']:focus,
	input[type='time']:focus,
	textarea:focus {
		outline: none;
		border-color: var(--color-theme);
		box-shadow: 0 0 0 2px rgba(0, 123, 255, 0.25);
	}

	/* Force 24-hour time format for time inputs */
	input[type='time']::-webkit-datetime-edit-ampm-field {
		display: none;
	}
	
	input[type='time']::-webkit-datetime-edit-fields-wrapper {
		padding: 0;
	}

	.checkbox-label {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		font-weight: normal;
		cursor: pointer;
	}

	.form-actions {
		display: flex;
		gap: 0.5rem;
		margin-top: 1.5rem;
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

	.pagination {
		display: flex;
		justify-content: center;
		align-items: center;
		gap: 1rem;
		margin-top: 2rem;
		padding: 1rem;
		border-top: 1px solid #ddd;
	}

	.pagination-btn {
		background-color: var(--color-theme);
		color: white;
		border: none;
		padding: 0.5rem 1rem;
		border-radius: 4px;
		font-size: 0.9rem;
		cursor: pointer;
		transition: background-color 0.2s;
	}

	.pagination-btn:hover:not(:disabled) {
		background-color: #004080;
	}

	.pagination-btn:disabled {
		background-color: #ccc;
		cursor: not-allowed;
	}

	.pagination-info {
		color: #666;
		font-size: 0.9rem;
		font-weight: 500;
	}

	/* Make datepicker current date visible */
	:global(.day.today) {
		background-color: var(--color-theme) !important;
		color: white !important;
		border: 2px solid var(--color-theme) !important;
	}
	:global(.day.today:not(.selected)) {
		background-color: white !important;
		color: var(--color-theme) !important;
	}
</style>
