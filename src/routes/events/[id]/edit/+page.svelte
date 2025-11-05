<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { localDateToUTC, parseUTCDate, localDateTimeToUTC, utcToHelsinkiDateTimeLocal, utcToHelsinkiDate } from '$lib/date-utils';
	import { user } from '$lib/auth';
	import { Datepicker, Timepicker } from 'flowbite-svelte';

	let event = $state<Event | null>(null);
	let isSubmitting = $state(false);

	// Edit form data object
	let formData = $state({
		title: '',
		start_date: '',
		end_date: '',
		all_day: false,
		location: '',
		description: '',
		image: null as File | null,
		image_description: '',
		state: 'submitted' as 'submitted' | 'published'
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
		if (!event) return; // Don't run until event is loaded

		if (formData.all_day) {
			// For all-day events, force end date to match start date
			endDateObj = new Date(startDateObj);
		}
		// For timed events, allow different end dates
	});

	$effect(() => {
		if (!event) return; // Don't run until event is loaded

		// Update form data when Date objects change
		formData.start_date = formatDateTimeForAPI(startDateObj, startTimeObj);
		formData.end_date = formatDateTimeForAPI(endDateObj, endTimeObj);
	});

	// Sync time strings back to Date objects
	$effect(() => {
		if (!event) return; // Don't run until event is loaded

		const [hours, minutes] = startTimeString.split(':').map(Number);
		startTimeObj.setHours(hours, minutes);
	});

	$effect(() => {
		if (!event) return; // Don't run until event is loaded

		const [hours, minutes] = endTimeString.split(':').map(Number);
		endTimeObj.setHours(hours, minutes);
	});

	// Check authentication
	// $: if (!$user) {
	// 	goto(resolve('/'));
	// }

	onMount(() => {
		// if (!$user) return;

		const eventId = $page.params.id;
		console.log('Edit form: Event ID:', eventId);
		if (!eventId) {
			console.log('Edit form: No event ID found');
			return;
		}

		// Load event
		console.log('Edit form: Loading event from PocketBase...');
		pb.collection('events')
			.getOne(eventId)
			.then((loadedEvent) => {
				console.log('Edit form: Event loaded successfully:', loadedEvent);
				event = loadedEvent as unknown as Event;
				console.log('Edit form: Event assigned, calling initializeEditForm');
				initializeEditForm();
			})
			.catch((error) => {
				console.error('Edit form: Error loading event:', error);
				goto(resolve('/events'));
			});

		// Add ESC key listener
		const handleKeydown = (event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				cancelEdit();
			}
		};
		document.addEventListener('keydown', handleKeydown);

		return () => {
			document.removeEventListener('keydown', handleKeydown);
		};
	});

	function initializeEditForm() {
		console.log('Edit form: initializeEditForm called, event:', event);
		if (!event) {
			console.log('Edit form: No event to initialize');
			return;
		}
		
		try {
			console.log('Edit form: Initializing form data...');
			formData.title = event.title;
			formData.location = event.location || '';
			formData.description = event.description || '';
			formData.image = null; // Don't pre-populate file input
			formData.image_description = event.image_description || '';
			
			// Set Date objects for components
			console.log('Edit form: Parsing dates...');
			const startDateTime = event.all_day 
				? parseUTCDate(event.start_date)
				: parseUTCDate(event.start_date);
			console.log('Edit form: Start date time:', startDateTime);
			startDateObj = new Date(startDateTime.getFullYear(), startDateTime.getMonth(), startDateTime.getDate());
			startTimeObj = new Date(1970, 0, 1, startDateTime.getHours(), startDateTime.getMinutes());
			
			if (event.end_date) {
				const endDateTime = event.all_day 
					? parseUTCDate(event.end_date)
					: parseUTCDate(event.end_date);
				console.log('Edit form: End date time:', endDateTime);
				endDateObj = new Date(endDateTime.getFullYear(), endDateTime.getMonth(), endDateTime.getDate());
				endTimeObj = new Date(1970, 0, 1, endDateTime.getHours(), endDateTime.getMinutes());
			}
			
			formData.all_day = event.all_day;
			formData.state = event.state;
			console.log('Edit form: Initialization complete, formData:', formData);
		} catch (error) {
			console.error('Edit form: Error in initializeEditForm:', error);
		}
	}

	function cancelEdit() {
		if (event) {
			goto(resolve(`/events/${event.id}`));
		} else {
			goto(resolve('/events'));
		}
	}

	async function saveEdit() {
		if (!$user || !event) return;

		isSubmitting = true;
		try {
			const submitData = new FormData();
			submitData.append('title', formData.title);
			if (formData.location) submitData.append('location', formData.location);
			if (formData.description) submitData.append('description', formData.description);
			if (formData.image) submitData.append('image', formData.image);
			if (formData.image_description) submitData.append('image_description', formData.image_description);
			submitData.append('start_date', formData.all_day ? localDateToUTC(formData.start_date) : localDateTimeToUTC(formData.start_date));
			submitData.append('end_date', formData.end_date ? (formData.all_day ? localDateToUTC(formData.end_date) : localDateTimeToUTC(formData.end_date)) : (formData.all_day ? localDateToUTC(formData.start_date) : localDateTimeToUTC(formData.start_date)));
			submitData.append('all_day', formData.all_day.toString());
			submitData.append('state', formData.state);

			await pb.collection('events').update(event.id, submitData);

			// Redirect back to view
			goto(resolve(`/events/${event.id}`));
		} catch (error) {
			console.error('Error updating event:', error);
			alert('Failed to update event. Please try again.');
		} finally {
			isSubmitting = false;
		}
	}
</script>

{#if event}
	<div class="event-header">
		<h1>Edit Event</h1>
	</div>

	<div class="edit-form">
		<form on:submit|preventDefault={saveEdit}>
			<div class="form-group">
				<label for="editTitle">Title *</label>
				<input type="text" id="editTitle" bind:value={formData.title} required disabled={isSubmitting} />
			</div>

			<div class="form-group">
				<label for="editLocation">Location</label>
				<input type="text" id="editLocation" bind:value={formData.location} disabled={isSubmitting} />
			</div>

			<div class="form-group">
				<label for="editDescription">Description</label>
				<textarea id="editDescription" bind:value={formData.description} rows="3" disabled={isSubmitting}
				></textarea>
			</div>

			<div class="form-group">
				<label for="editImage">Image</label>
				<input
					type="file"
					id="editImage"
					accept="image/*"
					disabled={isSubmitting}
					on:change={(e) => {
						const target = e.target as HTMLInputElement;
						formData.image = target.files?.[0] || null;
					}}
				/>
				{#if event.image}
					<p class="current-image">Current image: {event.image}</p>
				{/if}
			</div>

			<div class="form-group">
				<label for="editImageDescription">Image Description</label>
				<input
					type="text"
					id="editImageDescription"
					bind:value={formData.image_description}
					placeholder="Image description (optional)"
					disabled={isSubmitting}
				/>
			</div>

			<div class="form-row">
				<div class="form-group">
					<label for="editStartDate">Start Date *</label>
					<Datepicker 
						id="editStartDate" 
						bind:value={startDateObj} 
						locale="fi" 
						firstDayOfWeek={1}
						disabled={isSubmitting}
					/>
					{#if !formData.all_day}
						<Timepicker 
							id="editStartTime"
							bind:value={startTimeString}
							disabled={isSubmitting}
						/>
					{/if}
				</div>

				<div class="form-group">
					<label for="editEndDate">End Date</label>
					<Datepicker 
						id="editEndDate" 
						bind:value={endDateObj} 
						locale="fi" 
						firstDayOfWeek={1}
						disabled={isSubmitting || formData.all_day} 
					/>
					{#if !formData.all_day}
						<Timepicker 
							id="editEndTime"
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

			<div class="form-group">
				<label for="editState">Status</label>
				<select id="editState" bind:value={formData.state} disabled={isSubmitting}>
					<option value="submitted">Submitted</option>
					<option value="published">Published</option>
				</select>
			</div>

			<div class="form-actions">
				<button
					type="submit"
					class="save-btn"
					disabled={isSubmitting || !formData.title || !formData.start_date}
				>
					{isSubmitting ? 'Saving...' : 'Save Changes'}
				</button>
				<button type="button" class="btn-secondary" on:click={cancelEdit} disabled={isSubmitting}>
					Cancel
				</button>
			</div>
		</form>
	</div>
{:else}
	<p>Loading event...</p>
{/if}

<style>
	.event-header {
		display: flex;
		justify-content: space-between;
		align-items: flex-start;
	}

	.event-header h1 {
		margin: 1rem 0 3rem 0;
		flex: 1;
	}

	.save-btn {
		background-color: #0056a3;
		color: white;
		border: none;
		padding: 0.75rem 1.5rem;
		border-radius: 4px;
		font-size: 1rem;
		cursor: pointer;
		transition: background-color 0.2s;
	}

	.save-btn:hover:not(:disabled) {
		background-color: #004080;
	}

	.save-btn:disabled {
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

	.edit-form {
		background: #f8f9fa;
		padding: 2rem;
		border-radius: 8px;
		margin-bottom: 2rem;
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
	input[type='datetime-local'],
	input[type='time'],
	textarea,
	select {
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
	textarea:focus,
	select:focus {
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

	.current-image {
		margin: 0.5rem 0 0 0;
		font-size: 0.9rem;
		color: #666;
		font-style: italic;
	}

	.form-actions {
		display: flex;
		gap: 0.5rem;
		margin-top: 1.5rem;
	}
</style>
