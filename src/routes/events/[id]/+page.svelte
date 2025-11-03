<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { _ } from 'svelte-i18n';
	import { formatDateInHelsinki, localDateToUTC, parseUTCDate } from '$lib/date-utils';
	import { user } from '$lib/auth';

	let event: Event;
	let isEditing = false;
	let isSubmitting = false;
	let isDeleting = false;
	
	// Edit form fields
	let editTitle = '';
	let editLocation = '';
	let editDescription = '';
	let editStartDate = '';
	let editEndDate = '';
	let editAllDay = false;
	let editState: 'submitted' | 'published' = 'submitted';

	// Check authentication
	$: if (!$user) {
		goto('/');
	}

	onMount(() => {
		if (!$user) return;
		
		const eventId = $page.params.id;
		if (!eventId) return;

		// Load event
		pb.collection('events')
			.getOne(eventId)
			.then((loadedEvent) => {
				event = loadedEvent as unknown as Event;
				initializeEditForm();
			})
			.catch(() => {
				goto('/events');
			});

		// Add ESC key listener
		const handleKeydown = (event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				if (isEditing) {
					cancelEdit();
				} else {
					goto('/events');
				}
			}
		};
		document.addEventListener('keydown', handleKeydown);

		return () => {
			document.removeEventListener('keydown', handleKeydown);
		};
	});

	function initializeEditForm() {
		editTitle = event.title;
		editLocation = event.location || '';
		editDescription = event.description || '';
		editStartDate = parseUTCDate(event.start_date).toISOString().split('T')[0];
		editEndDate = event.end_date ? parseUTCDate(event.end_date).toISOString().split('T')[0] : '';
		editAllDay = event.all_day;
		editState = event.state;
	}

	function startEdit() {
		isEditing = true;
	}

	function cancelEdit() {
		isEditing = false;
		initializeEditForm();
	}

	async function saveEdit() {
		if (!$user || !event) return;
		
		isSubmitting = true;
		try {
			await pb.collection('events').update(event.id, {
				title: editTitle,
				location: editLocation || undefined,
				description: editDescription || undefined,
				start_date: localDateToUTC(editStartDate),
				end_date: editEndDate ? localDateToUTC(editEndDate) : localDateToUTC(editStartDate),
				all_day: editAllDay,
				state: editState
			});
			
			// Reload event
			event = await pb.collection('events').getOne(event.id) as unknown as Event;
			isEditing = false;
		} catch (error) {
			console.error('Error updating event:', error);
			alert('Failed to update event. Please try again.');
		} finally {
			isSubmitting = false;
		}
	}

	async function deleteEvent() {
		if (!$user || !event) return;
		
		if (!confirm('Are you sure you want to delete this event? This action cannot be undone.')) {
			return;
		}
		
		isDeleting = true;
		try {
			await pb.collection('events').delete(event.id);
			goto('/events');
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
		{#if isEditing}
			<h1>Edit Event</h1>
		{:else}
			<h1>{event.title}</h1>
		{/if}
		
		<div class="event-actions">
			{#if !isEditing}
				<button class="btn-secondary" on:click={startEdit}>Edit</button>
				<button class="btn-danger" on:click={deleteEvent} disabled={isDeleting}>
					{isDeleting ? 'Deleting...' : 'Delete'}
				</button>
			{/if}
		</div>
	</div>

	{#if isEditing}
		<div class="edit-form">
			<form on:submit|preventDefault={saveEdit}>
				<div class="form-group">
					<label for="editTitle">Title *</label>
					<input 
						type="text" 
						id="editTitle"
						bind:value={editTitle} 
						required 
						disabled={isSubmitting}
					/>
				</div>
				
				<div class="form-group">
					<label for="editLocation">Location</label>
					<input 
						type="text" 
						id="editLocation"
						bind:value={editLocation} 
						disabled={isSubmitting}
					/>
				</div>
				
				<div class="form-group">
					<label for="editDescription">Description</label>
					<textarea 
						id="editDescription"
						bind:value={editDescription} 
						rows="3"
						disabled={isSubmitting}
					></textarea>
				</div>
				
				<div class="form-row">
					<div class="form-group">
						<label for="editStartDate">Start Date *</label>
						<input 
							type="date" 
							id="editStartDate"
							bind:value={editStartDate} 
							required 
							disabled={isSubmitting}
						/>
					</div>
					
					<div class="form-group">
						<label for="editEndDate">End Date</label>
						<input 
							type="date" 
							id="editEndDate"
							bind:value={editEndDate} 
							disabled={isSubmitting}
						/>
					</div>
				</div>
				
				<div class="form-group">
					<label class="checkbox-label">
						<input 
							type="checkbox" 
							bind:checked={editAllDay} 
							disabled={isSubmitting}
						/>
						All Day Event
					</label>
				</div>
				
				<div class="form-group">
					<label for="editState">Status</label>
					<select id="editState" bind:value={editState} disabled={isSubmitting}>
						<option value="submitted">Submitted</option>
						<option value="published">Published</option>
					</select>
				</div>
				
				<div class="form-actions">
					<button type="submit" class="btn-primary" disabled={isSubmitting || !editTitle || !editStartDate}>
						{isSubmitting ? 'Saving...' : 'Save Changes'}
					</button>
					<button type="button" class="btn-secondary" on:click={cancelEdit} disabled={isSubmitting}>
						Cancel
					</button>
				</div>
			</form>
		</div>
	{:else}
		<div class="event-details">
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
			<p><strong>Status:</strong> {event.state}</p>
		</div>
		
		<div class="navigation">
			<button class="btn-secondary" on:click={() => goto('/events')}>Back to Events</button>
			<button class="btn-secondary" on:click={() => goto('/')}>Back to Calendar</button>
		</div>
	{/if}
{:else}
	<p>Loading event...</p>
{/if}

<style>
	.event-header {
		display: flex;
		justify-content: space-between;
		align-items: flex-start;
		margin-bottom: 2rem;
	}

	.event-header h1 {
		margin: 0;
		flex: 1;
	}

	.event-actions {
		display: flex;
		gap: 0.5rem;
	}

	.btn-primary {
		background-color: var(--color-theme);
		color: white;
		border: none;
		padding: 0.75rem 1.5rem;
		border-radius: 4px;
		font-size: 1rem;
		cursor: pointer;
		transition: background-color 0.2s;
	}

	.btn-primary:hover:not(:disabled) {
		background-color: #004080;
	}

	.btn-primary:disabled {
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

	input[type="text"],
	input[type="date"],
	textarea,
	select {
		width: 100%;
		padding: 0.75rem;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 1rem;
		box-sizing: border-box;
	}

	input[type="text"]:focus,
	input[type="date"]:focus,
	textarea:focus,
	select:focus {
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

	.form-actions {
		display: flex;
		gap: 0.5rem;
		margin-top: 1.5rem;
	}

	.event-details {
		background: white;
		padding: 2rem;
		border-radius: 8px;
		border: 1px solid #ddd;
		margin-bottom: 2rem;
	}

	.event-details p {
		margin: 0.5rem 0;
		line-height: 1.4;
	}

	.event-details strong {
		color: #333;
	}

	.navigation {
		display: flex;
		gap: 0.5rem;
	}
</style>
