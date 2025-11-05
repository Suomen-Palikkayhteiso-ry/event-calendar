<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event } from '$lib/types';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { localDateToUTC, parseUTCDate } from '$lib/date-utils';
	import { user } from '$lib/auth';

	let event: Event;
	let isSubmitting = false;

	// Edit form fields
	let editTitle = '';
	let editLocation = '';
	let editDescription = '';
	let editStartDate = '';
	let editEndDate = '';
	let editAllDay = false;
	let editState: 'submitted' | 'published' = 'submitted';

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
				initializeEditForm();
			})
			.catch(() => {
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
		editTitle = event.title;
		editLocation = event.location || '';
		editDescription = event.description || '';
		editStartDate = parseUTCDate(event.start_date).toISOString().split('T')[0];
		editEndDate = event.end_date ? parseUTCDate(event.end_date).toISOString().split('T')[0] : '';
		editAllDay = event.all_day;
		editState = event.state;
	}

	function cancelEdit() {
		goto(resolve(`/events/${event.id}`));
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
				<input type="text" id="editTitle" bind:value={editTitle} required disabled={isSubmitting} />
			</div>

			<div class="form-group">
				<label for="editLocation">Location</label>
				<input type="text" id="editLocation" bind:value={editLocation} disabled={isSubmitting} />
			</div>

			<div class="form-group">
				<label for="editDescription">Description</label>
				<textarea id="editDescription" bind:value={editDescription} rows="3" disabled={isSubmitting}
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
					<input type="date" id="editEndDate" bind:value={editEndDate} disabled={isSubmitting} />
				</div>
			</div>

			<div class="form-group">
				<label class="checkbox-label">
					<input type="checkbox" bind:checked={editAllDay} disabled={isSubmitting} />
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
				<button
					type="submit"
					class="save-btn"
					disabled={isSubmitting || !editTitle || !editStartDate}
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
		margin-bottom: 2rem;
	}

	.event-header h1 {
		margin: 0;
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
</style>
