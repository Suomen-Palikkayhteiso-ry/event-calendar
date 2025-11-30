<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event, EventFormData } from '$lib/types';
	import { page } from '$app/stores';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { user } from '$lib/auth';
	import { _ } from 'svelte-i18n';
	import { toast } from '@zerodevx/svelte-toast';
	import EventForm from '$lib/EventForm.svelte';
	import { prepareEventSubmitData } from '$lib/form-utils';

	let event = $state<Event | null>(null);
	let isSubmitting = $state(false);

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
			toast.push($_('invalid_event_id'));
			goto(resolve('/events'));
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
			})
			.catch((error) => {
				console.error('Edit form: Error loading event:', error);
				toast.push($_('failed_load_event'));
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

	function cancelEdit() {
		if (event) {
			goto(resolve(`/events/${event.id}`));
		} else {
			goto(resolve('/events'));
		}
	}

	async function saveEdit(formData: EventFormData) {
		if (!$user || !event) return;

		isSubmitting = true;
		try {
			const submitData = prepareEventSubmitData(formData);

			await pb.collection('events').update(event.id, submitData);

			toast.push($_('event_updated_successfully'));

			// Redirect back to view
			goto(resolve(`/events/${event.id}`));
		} catch (error) {
			console.error('Error updating event:', error);
			toast.push($_('failed_update_event'));
		} finally {
			isSubmitting = false;
		}
	}
</script>

{#if event}
	<h1 class="mb-8 text-gray-900">{$_('edit_event')}</h1>

	<EventForm
		mode="edit"
		{isSubmitting}
		initialData={{
			title: event.title,
			start_date: event.start_date,
			end_date: event.end_date || event.start_date,
			all_day: event.all_day,
			location: event.location || '',
			description: event.description || '',
			url: event.url || '',
			image: null,
			image_description: event.image_description || '',
			state: event.state,
			point: event.point
				? {
						lat: parseFloat(event.point.lat.toFixed(6)),
						lon: parseFloat(event.point.lon.toFixed(6))
					}
				: null
		}}
		currentImage={event.image}
		onSubmit={saveEdit}
		onCancel={cancelEdit}
	/>
{:else}
	<p>{$_('loading_event')}</p>
{/if}

<style>
	/* Make datepicker current date visible */
	:global(.day.today) {
		background-color: var(--color-brand-accent) !important;
		color: white !important;
		border: 2px solid var(--color-brand-accent) !important;
	}
	:global(.day.today:not(.selected)) {
		background-color: white !important;
		color: var(--color-brand-accent) !important;
	}
</style>
