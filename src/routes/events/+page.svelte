<script lang="ts">
	import { onMount } from 'svelte';
	import { pb } from '$lib/pocketbase';
	import type { Event, EventFormData } from '$lib/types';
	import { user } from '$lib/auth';
	import { goto } from '$app/navigation';
	import { resolve } from '$app/paths';
	import { _ } from 'svelte-i18n';
	import { toast } from '@zerodevx/svelte-toast';
	import EventForm from '$lib/EventForm.svelte';
	import EventList from '$lib/EventList.svelte';
	import KMLImport from '$lib/KMLImport.svelte';
	import { importKML } from '$lib/kml-utils';
	import { prepareEventSubmitData } from '$lib/form-utils';

	let events = $state<Event[]>([]);
	let currentPage = $state(1);
	let pageSize = 100;
	let isSubmitting = $state(false);
	let kmlFile: File | null = $state(null);
	let isImporting = $state(false);
	let isLoadingEvents = $state(false);

	// Form data object for create form
	let createFormData = $state({
		title: '',
		start_date: '',
		end_date: '',
		all_day: true,
		location: '',
		description: '',
		url: '',
		image: null as File | null,
		image_description: '',
		state: 'published' as 'draft' | 'published',
		point: null as { lat: number; lon: number } | null
	});

	let totalEvents = $state(0);

	// Check authentication
	$effect(() => {
		if (!$user) {
			goto(resolve('/'));
		}
	});

	async function fetchEvents(page = 1) {
		if (!$user) return;

		isLoadingEvents = true;
		try {
			const result = await pb.collection('events').getList(page, pageSize, {
				sort: '-start_date',
				filter: 'state = "published" || state = "draft"'
			});

			events = result.items as unknown as Event[];
			totalEvents = result.totalItems;
			currentPage = page;
		} catch (error) {
			console.error('Error fetching events:', error);
			toast.push($_('failed_fetch_events'));
		} finally {
			isLoadingEvents = false;
		}
	}

	onMount(async () => {
		await fetchEvents();
	});

	// Add ESC key listener
	$effect(() => {
		const handleKeydown = (event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				cancelAdd();
			}
		};
		document.addEventListener('keydown', handleKeydown);

		return () => {
			document.removeEventListener('keydown', handleKeydown);
		};
	});

	async function createEvent(formData: EventFormData) {
		if (!$user) return;

		isSubmitting = true;
		try {
			const submitData = prepareEventSubmitData(formData);

			await pb.collection('events').create(submitData);

			toast.push($_('event_created_successfully'));

			// Reset form
			createFormData.title = '';
			createFormData.location = '';
			createFormData.description = '';
			createFormData.url = '';
			createFormData.image = null;
			createFormData.image_description = '';
			createFormData.state = 'published';
			createFormData.all_day = true;
			createFormData.point = null;

			// Refresh events list
			await fetchEvents(currentPage);
		} catch (error) {
			console.error('Error creating event:', error);
			toast.push($_('failed_create_event'));
		} finally {
			isSubmitting = false;
		}
	}

	async function updateEventState(eventId: string, newState: string) {
		try {
			await pb.collection('events').update(eventId, { state: newState });
			// Refresh the events list
			await fetchEvents(currentPage);
			toast.push($_('event_updated_successfully'));
		} catch (error) {
			console.error('Error updating event state:', error);
			toast.push($_('failed_update_event'));
		}
	}

	function cancelAdd() {
		goto(resolve('/'));
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

	async function handleKMLImport() {
		if (!kmlFile) return;

		isImporting = true;
		try {
			await importKML(kmlFile, () => {
				kmlFile = null;
				fetchEvents(currentPage);
			});
			toast.push($_('kml_import_successful'));
		} catch (error) {
			console.error('Error importing KML:', error);
			toast.push($_('failed_kml_import'));
		} finally {
			isImporting = false;
		}
	}
</script>

<h1 class="mb-8 text-gray-900">{$_('add_new_event')}</h1>

{#if !$user}
	<p>{$_('login_required')}</p>
{:else}
	<EventForm mode="create" {isSubmitting} onSubmit={createEvent} />

	<KMLImport
		{kmlFile}
		{isImporting}
		onFileChange={(file) => (kmlFile = file)}
		onImport={handleKMLImport}
	/>

	<EventList
		{events}
		{totalEvents}
		{currentPage}
		{pageSize}
		onUpdateState={updateEventState}
		onNextPage={nextPage}
		onPrevPage={prevPage}
		loading={isLoadingEvents}
	/>
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
