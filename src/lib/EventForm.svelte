<script lang="ts">
	import { onMount, tick } from 'svelte';
	import type { EventFormData } from '$lib/types';
	import { _ } from 'svelte-i18n';
	import { toast } from '@zerodevx/svelte-toast';
	import DateTimePicker from '$lib/DateTimePicker.svelte';
	import { geocodeLocation } from '$lib/geocode';
	import { createEventFormStore } from '$lib/stores/event-form';
	import { Button, Input, Textarea, Label, Select } from '$lib/ui';

	interface Props {
		initialData?: Partial<EventFormData>;
		onSubmit: (data: EventFormData) => void | Promise<void>;
		isSubmitting: boolean;
		mode: 'create' | 'edit';
		currentImage?: string;
		onCancel?: () => void;
	}

	let { initialData, onSubmit, isSubmitting, mode, currentImage, onCancel }: Props = $props();

	// Create form store instance
	const formStore = createEventFormStore(initialData);

	let formState = $formStore;

	let mapCenter = $state<[number, number]>([60.1699, 24.9384]); // Helsinki default
	let mapZoom = $state(10);
	let isGeocoding = $state(false);
	let geocodingEnabled = $state(true);
	let MapComponent = $state<any>(null);

	// Initialize form data from initialData
	onMount(async () => {
		// Wait for the component to be fully rendered
		await tick();

		if (initialData) {
			formStore.reset(initialData);
			if (initialData.point) {
				mapCenter = [initialData.point.lat, initialData.point.lon];
			}
		}
	});

	// Reset form when initialData changes
	$effect(() => {
		if (initialData) {
			formStore.reset(initialData);
			if (initialData.point) {
				mapCenter = [initialData.point.lat, initialData.point.lon];
			}
		}
	});

	// Lazy load Map component when location is entered
	$effect(() => {
		if (formState.formData.location && !MapComponent) {
			import('$lib/Map.svelte').then((module) => {
				MapComponent = module.default;
			});
		}
	});

	async function handleSubmit(e: SubmitEvent) {
		e.preventDefault();

		// Ensure end_date is set if not provided
		if (!formState.formData.end_date) {
			formStore.updateField('end_date', formState.formData.start_date);
		}

		if (!formStore.validate()) {
			// Focus on first error field
			const firstErrorField = Object.keys(formState.errors)[0];
			const element = document.getElementById(firstErrorField);
			if (element) element.focus();
			return;
		}

		await onSubmit(formState.formData);
	}
</script>

<form onsubmit={handleSubmit}>
	<div class="mb-4">
		<Label htmlFor="title" required>{$_('title_required')}</Label>
		<!-- svelte-ignore a11y_autofocus -->
		<Input
			id="title"
			type="text"
			value={formState.formData.title}
			placeholder={$_('event_title')}
			required
			ariaRequired={true}
			ariaInvalid={formState.errors.title ? true : false}
			ariaDescribedBy={formState.errors.title ? 'error-title' : undefined}
			autofocus={mode === 'create'}
			disabled={isSubmitting}
			on:input={(e) => {
				formStore.updateField('title', (e.target as HTMLInputElement).value);
				formStore.clearError('title');
			}}
		/>
		{#if formState.errors.title}
			<p id="error-title" class="form-error">{formState.errors.title}</p>
		{/if}
	</div>

	<div class="mb-4">
		<Label htmlFor="location">{$_('location_label')}</Label>
		<div class="flex items-center gap-2">
			<Input
				id="location"
				type="text"
				value={formState.formData.location}
				placeholder={$_('location_optional')}
				disabled={isSubmitting}
				on:input={(e) => {
					formStore.updateField('location', (e.target as HTMLInputElement).value);
					formStore.clearError('location');
				}}
				on:blur={async () => {
					if (formState.formData.location && !isGeocoding && geocodingEnabled) {
						isGeocoding = true;
						try {
							const coords = await geocodeLocation(formState.formData.location);
							if (coords) {
								formStore.updateField('point', {
									lat: parseFloat(coords[0].toFixed(6)),
									lon: parseFloat(coords[1].toFixed(6))
								}); // rounded
								mapCenter = [formState.formData.point!.lat, formState.formData.point!.lon];
								mapZoom = 15; // Zoom in closer after geocoding
							}
						} catch (error) {
							console.error('Geocoding failed:', error);
							toast.push($_('geocoding_failed'));
						} finally {
							isGeocoding = false;
						}
					}
				}}
				class="flex-1"
			/>
			<button
				type="button"
				class="cursor-pointer rounded border border-gray-300 p-3 text-xl hover:bg-gray-50 focus:ring-2 focus:ring-brand-primary focus:outline-none"
				onclick={() => (geocodingEnabled = !geocodingEnabled)}
				aria-label={geocodingEnabled ? $_('disable_geocoding') : $_('enable_geocoding')}
			>
				{geocodingEnabled ? '🌍' : '📍'}
			</button>
		</div>
	</div>

	{#if formState.formData.location}
		<div class="mb-4">
			{#if MapComponent}
				<MapComponent
					center={mapCenter}
					zoom={mapZoom}
					markerPosition={formState.formData.point
						? [formState.formData.point.lat, formState.formData.point.lon]
						: null}
					onMarkerMove={(latlng: [number, number]) => {
						formStore.updateField('point', {
							lat: parseFloat(latlng[0].toFixed(6)),
							lon: parseFloat(latlng[1].toFixed(6))
						});
						mapCenter = [formState.formData.point!.lat, formState.formData.point!.lon];
					}}
				/>
			{:else}
				<p>Loading map...</p>
			{/if}
		</div>
	{/if}

	{#if formState.formData.point}
		<div class="mb-4 flex gap-4">
			<div class="flex-1">
				<Label htmlFor="lat">{$_('latitude')}</Label>
				<Input
					id="lat"
					type="number"
					step="0.000001"
					min="-90"
					max="90"
					value={formState.formData.point?.lat}
					disabled={isSubmitting}
					ariaInvalid={formState.errors.point ? true : false}
					ariaDescribedBy={formState.errors.point ? 'error-point' : undefined}
					on:input={(e) => {
						const lat = parseFloat((e.target as HTMLInputElement).value);
						formStore.updateField('point', {
							lat: lat,
							lon: formState.formData.point?.lon || 0
						});
					}}
				/>
			</div>
			<div class="flex-1">
				<Label htmlFor="lng">{$_('longitude')}</Label>
				<Input
					id="lng"
					type="number"
					step="0.000001"
					min="-180"
					max="180"
					value={formState.formData.point?.lon}
					disabled={isSubmitting}
					ariaInvalid={formState.errors.point ? true : false}
					ariaDescribedBy={formState.errors.point ? 'error-point' : undefined}
					on:input={(e) => {
						const lon = parseFloat((e.target as HTMLInputElement).value);
						formStore.updateField('point', {
							lat: formState.formData.point?.lat || 0,
							lon: lon
						});
					}}
				/>
			</div>
		</div>
		{#if formState.errors.point}
			<p id="error-point" class="form-error">{formState.errors.point}</p>
		{/if}
	{/if}

	<div class="mb-4">
		<Label htmlFor="description">{$_('description_label')}</Label>
		<Textarea
			id="description"
			value={formState.formData.description}
			placeholder={$_('description_optional')}
			rows={3}
			disabled={isSubmitting}
			on:input={(e) => formStore.updateField('description', (e.target as HTMLTextAreaElement).value)}
		/>
	</div>

	<div class="mb-4">
		<Label htmlFor="url">{$_('url_label')}</Label>
		<Input
			id="url"
			type="url"
			value={formState.formData.url}
			placeholder={$_('url_optional')}
			disabled={isSubmitting}
			pattern="https?://.+"
			ariaInvalid={formState.errors.url ? true : false}
			ariaDescribedBy={formState.errors.url ? 'error-url' : undefined}
			on:input={(e) => {
				formStore.updateField('url', (e.target as HTMLInputElement).value);
				formStore.clearError('url');
			}}
		/>
		{#if formState.errors.url}
			<p id="error-url" class="form-error">{formState.errors.url}</p>
		{/if}
	</div>

	<div class="mb-4">
		<Label htmlFor="image">{$_('image_label')}</Label>
		<input
			type="file"
			id="image"
			accept="image/*"
			disabled={isSubmitting}
			aria-describedby="imageHelp"
			onchange={(e) => {
				const target = e.target as HTMLInputElement;
				formStore.updateField('image', target.files?.[0] || null);
			}}
		/>
		{#if currentImage}
			<p id="imageHelp" class="form-help italic">
				{$_('current_image')}
				{currentImage}
			</p>
		{:else}
			<p id="imageHelp" class="form-help">{$_('image_help_text')}</p>
		{/if}
	</div>

	<div class="mb-4">
		<Label htmlFor="imageDescription">{$_('image_description_label')}</Label>
		<Input
			id="imageDescription"
			type="text"
			value={formState.formData.image_description}
			placeholder={$_('image_description_optional')}
			disabled={isSubmitting}
			on:input={(e) =>
				formStore.updateField('image_description', (e.target as HTMLInputElement).value)}
		/>
	</div>

	<div class="flex gap-4">
		<div class="flex-1">
			<DateTimePicker
				id="startDate"
				label={$_('start_date_required')}
				value={formState.formData.start_date}
				onChange={(value) => {
					formStore.updateField('start_date', value);
				}}
				disabled={isSubmitting}
				allDay={formState.formData.all_day}
				ariaInvalid={!!formState.errors.start_date}
				ariaDescribedBy={formState.errors.start_date ? 'error-start-date' : undefined}
			/>
			{#if formState.errors.start_date}
				<p id="error-start-date" class="form-error">{formState.errors.start_date}</p>
			{/if}
		</div>

		<div class="flex-1">
			<DateTimePicker
				id="endDate"
				label={$_('end_date')}
				value={formState.formData.end_date}
				onChange={(value) => {
					formStore.updateField('end_date', value);
				}}
				disabled={isSubmitting || !formState.formData.all_day}
				allDay={formState.formData.all_day}
				ariaInvalid={!!formState.errors.end_date}
				ariaDescribedBy={formState.errors.end_date ? 'error-end-date' : undefined}
			/>
			{#if formState.errors.end_date}
				<p id="error-end-date" class="form-error">{formState.errors.end_date}</p>
			{/if}
		</div>
	</div>

	<div class="mb-4">
		<label for="allDay" class="flex cursor-pointer items-center gap-2 font-normal">
			<input
				type="checkbox"
				id="allDay"
				checked={formState.formData.all_day}
				disabled={isSubmitting}
				onchange={(e) => {
					const target = e.target as HTMLInputElement;
					formStore.updateField('all_day', target.checked);
					if (!target.checked) {
						// For timed events, force end date to match start date
						formStore.updateField('end_date', formState.formData.start_date);
					}
				}}
			/>
			{$_('all_day_event_label')}
		</label>
	</div>

	<div class="mb-4">
		<Label htmlFor="state">{$_('status')}</Label>
		<Select
			id="state"
			value={formState.formData.state}
			disabled={isSubmitting}
			on:change={(e) =>
				formStore.updateField(
					'state',
					(e.target as HTMLSelectElement).value as EventFormData['state']
				)}
		>
			<option value="draft">{$_('draft')}</option>
			<option value="pending">{$_('pending')}</option>
			<option value="published">{$_('published')}</option>
			<option value="deleted">{$_('deleted')}</option>
		</Select>
	</div>

	<div class="mt-6 flex gap-2">
		<Button
			type="submit"
			variant="primary"
			disabled={isSubmitting || !formState.formData.title || !formState.formData.start_date}
			ariaDisabled={isSubmitting || !formState.formData.title || !formState.formData.start_date}
		>
			{isSubmitting
				? mode === 'create'
					? $_('creating')
					: $_('saving')
				: mode === 'create'
					? $_('create_event')
					: $_('save_changes')}
		</Button>
		<Button
			type="button"
			variant="secondary"
			onclick={() => (onCancel ? onCancel() : history.back())}
			disabled={isSubmitting}
		>
			{$_('cancel')}
		</Button>
	</div>
</form>
