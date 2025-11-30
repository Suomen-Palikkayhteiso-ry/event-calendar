import { get } from 'svelte/store';
import type { EventFormData } from '$lib/types';
import { validateEventForm } from '$lib/form-utils';

export interface EventFormState {
	formData: EventFormData;
	errors: Record<string, string>;
	isDirty: boolean;
}

function createEventFormStore(initialData?: Partial<EventFormData>) {
	const initialFormData: EventFormData = {
		title: '',
		start_date: '',
		end_date: '',
		all_day: true,
		location: '',
		description: '',
		url: '',
		image: null,
		image_description: '',
		state: 'published',
		point: null,
		...initialData
	};

	const initialState: EventFormState = {
		formData: initialFormData,
		errors: {},
		isDirty: false
	};

	// Using Svelte 5 runes for state management
	let state = $state(initialState);

	return {
		get state() {
			return state;
		},

		updateField<K extends keyof EventFormData>(field: K, value: EventFormData[K]) {
			state.formData[field] = value;
			state.isDirty = true;
			// Clear error for this field
			if (state.errors[field]) {
				delete state.errors[field];
			}
		},

		setErrors(errors: Record<string, string>) {
			state.errors = errors;
		},

		clearError(field: keyof EventFormData) {
			if (state.errors[field]) {
				delete state.errors[field];
			}
		},

		validate() {
			const validationErrors = validateEventForm(state.formData);
			state.errors = validationErrors;
			return Object.keys(validationErrors).length === 0;
		},

		reset(newInitialData?: Partial<EventFormData>) {
			const newFormData = { ...initialFormData, ...newInitialData };
			state = {
				formData: newFormData,
				errors: {},
				isDirty: false
			};
		}
	};
}

export type EventFormStore = ReturnType<typeof createEventFormStore>;

export { createEventFormStore };