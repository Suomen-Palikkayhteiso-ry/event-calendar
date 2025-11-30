import { writable, get } from 'svelte/store';
import type { EventFormData } from '$lib/types';
import { validateEventForm } from '$lib/form-utils';

export interface EventFormState {
	formData: EventFormData;
	errors: Partial<Record<string, string>>;
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

	const { subscribe, set, update } = writable(initialState);

	return {
		subscribe,

		updateField<K extends keyof EventFormData>(field: K, value: EventFormData[K]) {
			update((state) => {
				const newFormData = { ...state.formData, [field]: value };
				return {
					...state,
					formData: newFormData,
					isDirty: true,
					errors: { ...state.errors, [field]: undefined }
				};
			});
		},

		setErrors(errors: Partial<Record<string, string>>) {
			update((state) => ({ ...state, errors }));
		},

		clearError(field: keyof EventFormData) {
			update((state) => {
				const newErrors = { ...state.errors };
				delete newErrors[field];
				return { ...state, errors: newErrors };
			});
		},

		validate() {
			const state = get({ subscribe });
			const validationErrors = validateEventForm(state.formData);
			update((s) => ({ ...s, errors: validationErrors }));
			return Object.keys(validationErrors).length === 0;
		},

		reset(newInitialData?: Partial<EventFormData>) {
			const newFormData = { ...initialFormData, ...newInitialData };
			set({
				formData: newFormData,
				errors: {},
				isDirty: false
			});
		}
	};
}

export type EventFormStore = ReturnType<typeof createEventFormStore>;

export { createEventFormStore };