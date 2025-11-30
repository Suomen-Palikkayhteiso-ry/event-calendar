import { writable } from 'svelte/store';

export interface CalendarState {
	selectedDate: Date;
}

function createCalendarStore() {
	const initialState: CalendarState = {
		selectedDate: new Date()
	};

	const { subscribe, set, update } = writable(initialState);

	return {
		subscribe,
		set,
		update,

		setSelectedDate(date: Date) {
			update((state) => ({ ...state, selectedDate: date }));
		},

		reset() {
			set(initialState);
		}
	};
}

export const calendarStore = createCalendarStore();
