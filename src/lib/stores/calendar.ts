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

		setSelectedDateFromUrlParam(dateParam: string | null) {
			if (!dateParam) return;
			const paramDate = new Date(`${dateParam}T00:00:00`);
			if (!Number.isNaN(paramDate.getTime())) {
				this.setSelectedDate(paramDate);
			}
		},

		reset() {
			set(initialState);
		}
	};
}

export const calendarStore = createCalendarStore();
