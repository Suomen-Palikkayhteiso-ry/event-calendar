import type { EventFormData } from '$lib/types';
import { localDateToUTC, localDateTimeToUTC } from '$lib/date-utils';

export function validateEventForm(formData: EventFormData): Record<string, string> {
	const errors: Record<string, string> = {};

	// Title is required
	if (!formData.title.trim()) {
		errors.title = 'Otsikko on pakollinen';
	}

	// Start date is required
	if (!formData.start_date) {
		errors.start_date = 'Aloituspäivä on pakollinen';
	}

	// If end date is provided, it must be after start date
	if (formData.end_date && formData.start_date) {
		const start = new Date(formData.all_day ? formData.start_date.split('T')[0] : formData.start_date);
		const end = new Date(formData.all_day ? formData.end_date.split('T')[0] : formData.end_date);
		if (end < start) {
			errors.end_date = 'Lopetuspäivän täytyy olla aloituspäivän jälkeen';
		}
	}

	// URL validation if provided
	if (formData.url && !/^https?:\/\/.+/.test(formData.url)) {
		errors.url = 'URL:n täytyy alkaa http:// tai https://';
	}

	// Latitude and longitude validation if point is provided
	if (formData.point) {
		if (formData.point.lat < -90 || formData.point.lat > 90) {
			errors.point = 'Leveysasteen täytyy olla välillä -90 ja 90';
		}
		if (formData.point.lon < -180 || formData.point.lon > 180) {
			errors.point = 'Pituusasteen täytyy olla välillä -180 ja 180';
		}
	}

	return errors;
}

export function prepareEventSubmitData(formData: EventFormData): FormData {
	const submitData = new FormData();
	submitData.append('title', formData.title);
	if (formData.location) submitData.append('location', formData.location);
	if (formData.description) submitData.append('description', formData.description);
	if (formData.url) submitData.append('url', formData.url);
	submitData.append(
		'start_date',
		formData.all_day
			? localDateToUTC(formData.start_date.split('T')[0])
			: localDateTimeToUTC(formData.start_date)
	);
	submitData.append(
		'end_date',
		formData.end_date
			? formData.all_day
				? localDateToUTC(formData.end_date.split('T')[0])
				: localDateTimeToUTC(formData.end_date)
			: formData.all_day
				? localDateToUTC(formData.start_date.split('T')[0])
				: localDateTimeToUTC(formData.start_date)
	);
	submitData.append('all_day', formData.all_day.toString());
	if (formData.image) submitData.append('image', formData.image);
	if (formData.image_description)
		submitData.append('image_description', formData.image_description);
	submitData.append('state', formData.state);
	if (formData.point) submitData.append('point', JSON.stringify(formData.point));

	return submitData;
}
