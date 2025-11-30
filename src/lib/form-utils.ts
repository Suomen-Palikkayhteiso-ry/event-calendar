import type { EventFormData } from '$lib/types';
import { localDateToUTC, localDateTimeToUTC } from '$lib/date-utils';
import { _, get } from 'svelte-i18n';

/**
 * Validates event form data and returns an object with field errors
 * @param formData - The form data to validate
 * @returns An object with field names as keys and error messages as values
 */
export function validateEventForm(formData: EventFormData): Record<string, string> {
	const errors: Record<string, string> = {};
	const t = get(_);

	// Title is required
	if (!formData.title.trim()) {
		errors.title = t('title_required_error');
	}

	// Start date is required
	if (!formData.start_date) {
		errors.start_date = t('start_date_required_error');
	}

	// If end date is provided, it must be after start date
	if (formData.end_date && formData.start_date) {
		const start = new Date(
			formData.all_day ? formData.start_date.split('T')[0] : formData.start_date
		);
		const end = new Date(formData.all_day ? formData.end_date.split('T')[0] : formData.end_date);
		if (end < start) {
			errors.end_date = t('end_date_after_start_error');
		}
	}

	// URL validation if provided
	if (formData.url && !/^https?:\/\/.+/.test(formData.url)) {
		errors.url = t('url_invalid_error');
	}

	// Latitude and longitude validation if point is provided
	if (formData.point) {
		if (formData.point.lat < -90 || formData.point.lat > 90) {
			errors.point = t('latitude_invalid_error');
		}
		if (formData.point.lon < -180 || formData.point.lon > 180) {
			errors.point = t('longitude_invalid_error');
		}
	}

	return errors;
}

/**
 * Prepares event form data for submission to the API
 * Converts local dates to UTC and formats data as FormData
 * @param formData - The form data to prepare
 * @returns FormData object ready for API submission
 */
export function prepareEventSubmitData(formData: EventFormData): FormData {
	const submitData = new FormData();
	submitData.append('title', formData.title);
	submitData.append('location', formData.location);
	submitData.append('description', formData.description);
	submitData.append('url', formData.url);
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
	submitData.append('image_description', formData.image_description);
	submitData.append('state', formData.state);
	if (formData.point) submitData.append('point', JSON.stringify(formData.point));

	return submitData;
}
