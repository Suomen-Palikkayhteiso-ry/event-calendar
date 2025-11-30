import type { EventFormData } from '$lib/types';
import { localDateToUTC, localDateTimeToUTC } from '$lib/date-utils';

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
