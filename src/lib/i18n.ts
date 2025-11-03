import { addMessages, init } from 'svelte-i18n';

addMessages('fi', {
	public_calendar: 'Palikkakalenteri',
	select_date: 'Päivä:',
	back_to_calendar: 'Takaisin kalenteriin',
	location: 'Paikka:',
	start: 'Alkaa:',
	end: 'Päättyy:',
	all_day_event: 'Koko päivän tapahtuma',
	description: 'Kuvaus:'
});

init({
	fallbackLocale: 'fi',
	initialLocale: 'fi'
});
