import { addMessages, init } from 'svelte-i18n';

addMessages('fi', {
	calendar: 'kalenteri',
	list: 'lista',
	back: 'takaisin',
	public_calendar: 'Palikkakalenteri',
	select_date: 'Päivä:',
	back_to_calendar: 'Takaisin kalenteriin',
	location: 'Paikka:',
	start: 'Alkaa:',
	end: 'Päättyy:',
	all_day_event: 'Koko päivän tapahtuma',
	description: 'Kuvaus:',
	// Calendar navigation
	today: 'tänään',
	prev: 'Edellinen',
	next: 'Seuraava',
	// Page titles
	event_calendar: 'Palikkakalenteri',
	event_calendar_description: 'Suomen Palikkayhteisö ry:n Palikkakalenteri',
	// Events management
	add_new_event: 'Lisää uusi tapahtuma',
	login_required: 'Sinun täytyy kirjautua sisään hallitaksesi tapahtumia.',
	create_new_event: 'Luo uusi tapahtuma',
	title_required: 'Otsikko *',
	event_title: 'Tapahtuman otsikko',
	location_label: 'Paikka',
	location_optional: 'Paikka (valinnainen)',
	description_label: 'Kuvaus',
	description_optional: 'Kuvaus (valinnainen)',
	url_label: 'URL:',
	url_optional: 'URL (valinnainen)',
	image_label: 'Kuva',
	image_description_label: 'Kuvan kuvaus',
	image_description_optional: 'Kuvan kuvaus (valinnainen)',
	start_date_required: 'Aloituspäivä *',
	end_date: 'Lopetuspäivä',
	all_day_event_label: 'Koko päivän tapahtuma',
	creating: 'Luodaan...',
	create_event: 'Luo tapahtuma',
	cancel: 'Peruuta',
	existing_events: 'Olemassa olevat tapahtumat',
	edit: 'Muokkaa',
	all_day: '(Koko päivä)',
	status: 'Tila:',
	previous: 'Edellinen',
	page: 'Sivu',
	of: '/',
	total_events: 'tapahtumaa yhteensä',
	next_button: 'Seuraava',
	failed_create_event: 'Tapahtuman luonti epäonnistui. Yritä uudelleen.',
	event_created_successfully: 'Tapahtuma luotu onnistuneesti.',
	// Event detail
	deleting: 'Poistetaan...',
	delete: 'Poista',
	back_to_calendar_detail: 'Takaisin kalenteriin',
	loading_event: 'Ladataan tapahtumaa...',
	failed_delete_event: 'Tapahtuman poistaminen epäonnistui. Yritä uudelleen.',
	event_deleted_successfully: 'Tapahtuma poistettu onnistuneesti.',
	confirm_delete_event: 'Haluatko varmasti poistaa tämän tapahtuman?',
	// Event edit
	edit_event: 'Muokkaa tapahtumaa',
	current_image: 'Nykyinen kuva:',
	saving: 'Tallennetaan...',
	save_changes: 'Tallenna muutokset',
	failed_update_event: 'Tapahtuman päivitys epäonnistui. Yritä uudelleen.',
	event_updated_successfully: 'Tapahtuma päivitetty onnistuneesti.',
	// Status options
	draft: 'Luonnos',
	published: 'Julkaistu',
	// Header
	organization_alt: 'Suomen Palikkayhteisö ry',
	calendar_title: 'Palikkakalenteri',
	hello: 'Hei,',
	logout: 'Kirjaudu ulos',
	login: 'Kirjaudu sisään',
	// Layout
	rss_feed: 'RSS',
	html_feed: 'HTML',
	ical_feed: 'iCalendar',
	atom_feed: 'ATOM',
	json_feed: 'JSON',
	ical_description:
		'iCalendar tilaa tai integroi koko kalenterin helposti. Klikkaa vaikka tästä kalenteri puhelimeesi.',
	html_description:
		'Upota tai tulosta valmis HTML-tapahtumalistaus. Sisältää kalenterilinkit yksittäisiin tapahtumiin.',
	feeds_description:
		'Syötemuodot integroivat uudet tapahtumat verkkosivuille tai uutislukijoihin. Sisältää iCalendar-linkit.',
	// Callback
	completing_login: 'Viimeistellään kirjautumista...',
	// Non-member instructions
	non_member_prefix: 'Jos et ole Suomen Palikkayhteisö ry:n jäsen, ',
	send_event_email: 'lähetä tapahtumasi meille sähköpostilla'
});

init({
	fallbackLocale: 'fi',
	initialLocale: 'fi'
});
