module I18n exposing (..)


type alias Translations =
    { calendar : String
    , list : String
    , today : String
    , prev : String
    , next_button : String
    , back : String
    , public_calendar : String
    , select_date : String
    , back_to_calendar : String
    , location : String
    , start : String
    , end : String
    , all_day_event : String
    , description : String
    , event_calendar : String
    , event_calendar_description : String
    , add_new_event : String
    , login_required : String
    , create_new_event : String
    , title_required : String
    , title : String
    , event_title : String
    , location_label : String
    , location_optional : String
    , latitude : String
    , longitude : String
    , geocoding_failed : String
    , disable_geocoding : String
    , enable_geocoding : String
    , description_label : String
    , description_optional : String
    , dates : String
    , actions : String
    , url_label : String
    , url_optional : String
    , image_label : String
    , image_description_label : String
    , image_description_optional : String
    , image_help_text : String
    , view_on_map : String
    , edit_event : String
    , previous_page : String
    , next_page : String
    , events_table : String
    , start_date_required : String
    , end_date : String
    , all_day_event_label : String
    , creating : String
    , create_event : String
    , cancel : String
    , existing_events : String
    , edit : String
    , all_day : String
    , status : String
    , previous : String
    , page : String
    , of : String
    , total_events : String
    , failed_create_event : String
    , event_created_successfully : String
    , failed_fetch_events : String
    , failed_kml_import : String
    , failed_load_event : String
    , invalid_event_id : String
    , deleting : String
    , delete : String
    , back_to_calendar_detail : String
    , loading_event : String
    , loading_events : String
    , failed_delete_event : String
    , event_deleted_successfully : String
    , confirm_delete_event : String
    , current_image : String
    , saving : String
    , save_changes : String
    , failed_update_event : String
    , event_updated_successfully : String
    , draft : String
    , pending : String
    , published : String
    , deleted : String
    , organization_alt : String
    , calendar_title : String
    , hello : String
    , logout : String
    , login : String
    , rss_feed : String
    , html_feed : String
    , ical_feed : String
    , atom_feed : String
    , json_feed : String
    , geojson_feed : String
    , ical_description : String
    , html_description : String
    , feeds_description : String
    , completing_login : String
    , non_member_prefix : String
    , send_event_email : String
    , error_page_title : String
    , something_went_wrong : String
    , page_not_found : String
    , page_not_found_description : String
    , error_page_description : String
    , error_details : String
    , reload_page : String
    , component_error_title : String
    , component_error_description : String
    , try_again : String
    , title_required_error : String
    , start_date_required_error : String
    , end_date_after_start_error : String
    , url_invalid_error : String
    , latitude_invalid_error : String
    , longitude_invalid_error : String
    }


fi : Translations
fi =
    { calendar = "kalenteri"
    , list = "lista"
    , today = "tänään"
    , prev = "Edellinen"
    , next_button = "Seuraava"
    , back = "takaisin"
    , public_calendar = "Palikkakalenteri"
    , select_date = "Päivä:"
    , back_to_calendar = "Takaisin kalenteriin"
    , location = "Paikka:"
    , start = "Alkaa:"
    , end = "Päättyy:"
    , all_day_event = "Koko päivän tapahtuma"
    , description = "Kuvaus:"
    , event_calendar = "Palikkakalenteri"
    , event_calendar_description = "Suomen Palikkayhteisö ry:n Palikkakalenteri"
    , add_new_event = "Lisää uusi tapahtuma"
    , login_required = "Sinun täytyy kirjautua sisään hallitaksesi tapahtumia."
    , create_new_event = "Luo uusi tapahtuma"
    , title_required = "Otsikko *"
    , title = "Otsikko"
    , event_title = "Tapahtuman otsikko"
    , location_label = "Paikka"
    , location_optional = "Paikka (valinnainen)"
    , latitude = "Leveysaste"
    , longitude = "Pituusaste"
    , geocoding_failed = "Sijainnin haku epäonnistui. Tarkista osoite tai syötä koordinaatit manuaalisesti."
    , disable_geocoding = "Poista geocoding käytöstä"
    , enable_geocoding = "Ota geocoding käyttöön"
    , description_label = "Kuvaus"
    , description_optional = "Kuvaus (valinnainen)"
    , dates = "Päivämäärät"
    , actions = "Toiminnot"
    , url_label = "Kotisivut:"
    , url_optional = "Kotisivut (valinnainen)"
    , image_label = "Kuva"
    , image_description_label = "Kuvan kuvaus"
    , image_description_optional = "Kuvan kuvaus (valinnainen)"
    , image_help_text = "Valitse kuva tapahtumalle (valinnainen)"
    , view_on_map = "Näytä kartalla"
    , edit_event = "Muokkaa tapahtumaa"
    , previous_page = "Edellinen sivu"
    , next_page = "Seuraava sivu"
    , events_table = "Tapahtumat taulukko"
    , start_date_required = "Aloituspäivä *"
    , end_date = "Lopetuspäivä"
    , all_day_event_label = "Koko päivän tapahtuma"
    , creating = "Luodaan..."
    , create_event = "Luo tapahtuma"
    , cancel = "Peruuta"
    , existing_events = "Olemassa olevat tapahtumat"
    , edit = "Muokkaa"
    , all_day = "(Koko päivä)"
    , status = "Tila:"
    , previous = "Edellinen"
    , page = "Sivu"
    , of = "/"
    , total_events = "tapahtumaa yhteensä"
    , failed_create_event = "Tapahtuman luonti epäonnistui. Yritä uudelleen."
    , event_created_successfully = "Tapahtuma luotu onnistuneesti."
    , failed_fetch_events = "Tapahtumien lataaminen epäonnistui. Yritä uudelleen."
    , failed_kml_import = "KML-tuonnin epäonnistui. Tarkista tiedosto ja yritä uudelleen."
    , failed_load_event = "Tapahtuman lataaminen epäonnistui."
    , invalid_event_id = "Virheellinen tapahtuman tunniste."
    , deleting = "Poistetaan..."
    , delete = "Poista"
    , back_to_calendar_detail = "Takaisin kalenteriin"
    , loading_event = "Ladataan tapahtumaa..."
    , loading_events = "Ladataan tapahtumia..."
    , failed_delete_event = "Tapahtuman poistaminen epäonnistui. Yritä uudelleen."
    , event_deleted_successfully = "Tapahtuma poistettu onnistuneesti."
    , confirm_delete_event = "Haluatko varmasti poistaa tämän tapahtuman?"
    , current_image = "Nykyinen kuva:"
    , saving = "Tallennetaan..."
    , save_changes = "Tallenna muutokset"
    , failed_update_event = "Tapahtuman päivitys epäonnistui. Yritä uudelleen."
    , event_updated_successfully = "Tapahtuma päivitetty onnistuneesti."
    , draft = "Luonnos"
    , pending = "Odottaa"
    , published = "Julkaistu"
    , deleted = "Poistettu"
    , organization_alt = "Suomen Palikkayhteisö ry"
    , calendar_title = "Palikkakalenteri"
    , hello = "Hei,"
    , logout = "Kirjaudu ulos"
    , login = "Kirjaudu sisään"
    , rss_feed = "RSS"
    , html_feed = "HTML | PDF"
    , ical_feed = "iCalendar"
    , atom_feed = "ATOM"
    , json_feed = "JSON"
    , geojson_feed = "GeoJSON"
    , ical_description = "Kalenterivienti (ICS) tilaa tai integroi koko kalenterin helposti. Klikkaa kalenteri puhelimeesi!"
    , html_description = "Upota tai tulosta valmis tapahtumalistaus. Sisältää kalenterilinkit yksittäisiin tapahtumiin."
    , feeds_description = "Syötteet integroivat uudet tapahtumat verkkosivuille. Nämäkin sisältävät kalenterilinkit."
    , completing_login = "Viimeistellään kirjautumista..."
    , non_member_prefix = "Jos et ole Suomen Palikkayhteisö ry:n jäsen, "
    , send_event_email = "lähetä tapahtumasi meille sähköpostilla"
    , error_page_title = "Virhe"
    , something_went_wrong = "Jotain meni pieleen"
    , page_not_found = "Sivua ei löytynyt"
    , page_not_found_description = "Etsimääsi sivua ei löytynyt."
    , error_page_description = "Tapahtui odottamaton virhe. Yritä myöhemmin uudelleen."
    , error_details = "Virheen tiedot"
    , reload_page = "Lataa sivu uudelleen"
    , component_error_title = "Komponentin virhe"
    , component_error_description = "Tässä komponentissa tapahtui virhe."
    , try_again = "Yritä uudelleen"
    , title_required_error = "Otsikko on pakollinen"
    , start_date_required_error = "Aloituspäivä on pakollinen"
    , end_date_after_start_error = "Lopetuspäivän täytyy olla aloituspäivän jälkeen"
    , url_invalid_error = "URL:n täytyy alkaa http:// tai https://"
    , latitude_invalid_error = "Leveysasteen täytyy olla välillä -90 ja 90"
    , longitude_invalid_error = "Pituusasteen täytyy olla välillä -180 ja 180"
    }


get : String -> String
get key =
    case key of
        "calendar" ->
            fi.calendar

        "list" ->
            fi.list

        "today" ->
            fi.today

        "prev" ->
            fi.prev

        "next_button" ->
            fi.next_button

        "back" ->
            fi.back

        "public_calendar" ->
            fi.public_calendar

        "select_date" ->
            fi.select_date

        "back_to_calendar" ->
            fi.back_to_calendar

        "location" ->
            fi.location

        "start" ->
            fi.start

        "end" ->
            fi.end

        "all_day_event" ->
            fi.all_day_event

        "description" ->
            fi.description

        "event_calendar" ->
            fi.event_calendar

        "event_calendar_description" ->
            fi.event_calendar_description

        "add_new_event" ->
            fi.add_new_event

        "login_required" ->
            fi.login_required

        "create_new_event" ->
            fi.create_new_event

        "title_required" ->
            fi.title_required

        "title" ->
            fi.title

        "event_title" ->
            fi.event_title

        "location_label" ->
            fi.location_label

        "location_optional" ->
            fi.location_optional

        "latitude" ->
            fi.latitude

        "longitude" ->
            fi.longitude

        "geocoding_failed" ->
            fi.geocoding_failed

        "disable_geocoding" ->
            fi.disable_geocoding

        "enable_geocoding" ->
            fi.enable_geocoding

        "description_label" ->
            fi.description_label

        "description_optional" ->
            fi.description_optional

        "dates" ->
            fi.dates

        "actions" ->
            fi.actions

        "url_label" ->
            fi.url_label

        "url_optional" ->
            fi.url_optional

        "image_label" ->
            fi.image_label

        "image_description_label" ->
            fi.image_description_label

        "image_description_optional" ->
            fi.image_description_optional

        "image_help_text" ->
            fi.image_help_text

        "view_on_map" ->
            fi.view_on_map

        "edit_event" ->
            fi.edit_event

        "previous_page" ->
            fi.previous_page

        "next_page" ->
            fi.next_page

        "events_table" ->
            fi.events_table

        "start_date_required" ->
            fi.start_date_required

        "end_date" ->
            fi.end_date

        "all_day_event_label" ->
            fi.all_day_event_label

        "creating" ->
            fi.creating

        "create_event" ->
            fi.create_event

        "cancel" ->
            fi.cancel

        "existing_events" ->
            fi.existing_events

        "edit" ->
            fi.edit

        "all_day" ->
            fi.all_day

        "status" ->
            fi.status

        "previous" ->
            fi.previous

        "page" ->
            fi.page

        "of" ->
            fi.of

        "total_events" ->
            fi.total_events

        "failed_create_event" ->
            fi.failed_create_event

        "event_created_successfully" ->
            fi.event_created_successfully

        "failed_fetch_events" ->
            fi.failed_fetch_events

        "failed_kml_import" ->
            fi.failed_kml_import

        "failed_load_event" ->
            fi.failed_load_event

        "invalid_event_id" ->
            fi.invalid_event_id

        "deleting" ->
            fi.deleting

        "delete" ->
            fi.delete

        "back_to_calendar_detail" ->
            fi.back_to_calendar_detail

        "loading_event" ->
            fi.loading_event

        "loading_events" ->
            fi.loading_events

        "failed_delete_event" ->
            fi.failed_delete_event

        "event_deleted_successfully" ->
            fi.event_deleted_successfully

        "confirm_delete_event" ->
            fi.confirm_delete_event

        "current_image" ->
            fi.current_image

        "saving" ->
            fi.saving

        "save_changes" ->
            fi.save_changes

        "failed_update_event" ->
            fi.failed_update_event

        "event_updated_successfully" ->
            fi.event_updated_successfully

        "draft" ->
            fi.draft

        "pending" ->
            fi.pending

        "published" ->
            fi.published

        "deleted" ->
            fi.deleted

        "organization_alt" ->
            fi.organization_alt

        "calendar_title" ->
            fi.calendar_title

        "hello" ->
            fi.hello

        "logout" ->
            fi.logout

        "login" ->
            fi.login

        "rss_feed" ->
            fi.rss_feed

        "html_feed" ->
            fi.html_feed

        "ical_feed" ->
            fi.ical_feed

        "atom_feed" ->
            fi.atom_feed

        "json_feed" ->
            fi.json_feed

        "geojson_feed" ->
            fi.geojson_feed

        "ical_description" ->
            fi.ical_description

        "html_description" ->
            fi.html_description

        "feeds_description" ->
            fi.feeds_description

        "completing_login" ->
            fi.completing_login

        "non_member_prefix" ->
            fi.non_member_prefix

        "send_event_email" ->
            fi.send_event_email

        "error_page_title" ->
            fi.error_page_title

        "something_went_wrong" ->
            fi.something_went_wrong

        "page_not_found" ->
            fi.page_not_found

        "page_not_found_description" ->
            fi.page_not_found_description

        "error_page_description" ->
            fi.error_page_description

        "error_details" ->
            fi.error_details

        "reload_page" ->
            fi.reload_page

        "component_error_title" ->
            fi.component_error_title

        "component_error_description" ->
            fi.component_error_description

        "try_again" ->
            fi.try_again

        "title_required_error" ->
            fi.title_required_error

        "start_date_required_error" ->
            fi.start_date_required_error

        "end_date_after_start_error" ->
            fi.end_date_after_start_error

        "url_invalid_error" ->
            fi.url_invalid_error

        "latitude_invalid_error" ->
            fi.latitude_invalid_error

        "longitude_invalid_error" ->
            fi.longitude_invalid_error

        _ ->
            key
