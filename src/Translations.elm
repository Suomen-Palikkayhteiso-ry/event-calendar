module Translations exposing (Translations, fi)


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
    , of_ : String
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
    , home : String
    , map : String
    , events : String
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
    , geocoding_failed = "Geokoodaus epäonnistui"
    , disable_geocoding = "Poista geokoodaus käytöstä"
    , enable_geocoding = "Ota geokoodaus käyttöön"
    , description_label = "Kuvaus"
    , description_optional = "Kuvaus (valinnainen)"
    , dates = "Päivämäärät"
    , actions = "Toiminnot"
    , url_label = "URL"
    , url_optional = "URL (valinnainen)"
    , image_label = "Kuva"
    , image_description_label = "Kuvan kuvaus"
    , image_description_optional = "Kuvan kuvaus (valinnainen)"
    , image_help_text = "Kuva näytetään tapahtuman yksityiskohdat -sivulla. Suositeltu koko on 800x600 pikseliä."
    , view_on_map = "Näytä kartalla"
    , edit_event = "Muokkaa tapahtumaa"
    , previous_page = "Edellinen sivu"
    , next_page = "Seuraava sivu"
    , events_table = "Tapahtumat"
    , start_date_required = "Aloituspäivä *"
    , end_date = "Lopetuspäivä"
    , all_day_event_label = "Koko päivän tapahtuma"
    , creating = "Luodaan..."
    , create_event = "Luo tapahtuma"
    , cancel = "Peruuta"
    , existing_events = "Olemassa olevat tapahtumat"
    , edit = "Muokkaa"
    , all_day = "Koko päivä"
    , status = "Tila"
    , previous = "Edellinen"
    , page = "Sivu"
    , of_ = "/"
    , total_events = "Tapahtumaa yhteensä"
    , failed_create_event = "Tapahtuman luonti epäonnistui"
    , event_created_successfully = "Tapahtuma luotu onnistuneesti"
    , failed_fetch_events = "Tapahtumien haku epäonnistui"
    , failed_kml_import = "KML-tuonti epäonnistui"
    , failed_load_event = "Tapahtuman lataus epäonnistui"
    , invalid_event_id = "Virheellinen tapahtuman ID"
    , deleting = "Poistetaan..."
    , delete = "Poista"
    , back_to_calendar_detail = "Takaisin tapahtuman yksityiskohtiin"
    , loading_event = "Ladataan tapahtumaa..."
    , loading_events = "Ladataan tapahtumia..."
    , failed_delete_event = "Tapahtuman poisto epäonnistui"
    , event_deleted_successfully = "Tapahtuma poistettu onnistuneesti"
    , confirm_delete_event = "Haluatko varmasti poistaa tämän tapahtuman?"
    , current_image = "Nykyinen kuva"
    , saving = "Tallennetaan..."
    , save_changes = "Tallenna muutokset"
    , failed_update_event = "Tapahtuman päivitys epäonnistui"
    , event_updated_successfully = "Tapahtuma päivitetty onnistuneesti"
    , draft = "Luonnos"
    , pending = "Odottaa"
    , published = "Julkaistu"
    , deleted = "Poistettu"
    , organization_alt = "Suomen Palikkayhteisö ry"
    , calendar_title = "Kalenteri"
    , hello = "Hei"
    , logout = "Kirjaudu ulos"
    , login = "Kirjaudu sisään"
    , home = "Koti"
    , map = "Kartta"
    , events = "Tapahtumat"
    , rss_feed = "RSS-syöte"
    , html_feed = "HTML-syöte"
    , ical_feed = "iCal-syöte"
    , atom_feed = "Atom-syöte"
    , json_feed = "JSON-syöte"
    , geojson_feed = "GeoJSON-syöte"
    , ical_description = "Lisää tämä kalenteri kalenterisovellukseesi iCal-muodossa"
    , html_description = "Näytä tämä kalenteri HTML-muodossa"
    , feeds_description = "Syötteet"
    , completing_login = "Viimeistellään kirjautumista..."
    , non_member_prefix = "Ei-jäsen:"
    , send_event_email = "Lähetä tapahtuma sähköpostilla"
    , error_page_title = "Virhe"
    , something_went_wrong = "Jotain meni pieleen"
    , page_not_found = "Sivua ei löytynyt"
    , page_not_found_description = "Etsimääsi sivua ei löytynyt."
    , error_page_description = "Tapahtui odottamaton virhe."
    , error_details = "Virheen yksityiskohdat"
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