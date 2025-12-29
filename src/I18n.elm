module I18n exposing (get)

import Translations exposing (fi)


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

        "of_" ->
            fi.of_

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

        "home" ->
            fi.home

        "map" ->
            fi.map

        "events" ->
            fi.events

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
