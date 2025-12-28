import os
import json
import requests
from datetime import datetime, timezone, timedelta
from pathlib import Path

POCKETBASE_URL = 'https://data.suomenpalikkayhteiso.fi'

DAY_ABBR = ['su', 'ma', 'ti', 'ke', 'to', 'pe', 'la']

def to_helsinki_date(utc_string):
    """Convert UTC string to Helsinki timezone datetime"""
    if not utc_string.endswith('Z'):
        utc_string += 'Z'
    utc_date = datetime.fromisoformat(utc_string.replace('Z', '+00:00'))
    # For simplicity, assuming UTC+2 for Helsinki (no DST handling for now)
    helsinki_offset = 2 * 3600  # 2 hours in seconds
    helsinki_date = utc_date.replace(tzinfo=timezone.utc).astimezone(timezone(timedelta(seconds=helsinki_offset)))
    return helsinki_date.replace(tzinfo=None)

def to_utc_date(utc_string):
    """Convert UTC string to datetime object"""
    if not utc_string.endswith('Z'):
        utc_string += 'Z'
    return datetime.fromisoformat(utc_string.replace('Z', '+00:00'))

def create_pocketbase_client():
    """Create a PocketBase client (using requests for now)"""
    return POCKETBASE_URL

def fetch_published_events(pb_url):
    """Fetch published events from PocketBase"""
    yesterday = datetime.now() - timedelta(days=1)
    yesterday_iso = yesterday.strftime('%Y-%m-%d') + ' 00:00:00'

    url = f"{pb_url}/api/collections/events/records"
    params = {
        'filter': f'state = "published" && (end_date > "{yesterday_iso}" || start_date > "{yesterday_iso}")',
        'sort': 'start_date'
    }

    response = requests.get(url, params=params)
    response.raise_for_status()
    data = response.json()
    return data['items']

def format_date_in_helsinki(utc_string, all_day=False):
    """Format date in Helsinki timezone"""
    helsinki_date = to_helsinki_date(utc_string)
    day = str(helsinki_date.day)
    month = str(helsinki_date.month)
    date_str = f"{day}.{month}."

    if all_day:
        return date_str

    hours = str(helsinki_date.hour).zfill(2)
    minutes = str(helsinki_date.minute).zfill(2)
    time_str = f"{hours}.{minutes}"
    return f"{date_str} {time_str}"

def format_event_display_date(event):
    """Format event display date with proper range handling"""
    start_formatted = format_date_in_helsinki(event['start_date'], event.get('all_day', False))
    date_str = start_formatted

    if event.get('end_date'):
        end_formatted = format_date_in_helsinki(event['end_date'], event.get('all_day', False))
        if start_formatted != end_formatted:
            if event.get('all_day', False):
                start_parts = start_formatted.split('.')
                end_parts = end_formatted.split('.')
                if start_parts[1] == end_parts[1]:  # Same month
                    date_str = f"{start_parts[0]}.–{end_parts[0]}.{start_parts[1]}."
                else:
                    date_str += f"–{end_formatted}"
            else:
                start_date_part = start_formatted.split(' ')[0]
                end_date_part = end_formatted.split(' ')[0]
                if start_date_part == end_date_part:  # Same day
                    end_time = end_formatted.split(' ')[1]
                    date_str += f"–{end_time}"
                else:
                    date_str += f"–{end_formatted}"

    if event.get('all_day', False):
        start_utc = to_utc_date(event['start_date'])
        start_helsinki = to_helsinki_date(event['start_date'])
        prefix = ''

        if event.get('end_date') and event['end_date'] != event['start_date']:
            end_utc = to_utc_date(event['end_date'])
            end_helsinki = to_helsinki_date(event['end_date'])
            duration_days = (end_utc - start_utc).days + 1
            if duration_days < 8:
                prefix = f"{DAY_ABBR[start_helsinki.weekday()]}–{DAY_ABBR[end_helsinki.weekday()]} "
        else:
            prefix = f"{DAY_ABBR[start_helsinki.weekday()]} "

        return prefix + date_str

    # Handle single day events
    start_date_part = start_formatted.split(' ')[0]
    end_date_part = format_date_in_helsinki(event.get('end_date', event['start_date']), False).split(' ')[0] if event.get('end_date') else None
    is_single_day = not event.get('end_date') or start_date_part == end_date_part

    if is_single_day:
        start_helsinki = to_helsinki_date(event['start_date'])
        parts = date_str.split(' ')
        if len(parts) == 2:
            return f"{DAY_ABBR[start_helsinki.weekday()]} {parts[0]} klo {parts[1]}"

    return date_str

def write_static_file(relative_path, content):
    """Write content to static file in both static/ and build/ directories"""
    static_path = Path('static') / relative_path
    static_path.parent.mkdir(parents=True, exist_ok=True)

    if isinstance(content, str):
        static_path.write_text(content, encoding='utf-8')
    else:
        # Assume bytes
        static_path.write_bytes(content)

    # Also write to build directory if it exists
    if Path('build').exists():
        build_path = Path('build') / relative_path
        build_path.parent.mkdir(parents=True, exist_ok=True)
        if isinstance(content, str):
            build_path.write_text(content, encoding='utf-8')
        else:
            build_path.write_bytes(content)