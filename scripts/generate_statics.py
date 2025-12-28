#!/usr/bin/env python3
"""
Main script for generating static files for the event calendar
Python port of generate-statics.js
"""

from datetime import datetime
from generate_utils import *
from generate_embed import generate_embed
from generate_feeds import generate_feeds
from generate_ics_geojson import generate_ics_calendar, generate_geojson

def generate_statics():
    """Generate all static files for the event calendar"""
    print("Starting static generation...")

    # Create PocketBase client and fetch events
    pb_url = create_pocketbase_client()
    events = fetch_published_events(pb_url)

    print(f"Fetched {len(events)} published events")

    # Generate all static files
    generate_embed(events)
    generate_feeds(events)
    generate_ics_calendar(events)
    generate_geojson(events)

    print(f"Completed static generation for {len(events)} events")

if __name__ == '__main__':
    generate_statics()