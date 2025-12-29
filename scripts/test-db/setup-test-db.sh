#!/usr/bin/env bash
# Test database setup script for PocketBase

set -e

PB_DIR=".pocketbase"
PB_PORT="8090"
PB_URL="http://127.0.0.1:$PB_PORT"

echo "Setting up test database..."

# Create PocketBase directory if it doesn't exist
mkdir -p "$PB_DIR"

# Initialize PocketBase (this creates the initial schema)
echo "Initializing PocketBase..."
pocketbase migrate --dir="$PB_DIR" || true

# Create admin user
echo "Creating admin user..."
pocketbase --dir="$PB_DIR" superuser upsert test@example.com testpassword || echo "Admin user creation failed"

# Start PocketBase in background
echo "Starting PocketBase server..."
pocketbase serve --dir="$PB_DIR" --http="127.0.0.1:$PB_PORT" &
PB_PID=$!

# Wait for PocketBase to be ready
echo "Waiting for PocketBase to be ready..."
timeout=60
while ! curl -s "$PB_URL/api/health" > /dev/null; do
    if [ $timeout -le 0 ]; then
        echo "PocketBase failed to start"
        kill $PB_PID 2>/dev/null || true
        exit 1
    fi
    sleep 1
    timeout=$((timeout - 1))
done

echo "PocketBase is ready at $PB_URL"

# Wait a bit more for admin endpoints to be available
sleep 5

# Get admin token
echo "Getting admin token..."
AUTH_RESPONSE=$(curl -s -X POST "$PB_URL/api/admin/auth-with-password" \
  -H "Content-Type: application/json" \
  -d '{
    "identity": "test@example.com",
    "password": "testpassword"
  }')

echo "Auth response: $AUTH_RESPONSE"
TOKEN=$(echo "$AUTH_RESPONSE" | jq -r '.token' 2>/dev/null || echo "")

if [ -z "$TOKEN" ] || [ "$TOKEN" = "null" ]; then
    echo "Trying alternative auth endpoint..."
    AUTH_RESPONSE=$(curl -s -X POST "$PB_URL/api/admins/auth-with-password" \
      -H "Content-Type: application/json" \
      -d '{
        "identity": "test@example.com",
        "password": "testpassword"
      }')
    echo "Alternative auth response: $AUTH_RESPONSE"
    TOKEN=$(echo "$AUTH_RESPONSE" | jq -r '.token' 2>/dev/null || echo "")
fi

if [ -z "$TOKEN" ] || [ "$TOKEN" = "null" ]; then
    echo "Failed to get admin token"
    kill $PB_PID 2>/dev/null || true
    exit 1
fi

echo "Admin token obtained: ${TOKEN:0:20}..."

# Create events collection if it doesn't exist
echo "Creating events collection..."
RESPONSE=$(curl -s -X POST "$PB_URL/api/collections" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "events",
    "type": "base",
    "schema": [
      {
        "name": "title",
        "type": "text",
        "required": true
      },
      {
        "name": "description",
        "type": "text"
      },
      {
        "name": "start_date",
        "type": "date",
        "required": true
      },
      {
        "name": "end_date",
        "type": "date",
        "required": true
      },
      {
        "name": "start_time",
        "type": "text"
      },
      {
        "name": "end_time",
        "type": "text"
      },
      {
        "name": "all_day",
        "type": "bool",
        "default": false
      },
      {
        "name": "location",
        "type": "text"
      },
      {
        "name": "point",
        "type": "json"
      },
      {
        "name": "coordinates",
        "type": "json"
      },
      {
        "name": "url",
        "type": "url"
      },
      {
        "name": "image",
        "type": "file",
        "maxSelect": 1,
        "maxSize": 5242880
      },
      {
        "name": "image_description",
        "type": "text"
      },
      {
        "name": "state",
        "type": "select",
        "values": ["draft", "published", "deleted"],
        "default": "draft"
      }
    ]
  }')

echo "Collection creation response: $RESPONSE"

# If that failed, try with token
if ! echo "$RESPONSE" | grep -q '"name":"events"'; then
    echo "Trying with admin token..."
    RESPONSE=$(curl -s -X POST "$PB_URL/api/collections" \
      -H "Authorization: Bearer $TOKEN" \
      -H "Content-Type: application/json" \
      -d '{
        "name": "events",
        "type": "base",
        "schema": [
          {
            "name": "title",
            "type": "text",
            "required": true
          },
          {
            "name": "description",
            "type": "text"
          },
          {
            "name": "start_date",
            "type": "date",
            "required": true
          },
          {
            "name": "end_date",
            "type": "date",
            "required": true
          },
          {
            "name": "start_time",
            "type": "text"
          },
          {
            "name": "end_time",
            "type": "text"
          },
          {
            "name": "all_day",
            "type": "bool",
            "default": false
          },
          {
            "name": "location",
            "type": "text"
          },
          {
            "name": "point",
            "type": "json"
          },
          {
            "name": "coordinates",
            "type": "json"
          },
          {
            "name": "url",
            "type": "url"
          },
          {
            "name": "image",
            "type": "file",
            "maxSelect": 1,
            "maxSize": 5242880
          },
          {
            "name": "image_description",
            "type": "text"
          },
          {
            "name": "state",
            "type": "select",
            "values": ["draft", "published", "deleted"],
            "default": "draft"
          }
        ]
      }')
    echo "Collection creation with token response: $RESPONSE"
fi

# Check if collection was created or already exists
if echo "$RESPONSE" | grep -q '"name":"events"'; then
    echo "Events collection created successfully"
elif echo "$RESPONSE" | grep -q '"data":{},"message":"The request requires valid record authorization token"'; then
    echo "Admin token is invalid"
    kill $PB_PID 2>/dev/null || true
    exit 1
else
    echo "Collection may already exist or creation failed"
fi

# Import test events from fixture
echo "Importing test events from fixture..."
POCKETBASE_URL="$PB_URL" POCKETBASE_TOKEN="$TOKEN" pnpm import-test-db

echo "Test database setup complete!"
echo "PocketBase is running at $PB_URL"
echo "PID: $PB_PID"
echo "Press Ctrl+C to stop"

# Keep PocketBase running
wait $PB_PID