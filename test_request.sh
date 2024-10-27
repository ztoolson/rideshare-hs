#!/bin/bash

# 1. Sign up and save the response
RESPONSE=$(curl -X POST http://localhost:3000/api/auth/signup \
  -H "Content-Type: application/json" \
  -d '{
    "email": "ridertest@example.com",
    "password": "password123",
    "firstName": "Test",
    "lastName": "Rider",
    "type": "rider"
  }')

# If signup fails due to existing user, try login
if echo "$RESPONSE" | jq -e '.error' > /dev/null; then
    echo "User exists, trying login..."
    RESPONSE=$(curl -X POST http://localhost:3000/api/auth/login \
      -H "Content-Type: application/json" \
      -d '{
        "email": "ridertest@example.com",
        "password": "password123"
      }')
fi

# Extract token and userId using jq
TOKEN=$(echo $RESPONSE | jq -r '.token')
USER_ID=$(echo $RESPONSE | jq -r '.userId')

echo "Token: $TOKEN"
echo "User ID: $USER_ID"

# Check if we got a valid token
if [ "$TOKEN" = "null" ] || [ -z "$TOKEN" ]; then
    echo "Failed to get valid token"
    echo "Response was: $RESPONSE"
    exit 1
fi

# 2. Create trip request using saved token and user_id
TRIP_RESPONSE=$(curl -X POST http://localhost:3000/api/trip_requests \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d "{
    \"riderId\": $USER_ID,
    \"startAddress\": \"123 Start St\",
    \"endAddress\": \"456 End Ave\"
  }")

echo "Trip Response: $TRIP_RESPONSE"
