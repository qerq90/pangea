#!/bin/sh
set -e

GOOSE_URL="postgres://${POSTGRES_USER}:${POSTGRES_PASSWORD}@postgres:5432/${POSTGRES_DB}?sslmode=disable"

echo "Running migrations..."
goose -dir /app/migrations postgres "$GOOSE_URL" up

echo "Starting application..."
exec java -jar /app/app.jar
