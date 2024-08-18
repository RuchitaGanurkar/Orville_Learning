#!/bin/bash

# Set environment variables
export POSTGRES_DB="orville_project"
export POSTGRES_USER="ruchita"
export POSTGRES_PASSWORD="qwerty"
export DB_CONN_STRING="postgresql://ruchita:qwerty@database:5432/orville_project"

# Run Docker Compose
docker-compose up