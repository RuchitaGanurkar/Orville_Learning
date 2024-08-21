#!/bin/bash

set -o allexport
source .env
set +o allexport

# Run Docker Compose
docker-compose up