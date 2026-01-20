#!/bin/bash
set -e

if [ -f .env ] then
  . ./.env
else
  echo ".env file not found"
  exit 1
fi

: "${DEPLOY_HOST:?DEPLOY_HOST not set}"
: "${DEPLOY_PATH:?DEPLOY_PATH not set}"

./build.sh
rsync --delete --archive www/ "${DEPLOY_HOST}:${DEPLOY_PATH}"

echo "Uploaded files to VPS."
