#!/bin/bash
set -euo pipefail

SCRIPT_DIR=$(realpath "$(dirname "${BASH_SOURCE[0]}")")

ENV_FILE=$SCRIPT_DIR/.env

if [ -f $ENV_FILE ]; then
  . $ENV_FILE
else
  echo ".env file not found"
  exit 1
fi

: "${DEPLOY_HOST:?DEPLOY_HOST not set}"
: "${DEPLOY_PATH:?DEPLOY_PATH not set}"

$SCRIPT_DIR/build.sh

rsync --delete --archive www/ "${DEPLOY_HOST}:${DEPLOY_PATH}"

echo "Uploaded files to VPS."
