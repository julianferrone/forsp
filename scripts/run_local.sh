#!/bin/bash

SCRIPT_DIR=$(realpath "$(dirname "${BASH_SOURCE[0]}")")
$SCRIPT_DIR/build.sh

python3 -m http.server --directory "${SCRIPT_DIR}/../www" 8000

