#!/bin/sh
set -eu
cd "$(dirname "$0")"
gosh -I . manifest-get.scm "$@"
