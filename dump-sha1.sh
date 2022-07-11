#!/bin/sh
set -eu
cd "$(dirname "$0")"
gosh -I . dump-sha1.scm
