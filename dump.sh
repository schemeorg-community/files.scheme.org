#!/bin/sh
set -eu
cd "$(dirname "$0")"
gosh -I . dump.scm
