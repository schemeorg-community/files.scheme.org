#!/bin/sh
set -eu
cd "$(dirname "$0")"
./index.sh
set -x
rsync -vr files/ alpha.servers.scheme.org:/production/files/www/
