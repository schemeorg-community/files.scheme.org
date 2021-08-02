#!/bin/sh
set -eu
cd "$(dirname "$0")"
./index.sh
set -x
rsync -crv www/ alpha.servers.scheme.org:/production/files/www/
rsync -rv --size-only files/ alpha.servers.scheme.org:/production/files/www/
