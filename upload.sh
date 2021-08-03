#!/bin/sh
set -eu
cd "$(dirname "$0")"
./index.sh
set -x
curl --location --fail --silent --show-error -o www/schemeorg.css \
    https://www.staging.scheme.org/schemeorg.css
rsync -crv www/ alpha.servers.scheme.org:/production/files/www/
rsync -rv --size-only files/ alpha.servers.scheme.org:/production/files/www/
