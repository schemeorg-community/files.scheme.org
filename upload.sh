#!/bin/sh
set -eu
cd "$(dirname "$0")"
./index.sh
set -x
curl --location --fail --silent --show-error -o www/schemeorg.css \
    https://www.scheme.org/schemeorg.css
rsync -crv www/ scheme.org:/var/www/files.scheme.org/
rsync -rv --size-only files/ scheme.org:/var/www/files.scheme.org/
