#!/bin/sh
set -eu
cd "$(dirname "$0")"
./index.sh
set -x
curl --location --fail --silent --show-error -o www/schemeorg.css \
    https://www.scheme.org/schemeorg.css
rsync -crv www/ tuonela.scheme.org:/production/files/www/
rsync -rv --size-only files/ tuonela.scheme.org:/production/files/www/
