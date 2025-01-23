#!/bin/sh
set -eux -o pipefail

clojure -T:build clean
HYPERFIDDLE_ELECTRIC_BUILD="v3-alpha-SNAPSHOT"
clojure -T:build jar :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
clojure -T:build install :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
#env $(cat .env | xargs) clojure -T:build deploy :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'