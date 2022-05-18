#!/bin/sh

echo "Running Browser tests"
./node_modules/.bin/shadow-cljs -A:test compile :browser-test --force-spawn
./node_modules/.bin/karma start --single-run $@
