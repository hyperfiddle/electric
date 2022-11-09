#!/bin/sh -x

echo "Running Browser tests"
./node_modules/.bin/shadow-cljs -A:dev:test compile :browser-test --force-spawn
./node_modules/.bin/karma start --single-run $@ # --browsers Chrome
