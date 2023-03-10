#!/bin/sh -x

echo "Running Browser tests"
clj -M:dev:test:shadow-cljs compile :browser-test --force-spawn
./node_modules/.bin/karma start --single-run $@ # --browsers Chrome
