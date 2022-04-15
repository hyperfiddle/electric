# How to run tests in a browser during dev time

## Running once
Use the `run_tests_browser.sh` script.

## Live reloading
1. Start a watch build for `:browser-test` (either in shadow ui, or `shadow-cljs -A:test watch :browser-test`)
2. Run `./node_modules/.bin/karma start --browsers Chrome`