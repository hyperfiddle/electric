# How to run tests in a browser

## Running once
Run `./ci/run_tests_browser.sh`.

## Live reloading
1. Start a watch build for `:browser-test`, either:
    - in Shadow UI
    - at the REPL `(shadow/watch :browser-test)`
    - from a shell `shadow-cljs -A:test watch :browser-test`
2. Run `./node_modules/.bin/karma start --browsers Chrome`
   Karma will:
   - use Chrome if installed
   - fallback to Chromium if installed
   - download and install Chromium for you otherwise