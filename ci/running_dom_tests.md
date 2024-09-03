# How to run tests in a browser

## Running once
Run `./ci/run_tests_browser.sh`.

## Live reloading
```
clojure -M:test:shadow-cljs watch :browser-test
# in another shell:
./node_modules/.bin/karma start
```

Karma will:
- use Chrome if installed
- fallback to Chromium if installed
- download and install Chromium for you otherwise
