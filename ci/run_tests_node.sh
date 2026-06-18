#!/bin/sh

echo "Running NodeJS tests"
clojure -M:test:shadow-cljs compile :test --force-spawn "$@" || exit 1

# Run node, streaming output to the terminal while capturing it for inspection.
# Capture node's own exit status — a bare pipe to tee would mask it.
log=$(mktemp)
code_file=$(mktemp)
{ node out/node-tests.js 2>&1; echo $? >"$code_file"; } | tee "$log"
node_code=$(cat "$code_file")
rm -f "$code_file"

if [ "$node_code" -ne 0 ]; then
    rm -f "$log"
    echo "node tests failed (exit $node_code)"
    exit "$node_code"
fi

# A run that aborts mid-suite (e.g. an async test that throws before calling
# cljs.test `done`) exits 0 without printing the cljs.test summary. Treat a
# missing 'Ran N tests' line as failure so a stalled run can't pass as green.
if ! grep -q '^Ran [0-9]' "$log"; then
    rm -f "$log"
    echo "node tests aborted before completion — no cljs.test summary ('Ran N tests')"
    exit 1
fi

rm -f "$log"
