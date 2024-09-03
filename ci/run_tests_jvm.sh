#!/bin/bash

echo "Running JVM tests"

# All namespaces are tested by default to encourage tested code.
# Use :ns-regexp to blacklist specific namespaces.
# ^(?!foo.(bar|baz)).* : includes everything except foo.bar or foo.baz

clojure -X:test \
    :dirs "[\"src\" \"test\"]" \
    :patterns "[\"^(?!hyperfiddle.(api|popover|txn|electric-fulcro|electric-httpkit|spool|spec)|contrib.(datomic|test.datomic)).*\"]"

