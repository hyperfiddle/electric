#!/bin/bash

echo "Running JVM tests"
clojure -X:test \
    :dirs "[\"src\" \"test\"]"  \
    :patterns                                \
    "[\"hyperfiddle.((?!api|popover|txn|electric-fulcro|electric-httpkit|spool|spec).)*\"                 \
      \"contrib.((?!datomic).)*\"            \
     ]"

