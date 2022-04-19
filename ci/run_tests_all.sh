#!/bin/sh

./ci/run_tests_jvm.sh
./ci/run_tests_node.sh
./ci/run_tests_browser.sh $@