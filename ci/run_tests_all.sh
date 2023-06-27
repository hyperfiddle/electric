#!/bin/sh

./ci/run_tests_jvm.sh;          jvm_code=$?
./ci/run_tests_node.sh;         node_code=$?
./ci/run_tests_browser.sh "$@"; browser_code=$?

RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
RESET=$(tput sgr0)

reportResult() {
    if [ "$1" -eq 0 ]; then echo "${GREEN}ok${RESET}"; else echo "${RED}failed${RESET}"; fi
}

printf "\n --results--\n"
printf "JVM      %s\n" "$(reportResult $jvm_code)"
printf "node     %s\n" "$(reportResult $node_code)"
printf "browser  %s\n" "$(reportResult $browser_code)"

[ $jvm_code -eq 0 ] && [ $node_code -eq 0 ] && [ $browser_code -eq 0 ]
