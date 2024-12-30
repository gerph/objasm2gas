#!/bin/bash
##
# Test that the conversion works properly.
#

# Any failure is terminal
set -e

here=$(cd $(dirname "$0") && pwd -P)

# Find the output directory
output=
next_is_output=false

for arg in "$@" ; do
    if [[ "$arg" = '-o' ]] ; then
        next_is_output=true
    elif $next_is_output ; then
        next_is_output=false
        output=$arg
    fi
done

# Ensure that the output directory exists
if [[ "$output" != '' ]] ; then
    mkdir -p "$(dirname "$output")"
fi
"${here}"/arm2gas.pl "$@"

if [[ "$output" != '' ]] ; then
    # Check that it assembles
    if [[ ! -f "$output" ]] ; then
        echo "No output created" >&2
        exit 1
    fi

    AS="${AS:-arm-cortexa8_neon-linux-gnueabihf-as}"

    $AS "$output" -o /tmp/dummy
fi