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
gas_used=false

for arg in "$@" ; do
    if [[ "$arg" = '--gas' ]] ; then
        gas_used=true
    elif [[ "$arg" = '-o' ]] ; then
        next_is_output=true
    elif $next_is_output ; then
        next_is_output=false
        output=$arg
    fi
done

# Ensure that the output directory exists
if [[ "$output" != '' && ! "$output" =~ nodir ]] ; then
    mkdir -p "$(dirname "$output")"
    # Also create the expectation directory, if we can.
    expect=${output/gas/expect}
    mkdir -p "$(dirname "$expect")"
fi
"${here}"/objasm2gas.pl "$@"

if [[ "$output" != '' ]] && ! $gas_used ; then
    # Check that it assembles
    if [[ ! -f "$output" ]] ; then
        echo "No output created" >&2
        exit 1
    fi

    if [[ "$output" =~ aarch64 ]] ; then
        AS="${AS64:-aarch64-unknown-linux-gnu-as}"
    else
        AS="${AS:-arm-cortexa8_neon-linux-gnueabihf-as}"
    fi

    $AS "$output" -o /tmp/dummy
fi
