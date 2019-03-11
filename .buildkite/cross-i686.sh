#!/bin/sh

set -e

TARGET_TRIPLE=i686-unknown-linux-gnu
. ./driver/tests/integration/config.sh

apt-get -y install gcc-multilib file

rustup target add $TARGET_TRIPLE

# Cross-compile just stdlib
cargo build --target $TARGET_TRIPLE -p stdlib

# Only our "pass" tests will actually produce binaries for the target platform
ARRET_TEST_TARGET_TRIPLE=$TARGET_TRIPLE cargo test -p compiler pass

# Build a standalone binary
./target/debug/arret compile --target $TARGET_TRIPLE "${HELLO_WORLD_SOURCE}" -o "${TEMP_HELLO_WORLD_BINARY}"

# Make sure this is actually a 32bit binary
file "$TEMP_HELLO_WORLD_BINARY" | fgrep "Intel 80386"

# And it outputs what we expect
output=$("${TEMP_HELLO_WORLD_BINARY}")
if [ "${output}" != "${EXPECTED_HELLO_WORLD_OUTPUT}" ]; then
	>&2 echo "expected '${EXPECTED_HELLO_WORLD_OUTPUT}', got '${output}'"
	exit 1
fi

rm "${TEMP_HELLO_WORLD_BINARY}"
