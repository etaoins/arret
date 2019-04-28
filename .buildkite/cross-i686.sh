#!/bin/sh

set -e

TARGET_TRIPLE=i686-unknown-linux-gnu
. ./driver/tests/integration/config.sh

# Check that the project would build on i686. This ensures we're not making assumptions about e.g.
# pointer width
echo '--- :older_woman: Checking i686 debug'
cargo check --target $TARGET_TRIPLE

# Cross-compile just stdlib. Otherwise we would need a 32bit LLVM etc.
echo '--- :older_man: Testing i686 cross-compile'
cargo build --target $TARGET_TRIPLE -p arret-stdlib

# Only our integration tests will actually produce binaries for the target platform
ARRET_TEST_TARGET_TRIPLE=$TARGET_TRIPLE cargo test -p arret-compiler integration

# Build a standalone binary
./target/debug/arret compile --target $TARGET_TRIPLE "${HELLO_WORLD_SOURCE}" -o "${TEMP_HELLO_WORLD_BINARY}"

# Make sure it's actually a 32bit binary
file "$TEMP_HELLO_WORLD_BINARY" | fgrep "Intel 80386"

# And it outputs what we expect
output=$("${TEMP_HELLO_WORLD_BINARY}")
if [ "${output}" != "${EXPECTED_HELLO_WORLD_OUTPUT}" ]; then
	>&2 echo "expected '${EXPECTED_HELLO_WORLD_OUTPUT}', got '${output}'"
	exit 1
fi

rm "${TEMP_HELLO_WORLD_BINARY}"
