#!/bin/sh

set -e

TARGET_TRIPLE=i686-unknown-linux-gnu

apt-get -y install gcc-multilib

rustup target add $TARGET_TRIPLE

# Cross-compile just stdlib
cargo build --target $TARGET_TRIPLE -p stdlib

# Only our "pass" tests will actually produce binaries for the target platform
ARRET_TEST_TARGET_TRIPLE=$TARGET_TRIPLE cargo test -p compiler pass
