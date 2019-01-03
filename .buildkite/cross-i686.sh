#!/bin/sh

set -e

TARGET_TRIPLE=i686-unknown-linux-gnu

apt-get update
apt-get -y install clang-7 gcc-multilib

# Use Clang as it understands LLVM target triples
update-alternatives --install /usr/bin/cc cc /usr/bin/clang-7 100

rustup target add $TARGET_TRIPLE

# Cross-compile just stdlib
cargo build --target $TARGET_TRIPLE -p stdlib

# Only our "pass" tests will actually produce binaries for the target platform
ARRET_TEST_TARGET_TRIPLE=$TARGET_TRIPLE cargo test -p compiler pass
