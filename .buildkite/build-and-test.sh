#!/bin/sh
set -eu

echo '--- :cargo: Compiling debug'
cargo build

echo '--- :pray: Testing debug'
cargo test

echo '--- :keyboard: Testing driver'
./driver/tests/integration/run.sh target/debug/arret
