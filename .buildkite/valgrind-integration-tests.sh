#!/bin/sh
set -eu

echo '--- :boom: Running integration tests in Valgrind'

# Valgrind serialises all execution using user space locks.
# We can can some efficiency by telling Rayon to not use multiple threads.
export RAYON_NUM_THREADS=1

# This is a hack; `cargo` doesn't give us an easy way to wrap our tests
cd compiler
valgrind --leak-check=no --error-exitcode=42 ../target/debug/integration-????????????????
