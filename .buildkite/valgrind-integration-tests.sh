#!/usr/bin/env bash
set -eu

echo '--- :boom: Running integration tests in Valgrind'

# We've already run our test executables and they should be identical under Valgrind
export ARRET_TEST_SKIP_RUN_EXECUTABLE=1

# Integration tests must be run from the compiler directory
cd compiler

# This is a hack; `cargo` doesn't give us an easy way to wrap our tests
integration_test_binaries=(../target/debug/integration-????????????????)

if [ ${#integration_test_binaries[@]} -ne '1' ]; then
  echo 'Could not find integration test binary'
  exit 1
fi

integration_test_binary=${integration_test_binaries[0]}

# Valgrind serialises all execution using user space locks.
# Hack around this by spawning a single-threaded test per core
export RAYON_NUM_THREADS=1

ARRET_TEST_NUM_WORKERS=$(nproc)
export ARRET_TEST_NUM_WORKERS

valgrind_pids=()
for ((i=0; i<ARRET_TEST_NUM_WORKERS; i++)); do
  ARRET_TEST_WORKER_ID=$i valgrind --leak-check=no --error-exitcode=42 "${integration_test_binary}" &
  valgrind_pids[${i}]=$!
done

for valgrind_pid in ${valgrind_pids[*]}; do
  wait $valgrind_pid
done