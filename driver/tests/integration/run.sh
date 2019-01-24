#!/bin/sh
set -e

HELLO_WORLD_SOURCE=driver/tests/integration/hello-world.arret
EXPECTED_HELLO_WORLD_OUTPUT="Hello, world!"
TEMP_HELLO_WORLD_BINARY=target/hello-world

test_binary=${1:-cargo run}

assert_outputs_hello_world()
{
    echo $1
    output=$($1)
    if [ "${output}" != "${EXPECTED_HELLO_WORLD_OUTPUT}" ]; then
        >&2 echo "expected '${EXPECTED_HELLO_WORLD_OUTPUT}', got '${output}'"
        exit 1
    fi
}

assert_outputs_hello_world "${test_binary} eval ${HELLO_WORLD_SOURCE}"

assert_outputs_hello_world "${test_binary} eval -" < ${HELLO_WORLD_SOURCE}

${test_binary} compile ${HELLO_WORLD_SOURCE} -o "${TEMP_HELLO_WORLD_BINARY}"
assert_outputs_hello_world "${TEMP_HELLO_WORLD_BINARY}"
rm "${TEMP_HELLO_WORLD_BINARY}"
