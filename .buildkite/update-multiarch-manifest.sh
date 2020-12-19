#!/usr/bin/env bash
set -eu

# Needed for `docker manifest`
export DOCKER_CLI_EXPERIMENTAL=enabled

manifest=ghcr.io/etaoins/arret-repl

docker manifest create -a "${manifest}" "${manifest}-arm64" "${manifest}-amd64"
docker manifest push "${manifest}"