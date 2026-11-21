#!/bin/sh
set -e
cd "$(dirname "$0")/.."

IMAGE=opencascade-hs-occt8
docker build -q -t "$IMAGE" - < Dockerfile.occt8 > /dev/null

if [ -t 0 ]; then TTY="-it"; else TTY=""; fi
exec docker run --rm $TTY \
    -v "$PWD":/src \
    -v opencascade-hs-occt8-stack:/stack-root \
    "$IMAGE" "$@"
