#!/bin/bash
set -euo pipefail
# Test the playground in a headless browser via Puppeteer.
# Run inside Docker: docker run --rm opencascade-wasm bash scripts/test_browser.sh
# Requires: npm install puppeteer (done in Dockerfile)

source ~/.ghc-wasm/env

# Start the HTTP server
cd /opencascade-hs/playground/dist
python3 -m http.server 8080 &>/dev/null &
SERVER_PID=$!
sleep 1

# Run the Puppeteer test from the puppeteer install directory
cd /opt/puppeteer
node --experimental-wasm-type-reflection /opencascade-hs/scripts/test_browser.mjs 2>&1
EXIT=$?

kill $SERVER_PID 2>/dev/null
exit $EXIT
