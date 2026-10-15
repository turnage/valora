#!/bin/sh
# Builds the page into dist/, which the valora server embeds.
set -e
cd "$(dirname "$0")"
trunk build
