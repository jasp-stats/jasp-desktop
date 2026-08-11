#!/bin/bash
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SLEEP_FILE="$SCRIPT_DIR/../build/Resources/Data Sets/Data Library/1. Descriptives/Sleep.jasp"

exec "$SCRIPT_DIR/run_test_session.sh" \
  --test "$SCRIPT_DIR/test_results_accessibility.py" \
  --jasp-args "$SLEEP_FILE" \
  --wait 20 \
  "$@"