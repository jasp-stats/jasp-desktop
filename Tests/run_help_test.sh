#!/bin/bash
exec "$(dirname "$0")/run_test_session.sh" --test "$(dirname "$0")/test_help_accessibility.py" "$@"