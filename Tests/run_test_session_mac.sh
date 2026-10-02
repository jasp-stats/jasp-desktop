#!/bin/bash
set -u
#
# run_test_session_mac.sh – macOS variant of run_test_session.sh.
#
# No Xvfb / dbus / at-spi: JASP accessibility is exercised through the
# native macOS AX API (VoiceOver's channel) via the pyobjc backend in
# Tests/a11y_backends/ax_backend.py.
#
# Usage:
#   run_test_session_mac.sh --test <script.py>
#       [--jasp-args <args>] [--wait <seconds>]
#       [--jasp-config key=value ...] [--keep-jasp]
#
# Requirements:
#   - The process running this script (Terminal/IDE/agent) needs
#     *Accessibility* permission (System Settings → Privacy & Security →
#     Accessibility) for AX queries and CGEvent key synthesis.
#

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"           # .../Broncode/JASP/jasp-desktop
JASP_ROOT="$(cd "$REPO_ROOT/.." && pwd)"            # .../Broncode/JASP

# ── defaults (overridable via env) ────────────────────────────────────
JASP_BIN="${JASP_BIN:-$REPO_ROOT/build-screenreader/Desktop/JASP}"
PYTHON_BIN="${PYTHON_BIN:-$SCRIPT_DIR/a11y-venv-mac/bin/python3}"
LOG_FILE="${LOG_FILE:-/tmp/jasp_mac_test.log}"

TEST_SCRIPT=""
JASP_ARGS=""
WAIT_SEC="${WAIT_SEC:-10}"
JASP_CONFIG_VARS=()
KEEP_JASP=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --test)         TEST_SCRIPT="$2"; shift 2 ;;
        --jasp-args)    JASP_ARGS="$2"; shift 2 ;;
        --wait)         WAIT_SEC="$2"; shift 2 ;;
        --jasp-config)  JASP_CONFIG_VARS+=("$2"); shift 2 ;;
        --keep-jasp)    KEEP_JASP=true; shift ;;
        *)
            echo "Unknown flag: $1"
            echo "Usage: $0 --test <script.py> [--jasp-args ...] [--wait N] [--jasp-config k=v] [--keep-jasp]"
            exit 2 ;;
    esac
done

if [[ -z "$TEST_SCRIPT" ]]; then echo "FATAL: --test is required"; exit 1; fi
if [[ ! -f "$TEST_SCRIPT" ]]; then echo "FATAL: test script not found: $TEST_SCRIPT"; exit 1; fi
if [[ ! -x "$JASP_BIN" ]]; then echo "FATAL: JASP binary not found: $JASP_BIN"; exit 1; fi

TEST_NAME="$(basename "$TEST_SCRIPT" .py)"

# ── JASP config (optional) ───────────────────────────────────────────
# macOS QSettings stores in ~/Library/Preferences/org.jasp-stats.JASP.plist
if [[ ${#JASP_CONFIG_VARS[@]} -gt 0 ]]; then
    DOMAIN="org.jasp-stats.JASP"
    for kv in "${JASP_CONFIG_VARS[@]}"; do
        key="${kv%%=*}"; val="${kv#*=}"
        if [[ "$val" == "true" || "$val" == "false" ]]; then
            defaults write "$DOMAIN" "$key" -bool "$val"
        elif [[ "$val" =~ ^-?[0-9]+$ ]]; then
            defaults write "$DOMAIN" "$key" -int "$val"
        else
            defaults write "$DOMAIN" "$key" -string "$val"
        fi
    done
    echo "Config written: ${JASP_CONFIG_VARS[*]}"
fi

# ── a11y / webengine environment ─────────────────────────────────────
export QT_ACCESSIBILITY=1
# CDP debugging port for DOM-level checks (harmless if unused)
export QTWEBENGINE_CHROMIUM_FLAGS="${QTWEBENGINE_CHROMIUM_FLAGS:---remote-debugging-port=9223}"

cleanup() {
    if ! $KEEP_JASP; then
        kill -TERM "$JASP_PID" 2>/dev/null
        sleep 2
        kill -KILL "$JASP_PID" 2>/dev/null
    fi
}
trap cleanup EXIT

echo "Starting JASP: $JASP_BIN ${JASP_ARGS:+$JASP_ARGS}"
if [ -n "$JASP_ARGS" ]; then
    "$JASP_BIN" $JASP_ARGS >"$LOG_FILE" 2>&1 &
else
    "$JASP_BIN" >"$LOG_FILE" 2>&1 &
fi
JASP_PID=$!
echo "JASP running (PID $JASP_PID), log: $LOG_FILE"

sleep "$WAIT_SEC"
if ! kill -0 "$JASP_PID" 2>/dev/null; then
    echo "FATAL: JASP exited prematurely"
    tail -30 "$LOG_FILE"
    exit 1
fi

export JASP_PID
echo "Running test: $TEST_NAME"
"$PYTHON_BIN" "$TEST_SCRIPT"
rc=$?

if ! kill -0 "$JASP_PID" 2>/dev/null; then
    echo "WARNING: JASP exited during test run"
    tail -30 "$LOG_FILE"
fi

exit $rc
