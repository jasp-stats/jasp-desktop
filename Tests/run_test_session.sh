#!/bin/bash
set -e
#
# run_test_session.sh  –  unified harness for JASP accessibility tests.
#
# Usage:
#   run_test_session.sh --test <script.py>
#       [--jasp-args <args>] [--wait <seconds>]
#       [--jasp-config key=value ...] [--no-headless]
#
#   --headless          force Xvfb even when a real display is present
#                       (without this flag, Xvfb is auto-started only
#                        when no running X server is detected)
#   --no-headless       prevent Xvfb autostart; require a real display
#

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$SCRIPT_DIR/env_accessibility.sh"

# ── defaults ──────────────────────────────────────────────────────────
TEST_SCRIPT=""
JASP_ARGS=""
WAIT_SEC=10
JASP_CONFIG_VARS=()
FORCE_HEADLESS=false
NO_HEADLESS=false

# ── parse args ────────────────────────────────────────────────────────
while [[ $# -gt 0 ]]; do
    case "$1" in
        --test)
            TEST_SCRIPT="$2"; shift 2 ;;
        --jasp-args)
            JASP_ARGS="$2"; shift 2 ;;
        --wait)
            WAIT_SEC="$2"; shift 2 ;;
        --jasp-config)
            JASP_CONFIG_VARS+=("$2"); shift 2 ;;
        --headless)
            FORCE_HEADLESS=true; shift ;;
        --no-headless)
            NO_HEADLESS=true; shift ;;
        *)
            echo "Unknown flag: $1"
            echo "Usage: $0 --test <script.py> [--jasp-args ...] [--wait N] [--jasp-config k=v] [--headless|--no-headless]"
            exit 2 ;;
    esac
done

if [[ -z "$TEST_SCRIPT" ]]; then
    echo "FATAL: --test is required"
    exit 1
fi

if [[ ! -f "$TEST_SCRIPT" ]]; then
    echo "FATAL: test script not found: $TEST_SCRIPT"
    exit 1
fi

TEST_NAME="$(basename "$TEST_SCRIPT" .py)"

# ── X server detection / Xvfb autostart ──────────────────────────────
XVFB_PID=""
XVFB_DISPLAY=""

_pick_display() {
    for dn in $(seq 99 199); do
        if [[ ! -S "/tmp/.X11-unix/X$dn" ]]; then
            echo ":$dn"
            return 0
        fi
    done
    return 1
}

_has_running_xserver() {
    for sock in /tmp/.X11-unix/X*; do
        [[ -S "$sock" ]] && return 0
    done
    return 1
}

_start_xvfb() {
    XVFB_DISPLAY="$(_pick_display)"
    if [[ -z "$XVFB_DISPLAY" ]]; then
        echo "FATAL: could not find free display"
        exit 1
    fi
    echo "Starting Xvfb on $XVFB_DISPLAY ..."
    Xvfb "$XVFB_DISPLAY" -screen 0 1920x1080x24 +extension RANDR &
    XVFB_PID=$!
    sleep 1
    if ! kill -0 $XVFB_PID 2>/dev/null; then
        echo "FATAL: Xvfb failed to start"
        exit 1
    fi
    DISPLAY="$XVFB_DISPLAY"
    echo "Xvfb running (PID $XVFB_PID) on $DISPLAY"
}

if $NO_HEADLESS; then
    if [[ -z "${DISPLAY:-}" ]] || ! _has_running_xserver; then
        echo "FATAL: --no-headless specified but no X server found"
        exit 1
    fi
elif $FORCE_HEADLESS; then
    _start_xvfb
elif ! _has_running_xserver; then
    _start_xvfb
fi

# ── JASP config (optional) ───────────────────────────────────────────
if [[ ${#JASP_CONFIG_VARS[@]} -gt 0 ]]; then
    JASP_CONFIG_DIR="${HOME}/.config/JASP"
    JASP_CONFIG_FILE="${JASP_CONFIG_DIR}/JASP.conf"
    mkdir -p "$JASP_CONFIG_DIR"

    echo "[General]" > "$JASP_CONFIG_FILE"
    for kv in "${JASP_CONFIG_VARS[@]}"; do
        echo "${kv}" >> "$JASP_CONFIG_FILE"
    done
    echo "Config written: ${JASP_CONFIG_VARS[*]}"
fi

# ── log file ──────────────────────────────────────────────────────────
LOG_FILE="/tmp/jasp_${TEST_NAME}.log"

# ── session bus ────────────────────────────────────────────────────────
export DISPLAY
export QT_LINUX_ACCESSIBILITY_ALWAYS_ON
export QT_ACCESSIBILITY
export QTWEBENGINE_RESOURCES_PATH
export QTWEBENGINEPROCESS_PATH
export QTWEBENGINE_CHROMIUM_FLAGS

DBUS_PID=""
BUS_ADDR="$(dbus-daemon --session --print-address --fork 2>/dev/null)"
if [[ -n "$BUS_ADDR" ]]; then
    DBUS_PID="$(pgrep -fn "dbus-daemon.*--print-address" 2>/dev/null | head -1)"
    export DBUS_SESSION_BUS_ADDRESS="$BUS_ADDR"
fi

if [[ -z "${DBUS_SESSION_BUS_ADDRESS:-}" ]]; then
    echo "FATAL: could not start session bus"
    exit 2
fi

cleanup() {
    # Fire-and-forget kill — no wait, no hang
    kill -KILL $XVFB_PID $JASP_PID $ATSPI_BUS $ATSPI_REG $WATCHDOG_PID $DBUS_PID 2>/dev/null
    pkill -KILL -P "$$" 2>/dev/null
}
trap cleanup EXIT

sleep 1  # let session bus settle

/usr/lib/at-spi-bus-launcher --launch-immediately &
ATSPI_BUS=$!
for i in $(seq 1 5); do
    sleep 1
    if gdbus call --session --dest org.a11y.Bus --object-path /org/a11y/bus --method org.freedesktop.DBus.Peer.Ping &>/dev/null; then
        break
    fi
    if ! kill -0 "$ATSPI_BUS" 2>/dev/null; then
        echo "FATAL: at-spi-bus-launcher died (PID $ATSPI_BUS)"
        exit 2
    fi
done
if ! gdbus call --session --dest org.a11y.Bus --object-path /org/a11y/bus --method org.freedesktop.DBus.Peer.Ping &>/dev/null; then
    echo "FATAL: AT-SPI bus failed after retries"
    exit 2
fi

/usr/lib/at-spi2-registryd &
ATSPI_REG=$!
sleep 1

echo "Starting JASP ..."
if [ -n "$JASP_ARGS" ]; then
    "$JASP_BIN" "$JASP_ARGS" >"$LOG_FILE" 2>&1 &
else
    "$JASP_BIN" >"$LOG_FILE" 2>&1 &
fi
JASP_PID=$!
sleep "$WAIT_SEC"

if ! kill -0 $JASP_PID 2>/dev/null; then
  echo "FATAL: JASP exited prematurely (PID $JASP_PID)"
  tail -20 "$LOG_FILE"
  exit 1
fi

echo "JASP running (PID $JASP_PID)"
echo "Running test: $TEST_NAME"

export JASP_PID

# Background watchdog: if JASP dies, kill the Python test promptly
(
    while kill -0 $JASP_PID 2>/dev/null; do
        sleep 2
    done
    pkill -KILL -P "$$" python3 2>/dev/null
) &
WATCHDOG_PID=$!

/usr/bin/python3 "$TEST_SCRIPT"
rc=$?

kill -KILL $WATCHDOG_PID 2>/dev/null

if ! kill -0 $JASP_PID 2>/dev/null; then
  echo "WARNING: JASP exited during test run (PID $JASP_PID)"
  tail -20 "$LOG_FILE"
fi

exit $rc