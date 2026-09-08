#!/usr/bin/env bash
#
# JASP gate test runner.
#
# Ensures a Python venv with the MCP stack exists (mcp, httpx, jasp-mcp from
# the pinned submodule), builds the JASP binary if it is missing, then runs
# Tests/gatetest/gatetest.py with all arguments passed through.
#
# Usage:  Tests/gatetest/run_gatetest.sh [gatetest.py args...]
#   e.g.  Tests/gatetest/run_gatetest.sh --module jaspTTests --fail-fast
#
# Exit code is the gate test's exit code (0 = pass, 1 = gate failure).

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(dirname "$(dirname "$HERE")")"

JASP_BIN="${JASP_BIN:-$REPO/build/Desktop/JASP}"
VENV="$HERE/.venv"
PYTHON="${PYTHON:-python3}"

if [[ ! -x "$JASP_BIN" ]]; then
	echo "JASP binary not found at $JASP_BIN — building target 'JASP' (this may take a while)..."
	cmake --build "$REPO/build" --target JASP
fi

if [[ ! -x "$VENV/bin/python" ]]; then
	echo "Setting up gate test venv at $VENV ..."
	"$PYTHON" -m venv "$VENV"
	"$VENV/bin/pip" install --quiet --upgrade pip
	# mcp 2.x removed the lowlevel Server API jasp-mcp uses (@server.list_tools) — stay on 1.x
	"$VENV/bin/pip" install --quiet "mcp>=1.0.0,<2" "httpx>=0.27.0"
fi

# jasp-mcp comes from the pinned submodule so the MCP layer under test is deterministic.
if [[ -f "$HERE/jasp-mcp/src/jasp_mcp/server.py" ]]; then
	if ! "$VENV/bin/python" -c "import jasp_mcp" 2>/dev/null; then
		echo "Installing jasp-mcp from submodule ..."
		"$VENV/bin/pip" install --quiet "$HERE/jasp-mcp"
	fi
else
	echo "WARNING: Tests/gatetest/jasp-mcp submodule is missing."
	echo "         Run: git submodule update --init Tests/gatetest/jasp-mcp"
	echo "         Until then the gate runs with the MCP layer absent (recorded as a failure)."
fi

exec "$VENV/bin/python" "$HERE/gatetest.py" --jasp-bin "$JASP_BIN" "$@"
