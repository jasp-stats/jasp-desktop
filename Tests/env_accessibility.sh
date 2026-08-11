#!/bin/bash
# Shared environment for all JASP accessibility tests.
# Source this file at the top of any accessibility test runner.
# To override any value, set the variable BEFORE sourcing this file.

# Detect repo root relative to this file's location.
# This file lives at jasp-desktop/Tests/env_accessibility.sh
# So repo root is two levels up.
if [ -z "${JASP_REPO_ROOT}" ]; then
    JASP_REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
fi

# Qt install prefix (built locally)
if [ -z "${JASP_QT_INSTALL}" ]; then
    JASP_QT_INSTALL="${JASP_REPO_ROOT}/qt-install"
fi

# JASP binary
if [ -z "${JASP_BIN}" ]; then
    JASP_BIN="${JASP_REPO_ROOT}/jasp-desktop/build/Desktop/JASP"
fi

# QtWebEngine resources and process
if [ -z "${QTWEBENGINE_RESOURCES_PATH}" ]; then
    QTWEBENGINE_RESOURCES_PATH="${JASP_QT_INSTALL}/usr/share/qt6/resources"
fi
if [ -z "${QTWEBENGINEPROCESS_PATH}" ]; then
    QTWEBENGINEPROCESS_PATH="${JASP_QT_INSTALL}/usr/lib/qt6/QtWebEngineProcess"
fi

# Display (can be overridden to use Xvfb or a real display)
if [ -z "${DISPLAY}" ]; then
    DISPLAY=:0.0
fi

# Accessibility always on
export QT_LINUX_ACCESSIBILITY_ALWAYS_ON=1
export QT_ACCESSIBILITY=1

# Chromium flags for headless/CI
if [ -z "${QTWEBENGINE_CHROMIUM_FLAGS}" ]; then
    QTWEBENGINE_CHROMIUM_FLAGS="--disable-gpu --disable-software-rasterizer"
fi

export DISPLAY
export QT_LINUX_ACCESSIBILITY_ALWAYS_ON
export QT_ACCESSIBILITY
export QTWEBENGINE_RESOURCES_PATH
export QTWEBENGINEPROCESS_PATH
export QTWEBENGINE_CHROMIUM_FLAGS