#!/usr/bin/env python3
"""Hammer a menu-opening button via AXPress; report when the AX bridge dies."""
import subprocess
import sys
import time

import ApplicationServices as AS
import Quartz
from ApplicationServices import (
    AXUIElementCreateApplication,
    AXUIElementCopyAttributeValue,
    AXUIElementPerformAction,
    kAXMainWindowAttribute,
    kAXChildrenAttribute,
    kAXRoleAttribute,
    kAXTitleAttribute,
    kAXPressAction,
    kAXFocusedUIElementAttribute,
)
from CoreFoundation import kCFBooleanTrue

ERR = {-25204: "NotImpl", -25207: "CannotComplete", -25211: "NoValue", -25212: "InvalidElem"}


def attr(e, n):
    ok, v = AXUIElementCopyAttributeValue(e, n, None)
    return (ok, v)


def alive(app):
    ok, mw = attr(app, kAXMainWindowAttribute)
    if ok:
        return False
    ok, kids = attr(mw, kAXChildrenAttribute)
    return ok == 0


def find_button(app, name):
    ok, mw = attr(app, kAXMainWindowAttribute)
    if ok:
        return None
    stack = [mw]
    while stack:
        e = stack.pop(0)
        ok, kids = attr(e, kAXChildrenAttribute)
        if ok:
            continue
        for k in kids:
            okr, r = attr(k, kAXRoleAttribute)
            okt, t = attr(k, kAXTitleAttribute)
            if okr == 0 and r == "AXButton" and okt == 0 and t == name:
                return k
            stack.append(k)
    return None


def escape():
    for down in (True, False):
        ev = Quartz.CGEventCreateKeyboardEvent(None, 53, down)
        Quartz.CGEventPost(Quartz.kCGHIDEventTap, ev)
        time.sleep(0.03)


def main():
    binary = sys.argv[1]
    rounds = int(sys.argv[2]) if len(sys.argv) > 2 else 15
    proc = subprocess.Popen([binary], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    time.sleep(6)
    pid = proc.pid
    app = AXUIElementCreateApplication(pid)
    AS.AXUIElementSetAttributeValue(app, "AXManualAccessibility", kCFBooleanTrue)
    # engage
    for _ in range(20):
        attr(app, kAXFocusedUIElementAttribute)
        ok, mw = attr(app, kAXMainWindowAttribute)
        if ok == 0:
            ok, kids = attr(mw, kAXChildrenAttribute)
            if ok == 0 and len(kids) > 0:
                break
        time.sleep(0.5)
    print(f"engaged, alive={alive(app)}")

    for i in range(rounds):
        b = find_button(app, "Open menu")
        if b is None:
            print(f"round {i}: button not found (dead={not alive(app)})")
            break
        r = AXUIElementPerformAction(b, kAXPressAction)
        time.sleep(0.8)
        a = alive(app)
        print(f"round {i}: press={r} {ERR.get(r, 'ok' if r == 0 else '')} alive={a}")
        if not a:
            print(f"AX DIED at round {i}")
            break
        escape()
        time.sleep(0.5)
        if not alive(app):
            print(f"AX DIED after escape at round {i}")
            break
    else:
        print(f"survived {rounds} rounds")
    proc.terminate()


if __name__ == "__main__":
    main()
