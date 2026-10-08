#!/usr/bin/env python3
"""Regenerates the example forced.json files in this folder:

    forced-expired.json   — past expiresAt   -> JASP logs/nags "expired"
    forced-valid.json     — 14 more months   -> JASP stays silent
    forced-dormant.json   — enabled:false    -> check compiled in but dormant

The expired one is the test case: copy it next to the JASP executable as
forced.json (or into its Resources/ folder) and start JASP. Debug builds log
the state; release PRO builds also pop up the dialog.

Run from the repo root:

    python Tools/license/examples/make_examples.py
"""

import datetime as dt
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import license as L  # noqa: E402  (Tools/license/license.py)

HERE = os.path.dirname(os.path.abspath(__file__))


def write(name: str, payload: dict) -> None:
    keys = L.load_keys()
    blob = L.mint_blob(keys, payload)
    with open(os.path.join(HERE, name), "w", encoding="utf-8") as f:
        json.dump({"license": blob}, f, indent=2)
        f.write("\n")
    # mirror a JASP built right now, so the stale-token rule applies like it
    # will on a fresh dev build
    state, _ = L.verify_blob(keys, blob, build_date=dt.datetime.now(dt.timezone.utc))
    print(f"{name:22s} -> {state:8s} ({len(blob)} base64 chars)")


def main() -> None:
    today = dt.datetime.now(dt.timezone.utc)

    write("forced-expired.json", {
        "schema": L.SCHEMA, "enabled": True, "serial": "EXAMPLE-expired",
        # issued recently (so the stale-token rule stays out of the way) but
        # already expired: exactly what a dead 14-month license looks like
        # to a build younger than ~30 days
        "issuedAt": L.iso_z(today - dt.timedelta(days=20)),
        "expiresAt": L.iso_z(today - dt.timedelta(days=1)),
    })
    write("forced-valid.json", {
        "schema": L.SCHEMA, "enabled": True, "serial": "EXAMPLE-valid",
        "issuedAt": L.iso_z(today),
        "expiresAt": L.iso_z(L.add_months(today, L.DEFAULT_MONTHS)),
    })
    write("forced-dormant.json", {
        "schema": L.SCHEMA, "enabled": False, "serial": "EXAMPLE-dormant",
        "issuedAt": L.iso_z(today),
        "expiresAt": None,
    })


if __name__ == "__main__":
    main()
