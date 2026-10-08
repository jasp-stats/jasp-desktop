#!/usr/bin/env python3
"""JASP Enterprise license tooling: keygen, mint, verify, selftest.

Blob layout (standard base64 of):
    "JLIC" | 0x01 | nonce[24] | secretbox(MAC[16] + payload + sig[64])

payload is compact JSON:
    {"schema":1,"enabled":true,"serial":"...","issuedAt":"...","expiresAt":"..."|null}

Two keys, two jobs (see Desktop/utilities/license.cpp for the reading side):

- The Ed25519 *signing* key is the real secret. Only its public half is
  compiled into JASP (licensekeys.h), so only we can mint a blob that JASP
  accepts. Keep the seed in the JASP vault — never in this repo.
- The secretbox key is compiled into JASP too. It merely hides the dates
  from people opening forced.json / the registry: a curtain, not a lock.

Deploy a minted blob either as forced.json next to the app:

    { "license": "<blob>" }

or, to extend an install without re-imaging, through group policy:

    reg add "HKLM\\Software\\Policies\\JASP" /v license /t REG_SZ /d "<blob>"
"""

import argparse
import base64
import datetime as dt
import json
import os
import secrets
import sys

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
HEADER_PATH = os.path.join(REPO_ROOT, "Desktop", "utilities", "licensekeys.h")
KEYS_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), ".license_keys.json")

MAGIC = b"JLIC"
VERSION = 1
SCHEMA = 1
DEFAULT_MONTHS = 14

try:
    # crypto_sign returns sig||message; the first 64 bytes are the detached
    # Ed25519 signature — exactly what libsodium's crypto_sign_verify_detached
    # (used by Desktop/utilities/license.cpp) checks.
    from nacl.bindings import (
        crypto_sign,
        crypto_sign_open,
        crypto_sign_seed_keypair,
        crypto_secretbox_easy,
        crypto_secretbox_open_easy,
        crypto_sign_PUBLICKEYBYTES,
        crypto_sign_BYTES,
        crypto_secretbox_KEYBYTES,
        crypto_secretbox_NONCEBYTES,
        crypto_secretbox_MACBYTES,
    )
    from nacl.exceptions import CryptoError
except ImportError:
    sys.exit("This tool needs PyNaCl:  python -m pip install pynacl")


def sign_detached(body: bytes, sk: bytes) -> bytes:
    return crypto_sign(body, sk)[:crypto_sign_BYTES]


def utc_now_iso() -> str:
    return dt.datetime.now(dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def add_months(moment: dt.datetime, months: int) -> dt.datetime:
    month = moment.month - 1 + months
    year = moment.year + month // 12
    month = month % 12 + 1
    day = min(moment.day, [31, 29 if year % 4 == 0 and (year % 100 != 0 or year % 400 == 0) else 28,
                           31, 30, 31, 30, 31, 31, 30, 31, 30, 31][month - 1])
    return moment.replace(year=year, month=month, day=day)


def iso_z(moment: dt.datetime) -> str:
    return moment.astimezone(dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def parse_date(value: str) -> dt.datetime:
    return dt.datetime.strptime(value, "%Y-%m-%d").replace(tzinfo=dt.timezone.utc)


# --------------------------------------------------------------------------- keys

def load_keys() -> dict:
    if not os.path.exists(KEYS_PATH):
        sys.exit(f"No keys found at {KEYS_PATH} — run `keygen` first (and keep the result out of git!).")
    with open(KEYS_PATH, "r", encoding="utf-8") as f:
        return json.load(f)


def cmd_keygen(_args) -> None:
    if os.path.exists(KEYS_PATH):
        sys.exit("Keys already exist. Re-running keygen invalidates EVERY blob minted so far;\n"
                 "delete .license_keys.json and licensekeys.h manually if you really mean it.")

    seed = secrets.token_bytes(32)
    k_obf = secrets.token_bytes(crypto_secretbox_KEYBYTES)
    pk, sk = crypto_sign_seed_keypair(seed)

    with open(KEYS_PATH, "w", encoding="utf-8") as f:
        json.dump({"seed": seed.hex(), "k_obf": k_obf.hex()}, f, indent=2)

    def c_array(name: str, key: bytes, length: int) -> str:
        lines = []
        for off in range(0, len(key), 12):
            lines.append("\t" + ", ".join(f"0x{b:02x}" for b in key[off:off + 12]) + ",")
        body = "\n".join(lines).rstrip(",")
        return f"constexpr unsigned char {name}[{length}] = {{\n{body}\n}};\n"

    header = f"""// GENERATED FILE — do not edit by hand. Regenerate with:
//   python Tools/license/license.py keygen
// Regenerating invalidates every blob minted so far.
#ifndef LICENSEKEYS_H
#define LICENSEKEYS_H

#include <sodium.h>

/// Ed25519 public key JASP verifies enterprise license blobs against. The
/// matching private key is NOT in this repo: it lives in the JASP vault and
/// only Tools/license/license.py mint ever sees it.
{c_array("jaspLicensePK", pk, crypto_sign_PUBLICKEYBYTES)}
/// Secretbox key lifting the obfuscation curtain: it hides the expiry dates
/// from people reading forced.json or the registry. It ships in every binary
/// by design and protects nothing cryptographically — authenticity comes
/// from the signature above. Be honest about what that is.
{c_array("jaspLicenseKObf", k_obf, crypto_secretbox_KEYBYTES)}
#endif // LICENSEKEYS_H
"""
    with open(HEADER_PATH, "w", encoding="utf-8") as f:
        f.write(header)

    print(f"Wrote {os.path.relpath(HEADER_PATH, REPO_ROOT)}  (pk={pk.hex()[:16]}...)")
    print(f"Wrote {os.path.relpath(KEYS_PATH, REPO_ROOT)}")
    print("MOVE THE SEED TO THE VAULT, then delete the local keys file:")
    print(f"  seed:  {seed.hex()}")
    print(f"  k_obf: {k_obf.hex()}")


# --------------------------------------------------------------------------- mint

def build_payload(args) -> dict:
    issued = parse_date(args.issued) if args.issued else dt.datetime.now(dt.timezone.utc)
    if args.disabled:
        expires = None
    elif args.never:
        expires = None
    elif args.expires:
        expires = parse_date(args.expires)
    else:
        expires = add_months(issued, args.months)
    return {
        "schema": SCHEMA,
        "enabled": not args.disabled,
        "serial": args.serial,
        "issuedAt": iso_z(issued),
        "expiresAt": iso_z(expires) if expires else None,
    }


def mint_blob(keys: dict, payload: dict) -> str:
    body = json.dumps(payload, separators=(",", ":"), ensure_ascii=False).encode("utf-8")
    _, sk = crypto_sign_seed_keypair(bytes.fromhex(keys["seed"]))
    sig = sign_detached(body, sk)
    nonce = secrets.token_bytes(crypto_secretbox_NONCEBYTES)
    ciphertext = crypto_secretbox_easy(body + sig, nonce, bytes.fromhex(keys["k_obf"]))
    return base64.b64encode(MAGIC + bytes([VERSION]) + nonce + ciphertext).decode("ascii")


def cmd_mint(args) -> None:
    keys = load_keys()
    payload = build_payload(args)
    blob = mint_blob(keys, payload)

    print(f"payload : {json.dumps(payload, separators=(',', ':'))}")
    print(f"blob    : {blob}\n")
    print("forced.json:")
    print(json.dumps({"license": blob}, indent=2))
    print('\ngroup policy (run as admin):')
    print(f'  reg add "HKLM\\Software\\Policies\\JASP" /v license /t REG_SZ /d "{blob}"')


# --------------------------------------------------------------------------- verify

def cmd_verify(args) -> None:
    blob = args.blob
    if os.path.isfile(blob):
        with open(blob, "r", encoding="utf-8") as f:
            blob = f.read().strip()
    state, payload = verify_blob(load_keys(), blob, build_date=args.build_date)
    print(f"state   : {state}")
    if payload is not None:
        print(f"payload : {json.dumps(payload, separators=(',', ':'))}")
    sys.exit(0 if state in ("VALID", "DORMANT") else 1)


def verify_blob(keys: dict, blob: str, build_date: dt.datetime | None = None) -> tuple[str, dict | None]:
    """Mirror of the C++ state machine in Desktop/utilities/license.cpp."""
    try:
        raw = base64.b64decode(blob.strip(), validate=True)
    except Exception:
        return "INVALID", None

    header_len = len(MAGIC) + 1 + crypto_secretbox_NONCEBYTES
    min_plain = 1 + crypto_sign_BYTES
    if len(raw) < header_len + crypto_secretbox_MACBYTES + min_plain:
        return "INVALID", None
    if raw[:4] != MAGIC or raw[4] != VERSION:
        return "INVALID", None

    nonce = raw[5:header_len]
    ciphertext = raw[header_len:]
    try:
        plain = crypto_secretbox_open_easy(ciphertext, nonce, bytes.fromhex(keys["k_obf"]))
    except CryptoError:
        return "INVALID", None

    body, sig = plain[:-crypto_sign_BYTES], plain[-crypto_sign_BYTES:]
    pk, _ = crypto_sign_seed_keypair(bytes.fromhex(keys["seed"]))
    try:
        if crypto_sign_open(sig + body, pk) != body:
            return "INVALID", None
    except CryptoError:
        return "INVALID", None

    try:
        payload = json.loads(body.decode("utf-8"))
    except Exception:
        return "INVALID", None
    if payload.get("schema") != SCHEMA:
        return "INVALID", None
    if not payload.get("enabled", True):
        return "DORMANT", payload

    issued = dt.datetime.fromisoformat(payload["issuedAt"].replace("Z", "+00:00"))
    if build_date is not None and issued < build_date - dt.timedelta(days=30):
        return "STALE_TOKEN", payload

    expires = payload.get("expiresAt")
    if not expires:
        return "VALID", payload
    expires = dt.datetime.fromisoformat(expires.replace("Z", "+00:00"))
    return ("EXPIRED" if dt.datetime.now(dt.timezone.utc) >= expires else "VALID"), payload


# --------------------------------------------------------------------------- selftest

def cmd_selftest(_args) -> None:
    keys = load_keys()
    today = dt.datetime.now(dt.timezone.utc)

    def payload(expires_at, issued_at=None, enabled=True):
        return {"schema": SCHEMA, "enabled": enabled, "serial": "selftest",
                "issuedAt": iso_z(issued_at or today), "expiresAt": expires_at}

    checks = [
        (f"valid {DEFAULT_MONTHS} months", payload(iso_z(add_months(today, DEFAULT_MONTHS))), None, "VALID"),
        ("never expires",                  payload(None), None, "VALID"),
        ("dormant",                        payload(None, enabled=False), None, "DORMANT"),
        ("expired yesterday",              payload(iso_z(today - dt.timedelta(days=1))), None, "EXPIRED"),
        ("stale token",                    payload(iso_z(add_months(today, 1)),
                                            issued_at=today - dt.timedelta(days=400)), today, "STALE_TOKEN"),
    ]

    ok = True
    for name, pl, build_date, expected in checks:
        state, _ = verify_blob(keys, mint_blob(keys, pl), build_date=build_date)
        good = state == expected
        ok &= good
        print(f"{'PASS' if good else 'FAIL'}  {name}: {state} (expected {expected})")

    # tamper detection: flip one bit inside the ciphertext
    blob = mint_blob(keys, payload(None))
    raw = bytearray(base64.b64decode(blob))
    raw[-1] ^= 1
    state, _ = verify_blob(keys, base64.b64encode(bytes(raw)).decode())
    good = state == "INVALID"
    ok &= good
    print(f"{'PASS' if good else 'FAIL'}  tampered ciphertext rejected: {state}")

    # wrong signing key must not verify
    _, other_sk = crypto_sign_seed_keypair(secrets.token_bytes(32))
    body = json.dumps(payload(None), separators=(",", ":")).encode()
    sig = sign_detached(body, other_sk)
    nonce = secrets.token_bytes(crypto_secretbox_NONCEBYTES)
    ct = crypto_secretbox_easy(body + sig, nonce, bytes.fromhex(keys["k_obf"]))
    state, _ = verify_blob(keys, base64.b64encode(MAGIC + bytes([VERSION]) + nonce + ct).decode())
    good = state == "INVALID"
    ok &= good
    print(f"{'PASS' if good else 'FAIL'}  wrong signing key rejected: {state}")

    print("selftest:", "OK" if ok else "FAILED")
    sys.exit(0 if ok else 1)


# --------------------------------------------------------------------------- main

def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)

    sub.add_parser("keygen", help="one-time: generate the keypair + curtain key, write licensekeys.h")

    mint = sub.add_parser("mint", help="mint a signed license blob")
    mint.add_argument("--serial", required=True, help="audit string, e.g. 2026-10-acme-001")
    mint.add_argument("--issued", help="issue date YYYY-MM-DD (default: today, UTC)")
    mint.add_argument("--months", type=int, default=DEFAULT_MONTHS,
                      help=f"validity in months from issue date (default {DEFAULT_MONTHS})")
    mint.add_argument("--expires", help="explicit expiry date YYYY-MM-DD (overrides --months)")
    mint.add_argument("--never", action="store_true", help="expiresAt null: never expires")
    mint.add_argument("--disabled", action="store_true", help="enabled false: check dormant on this deployment")

    verify = sub.add_parser("verify", help="decode a blob and report the state JASP would compute")
    verify.add_argument("blob", help="the base64 blob, or a path to a file containing it")
    verify.add_argument("--build-date", type=parse_date, help="pretend JASP was built on this date (stale-token rule)")

    sub.add_parser("selftest", help="mint/verify round-trip + tamper checks")

    args = parser.parse_args()
    {"keygen": cmd_keygen, "mint": cmd_mint, "verify": cmd_verify, "selftest": cmd_selftest}[args.command](args)


if __name__ == "__main__":
    main()
