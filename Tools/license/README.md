# Enterprise license expiry

The 14-month nag for enterprise JASP builds. Two halves:

- **This folder** — offline tooling to mint signed license blobs.
- `Desktop/utilities/license.{h,cpp}` — the reader compiled into PRO builds
  (`-DPRO -DLICENSE_CHECK`; set the CMake cache flag `LICENSE_CHECK=OFF` to
  compile the check out entirely).

## Threat model, honestly

The blob is **Ed25519-signed** (only JASP can mint one that verifies — the
private key lives in the vault, never in this repo) and **secretbox-encrypted
under a key compiled into every binary** (that key only hides the dates from
people opening `forced.json` or the registry; it is a curtain, not a lock).
Anyone able to patch the binary can remove the check — this feature defends
against editing files and registry keys, nothing more. It is a nag, not DRM.

## Workflow

```sh
# one-time (already done for the current keys; re-running invalidates all blobs):
python Tools/license/license.py keygen      # writes licensekeys.h + .license_keys.json
#   -> MOVE the seed from .license_keys.json to the JASP vault, delete the local file

# per client / per extension:
python Tools/license/license.py mint --serial 2026-10-acme-001              # 14 months
python Tools/license/license.py mint --serial 2026-10-acme-001 --months 24  # custom validity
python Tools/license/license.py mint --serial internal --never              # never expires
python Tools/license/license.py mint --serial partner --disabled            # check dormant

python Tools/license/license.py verify "<blob>" [--build-date 2026-10-07]   # what JASP will decide
python Tools/license/license.py selftest                                   # mint/verify/tamper checks
```

Deploy the minted string either as `forced.json` next to the app (per-client
MSI layer):

```json
{ "license": "<blob>" }
```

or, to extend an install without re-imaging, via group policy (the setting
rides the normal tier order, and policy outranks the shipped file):

```
reg add "HKLM\Software\Policies\JASP" /v license /t REG_SZ /d "<blob>"
```

## What JASP decides

| Blob (from GPO > forced.json > user INI)      | Release PRO build     | Debug build                |
|-----------------------------------------------|-----------------------|----------------------------|
| none found                                    | expired (fail closed) | silent (dev sanity)        |
| bad base64 / undecodable / signature fails    | expired (fail closed) | expired — popup            |
| `enabled: false`                              | dormant               | dormant                    |
| minted > 30 days before this build existed    | expired (stale token) | expired — popup            |
| `now >= expiresAt`                            | expired               | expired — popup            |
| otherwise                                     | silent                | silent                     |

Debug builds pop up whenever a blob is actually present, so the dialog is
testable by dropping `Tools/license/examples/forced-expired.json` next to
the executable as `forced.json`; only the no-blob case stays silent.

The stale-token rule is the leaked-blob defense: a blob that escapes (e.g. an
old `enabled:false`) cannot license any build released after it was minted,
while normally minted blobs and later GPO extensions keep working.

Popup (release builds, once per session, OK button, no dates in the text):

> **JASP Enterprise** — This enterprise version of JASP has expired. Please
> contact your provider for the latest version.

Batch/headless runs (`MainWindow` with `batchRun`, backendless tests) log the
state and never pop up.

## Enforcement (`LICENSE_ENFORCEMENT`, default OFF)

With `-DLICENSE_ENFORCEMENT` (CMake `LICENSE_ENFORCEMENT=ON`, effective only
next to `PRO` + `LICENSE_CHECK`) an expired license does more than nag:
`main()` demotes `AppInfo::proMode()` before argument parsing, so for that
session JASP behaves like a community build:

- **SSO / OIDC sign-in** — `AiBridge` falls back to API-key-only (the "only
  available in JASP PRO" message) and the sign-in switch disappears from AI
  preferences: the QML `PRO` property now follows `AppInfo::proMode()` at
  runtime instead of the compile-time flag.
- **Batch data-sync command line** — the PRO-only arguments stop parsing, so
  scheduled sync jobs open the GUI instead of running headless.

The popup text then mentions the disabled features, and a fresh blob (via
forced.json or group policy) restores everything on the next launch — no
reinstall, self-healing.

Why default OFF: with enforcement, every failure of the license pipeline
(missing file, stale token, our own bugs) escalates from "popup" to
"client features are down". Nag-only is the fail-safe default; enforcement
belongs in packaging recipes where someone decided to ship it.

Debug carve-out: as with the popup, debug builds only demote when a blob is
actually deployed (or `JASP_LICENSE_TEST_FAIL_CLOSED=1` is set), so blob-less
development keeps its PRO features.

## Technical note: finding forced.json before `Application` exists

`main()` parses arguments — and runs the enforcement demotion — before
`QCoreApplication` exists, and Qt's `applicationDirPath()` refuses to answer
that early. `Settings::shippedSettings()` therefore resolves the executable
directory from the OS when no instance exists yet (`GetModuleFileNameW` /
`/proc/self/exe` / `_NSGetExecutablePath` — the same calls Qt makes
internally). No `argv[0]` trust involved; if the OS will not say, the file
simply is not found, which fails closed.
