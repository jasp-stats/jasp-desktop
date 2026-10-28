W# Windows Sandbox, EFS & the Save Crash — Analysis and Implementation Plan

**Status:** analysis + plan. **Phase 1 is implemented and validated** (2026-09-30: Descriptives-with-plots save/load round-trip OK). Phases 0/2/3 not yet started — Phase 0 and the Phase 3 spike need a Windows machine.
**Origin:** [jasp-issues#4566](https://github.com/jasp-stats/jasp-issues/issues/4566) "JASP crashed with WindowsApp EFS"
**Related:** #3562 (Security Sandbox Failure loop), #4523, #4548 (engine crashed on save)

### Status log

- **A/B experiment + full failure repro (2026-09-30, final):** against the locally encrypted package, the engine passed ALL money tests under BOTH identities - the custom `_JASP_JASPENGINE_V1` container (= today's JASP) AND the package SID. ⇒ this locally triggered Application-Protected flavor decrypts **per user** (LSASS), not per package identity - same conclusion as Experiment B. What IS reproducibly broken: **`CopyItem`/`CopyFile` from the encrypted source fails with "The specified file could not be encrypted" while stream copying succeeds** - the exact #4566 trigger, and exactly what Phase 1 replaced. **Revised conclusions:** (1) the observed JASP symptom (Save As crash) is fully explained and fully fixed by Phase 1 alone - on per-user-flavor machines the engine can read the encrypted trees even in the custom container; (2) the identity-bound flavor implied by some Codex/Claude reports (foreign processes denied, "not encrypted as expected" integrity enforcement) is a Store-signed/PICE phenomenon we cannot reproduce locally - if it exists for JASP engines, Phase 3 covers it, but **Phase 3 is now optional insurance + grant simplification, not a required fix**; (3) the local encryption trigger (elevated deploy + non-system default volume) yields the anti-drive-removal flavor, a faithful repro of the CopyFile failure mode.

- **MONEY TEST PASSED (2026-09-30 evening) — Phase 3 CONFIRMED:** reproduced `Application Protected` EFS locally on our own EFSPoC package (trigger: elevated `Add-AppxPackage` with `Set-AppxDefaultVolume D:\WindowsApps` in effect; Developer-signed, installed on C: — so the Store-only pattern from the reports is not a hard requirement, exact trigger variable still to bisect). Against the encrypted package: the full-trust launcher runs (transparent decrypt), and **the engine spawned in the package AppContainer executes FROM the encrypted exe, keeps WIN://SYSAPPID identity, and passes both LocalCache read/create money tests (exit 0)**. ⇒ An engine under the package-family AppContainer SID CAN transparently read Application Protected files. **Remaining for end-to-end proof: install JASP's own MSIX under the same trigger and run the real JASPEngine with the Phase 3 launch branch.** Full EFSPoC results (formula, #787, child-SID finding, profile-registration requirement, SYSAPPID survival) in the spike entry below.

- **EFSPoC spike results (2026-09-30, Win11 25H2 Pro, non-encrypted machine) — Phase 3 plumbing PROVEN:**
  1. **Derivation formula corrected & triple-verified:** SID = S-1-15-2 + first 28 bytes of SHA-256 over the *downcased UTF-16LE* moniker, read as **7 LITTLE-endian DWORDs** (big-endian reading is wrong; validated against the OS-recorded SID of `_JASP_JASPENGINE_V1` stamped on JASP's granted temp dir, against PowerShell reimplementation, and against the ACE Windows wrote on the PoC package's own AC profile dir).
  2. **WindowsAppSDK#787 reproduced:** `DeriveAppContainerSidFromAppContainerName(own family)` → hr=0, sid=null.
  3. **NEW FINDING — packaged-process AC APIs return CHILD AppContainer SIDs:** inside a packaged process, `DeriveAppContainerSidFromAppContainerName`/`CreateAppContainerProfile` derive 11+-subauthority children of the *caller's package*, not plain moniker hashes (explains #787: own-family degenerates to null). The manual plain-SID derivation is therefore REQUIRED, and correct.
  4. **Spawn recipe:** `CreateProcess` with `SECURITY_CAPABILITIES{AppContainerSid = plain package SID}` fails with ERROR_FILE_NOT_FOUND (2) until `CreateAppContainerProfile(familyName, ...)` registers a profile; then it works. Critical implementation detail for JASP.
  5. **Child token verified:** `TokenIsAppContainer: yes`, AppContainer SID = exactly the derived SID, and **`WIN://SYSAPPID` claims SURVIVE into the child** (full name + app ID + family name, plus WIN://PKG/WIN://PKGHOSTID) — the kernel-strips-claims risk is refuted. `GetCurrentPackageFamilyName` works in the child.
  6. **Money tests (read + create in LocalCache): both OK** — LocalCache is unencrypted on this machine, so the EFS-specific decryption leg remains untested; every other Phase 3 prerequisite is proven. Machine applies no Application-Protected encryption (Teams' and EFSPoC's LocalCaches both plain).
  PoC lives in `Tools/windows/efspoc/` (build.cmd builds+signs EFSPoC.msix; run and read poc-launcher.log/poc-engine.log).

- **Experiment B (2026-09-30, Win11 25H2 Pro, engine sandbox on):** classic user EFS applied via `cipher /e /s:JASP_Sandbox`. The EFS tripwire fired exactly as designed; **but the `-rw` container probe passed and the engine wrote its log (EFS-encrypted) in the encrypted tree all session**. ⇒ **F4 is refuted for classic user EFS on modern Windows**: EFS encrypt-at-create and transparent decrypt are performed per-user via LSASS and work fine from inside an AppContainer; container SIDs do not matter to EFS. Consequences: the #3562 failure loop is unlikely to be classic-EFS-caused on current Windows; the live EFS threat is the MSIX "Application Protected" flavor (Phase 3); **Phase 2 demoted to optional/defensive**. The experiment also validated Phase 0 end-to-end (tripwire + probes + no probe-file leftovers).

- **Phase 0.1–0.3 (done, 2026-09-30):** `container_permission_checker/main.cpp` rewritten as a real-I/O probe (`-r` read probe / `-rw` read+create+write+read-back+delete probe, pure Win32, exit code packs `stage<<24|pathIndex<<16|GetLastError()`; missing dirs skipped as before, empty `-rw` dirs pass via the write probe). `checkIfAccessible` gained `ProbeMode` + failure decoding into `Log::log()`, the **timeout false-pass fix** (5 s, timeout/crash = failure) and the QProcess-leak fix (stack object). EFS self-report added per engine launch: `GetFileAttributesW` on every granted path + programDir, loud log line when `FILE_ATTRIBUTE_ENCRYPTED`. **Deviations from plan:** checker diagnostics travel via packed exit code instead of stderr (the STARTUPINFOEX override drops QProcess's std pipes, and flipping `inheritHandles` would widen the sandboxed spawn); EFS logging reports only encrypted/unreadable paths (not every entry) to avoid spam across engine restarts. **0.4 open:** request WER faulting module + `cipher /c` output from the #4566 reporter.

- **Phase 1 (done):** `Utils::copyFileStreamed` added (`Common/utils.{h,cpp}`); `createSnapshot` rewritten non-throwing with stream copy + DB-fatal/others-skip policy; snapshot-leak fixed in `saveDataSet` via `ScopedSnapshotCleanup`; `mainwindow.cpp` FileSave/autosave failure → `setComplete(false, msg)`; **Save Image sites** (`_analysisSaveImageHandler`, `analysisImageSavedHandler`) stream-copied too; `Tests/testall.cpp` caller updated.
- **Research (2026-09-30):** error code confirmed as `ERROR_ENCRYPTION_FAILED` (0x80071770); `xcopy /G` (= `COPY_FILE_ALLOW_DECRYPTED_DESTINATION`) is the ecosystem-standard workaround — stream copy is its programmatic equivalent; WindowsAppSDK#787 (null SID for own family name) confirmed real; decrypting files under `WindowsApps`/`Packages` bricks them ("not encrypted as expected" catch-22) — never `DecryptFile` inside those trees; **new Phase 3 spike items:** verify `WIN://SYSAPPID` token claim survives into the AC child (kernel strips package claims from children without the same identity), and verify encrypt-at-create in LocalCache.
- **Dev env note:** debug builds run straight from the Qt kit need `QTWEBENGINE_DISABLE_SANDBOX=1` (Chromium sandbox kills `QtWebEngineProcessd.exe` with 0xC0000135); now set automatically under `JASP_DEBUG` in `Desktop/main.cpp`.

---

## 1. TL;DR

1. **A real, confirmed crash bug:** `JASPExporter::createSnapshot()` throws `LoaderException` from inside `MainWindow::fileEventRequestHandler` (`mainwindow.cpp` ~1794), where **no try/catch exists**. Any snapshot copy failure — EFS, a locked file, antivirus, disk full — becomes `std::terminate` → "JASP crashed". This affects **all installers**, not just MSIX.
2. **The EFS connection (hypothesis, strong):** Windows 11 24H2/25H2 EFS-encrypts MSIX package files at "Application Protected" compatibility level. `CopyFile` (used by `std::filesystem::copy`) **fails** against such files while plain read/write **succeeds** — the exact asymmetry that makes everything work until "Save As".
3. **The strategic fix:** on MSIX, launch the engine with **JASP's own package-family AppContainer SID** instead of the custom `_JASP_JASPENGINE_V1` profile. The engine then holds the identity the EFS keys are bound to. MSI/ZIP builds are structurally immune to the Application-Protected family (no package tree → nothing encrypted) and keep the custom container.

---

## 2. The failure landscape

| # | Failure mode | MSIX | MSI/ZIP | Root cause |
|---|---|---|---|---|
| F1 | Crash on Save As (`createSnapshot` throws uncaught) | ✅ | ✅ | `mainwindow.cpp` call site has no try/catch |
| F2 | `CopyFile` breaks against EFS Application-Protected sources | ✅ | ❌ can't occur | Windows encrypts MSIX package tree; `CopyFileExW` can't re-wrap keys; MSVC STL doesn't pass `COPY_FILE_ALLOW_DECRYPTED_DESTINATION` |
| F3 | Engine can't create/read files in EFS-encrypted dirs | ✅ | ✅ (classic EFS only) | AppContainer tokens cannot perform EFS operations; DACL grants don't help |
| F4 | Classic user EFS on `Documents\JASP_Sandbox` | ✅ | ✅ | `JASP_Sandbox` inherits encryption from Documents; engine (AC token) can't write logs/read data there |
| F5 | "Security Sandbox Failure" loop (#3562) | ✅ | ✅ | `checkIfAccessible` detects failure → ACL grants can't fix EFS → loop |
| F6 | Sandbox startup is slow ("Intializing JASP security sandbox") | ✅ | ✅ | `grantAccessToExeDir` walks the whole install tree; checker spawns per engine launch |

> **Confidence note:** F1 is confirmed by code reading. F2/F3 are strongly supported by external evidence (see §4) but have not been reproduced on a JASP machine yet — Phase 0 exists precisely to convert them from hypothesis to telemetry.

---

## 3. Mental model (the short version)

**SID** = a name Windows hashes out of a moniker. `S-1-5-…` = user, `S-1-15-2-…` = AppContainer identity.
**Token** = the badge a process wears: list of SIDs + integrity level (clearance).
**DACL** = the guest list on each door (file/dir/pipe). Match any SID on the badge → allowed.
**AppContainer token** = a special badge where your *personal* names stop counting; only the container SID (+ capabilities) can get you in. Since normal doors never list container SIDs → **denied by default**. That's the sandbox; it is not something we build, it's the absence of our name on the lists.
**Integrity level** = coarse clearance; low-IL can't write medium-IL objects regardless of DACL.
**EFS** = a third gate that isn't a list at all — it's keys. The FEK (file encryption key) is wrapped with the certificates of allowed identities. ACL access ≠ decryption.

Three gates per file access: **DACL (name match) → integrity (clearance) → EFS (key)**. Our existing tooling only speaks the first language.

### EFS specifics that matter

- Classic EFS: user opts in (Pro/Enterprise only; Home can't). Directories marked "encrypted" auto-encrypt **new files created inside, using the creating process's token**. Decryption requires the user's private key (DPAPI-protected in the profile).
- **Application Protected EFS** (new, 24H2/25H2 era): Windows itself EFS-encrypts **MSIX package files** — the install dir under `C:\Program Files\WindowsApps\` and parts of the package's `%LOCALAPPDATA%\Packages\<family>\LocalCache\` — with keys tied to the **package identity**. Happens on personal, unmanaged machines. Users never asked for it.
- **The asymmetry that explains "works until Save As":** open/read/write works transparently for the rightful identity; **`CopyFileW` fails** ("The specified file could not be encrypted") because it tries to *preserve* the encrypted state at the destination and can't re-wrap the keys. Documented via `COPY_FILE_ALLOW_DECRYPTED_DESTINATION` semantics; observed empirically in the wild (§4).

### Where JASP's paths land (MSIX build)

| Path | Resolves to | Encryption exposure |
|---|---|---|
| `Dirs::tempDir()` / session dir | under `Packages\<family>\LocalCache\…` | Application Protected (F2/F3) |
| `AppDirs::appData(false)` | same tree | same |
| `internal.sqlite` (DB) | in the session dir | same |
| `AppDirs::sandboxedDocuments()` | `Documents\JASP_Sandbox` | classic user EFS only (F4) |
| `AppDirs::logDir()` | `JASP_Sandbox\Logs` | classic user EFS only |
| install dir | `C:\Program Files\WindowsApps\<pkg>` | Application Protected (reads OK in-app) |

The Save As flow: `MainWindow::fileEventRequestHandler` → `JASPExporter::createSnapshot` (recursive `std::filesystem::copy` of the **whole session dir** → `%TEMP%`) → `AsyncLoader::saveTask` → zip via libarchive to `<target>.jasp.tmp` → `Utils::renameOverwrite`. The snapshot copy is the **only** bulk `CopyFile` in the product — and it runs over exactly the tree Windows encrypts.

---

## 4. External evidence

- [openai/codex#46582](https://github.com/openai/codex/issues/46582) — MSIX package dir EFS-encrypted (`cipher /c` → "Application Protected"); `fs.copyFileSync` (→ `CopyFileW`) fails with unknown error; `readFileSync`+`writeFileSync` **succeeds**. Same volume, same identity → not ACLs, not cross-volume: it is CopyFile-against-EFS-source.
- [anthropics/claude-code#83703](https://github.com/anthropics/claude-code/issues/83703) — package LocalCache tree EFS-encrypted at "Application Protected" on a personal, non-domain, non-MDM machine; "enforced by the Windows APPX/Package Identity subsystem… Windows' own packaging behavior".
- [openai/codex#34764](https://github.com/openai/codex/issues/34764) — copying bundled runtime out of WindowsApps fails: "The specified file could not be encrypted".
- [Raymond Chen: What are these SIDs of the form S-1-15-2-xxx?](https://devblogs.microsoft.com/oldnewthing/20220502-00/?p=106550) — AppContainer SIDs are `S-1-15-2-<7 subauths>`; to map a SID to an app, feed each installed family name into `DeriveAppContainerSidFromAppContainerName` — **the moniker for a package IS its family name**.
- [MS: Launch an AppContainer](https://learn.microsoft.com/en-us/windows/win32/secauthz/implementing-an-appcontainer) — calls the S-1-15-2 value the "Package SID"; `DeriveAppContainerSidFromAppContainerName` derives it from the moniker.
- [SO: API to get AppContainerName from AppContainerSid](https://stackoverflow.com/questions/47521346/api-to-get-appcontainername-from-appcontainersid) — derivation = `lowercase(moniker)` → **SHA-256** → first 7 dwords → `S-1-15-2-h0-…-h6`.
- [WindowsAppSDK#787](https://github.com/microsoft/ProjectReunion/issues/787) — ⚠️ **landmine:** calling `DeriveAppContainerSidFromAppContainerName` with the **caller's own package family name** from inside a packaged process returns success but a **null SID**. This is exactly our call — hence the manual-hash fallback.

---

## 5. Current implementation map (files & functions)

| File | What it does |
|---|---|
| `Desktop/utilities/wincontainermanager.cpp` | `launchSandboxedEngine`: creates custom AC `_JASP_JASPENGINE_V1`, grants ACLs (`AllowNamedObjectAccess` on tempDir/appData/appData(roaming)/`JASP_Sandbox`/userModulesDir; `grantAccessToExeDir` → `ALL APPLICATION PACKAGES` + `ALL RESTRICTED APP PACKAGES` RX on install dir), verifies via `checkIfAccessible`, launches engine with `PROC_THREAD_ATTRIBUTE_SECURITY_CAPABILITIES` |
| `Desktop/container_permission_checker/main.cpp` | the "verification": **only `std::filesystem::status()`** — a metadata existence check. Never opens/creates anything → blind to EFS (failures happen at open/create time) and to real ACL problems |
| `Desktop/data/exporters/jaspexporter.cpp` | `createSnapshot`: `std::filesystem::copy` recursive of session dir → `%TEMP%` snapshot; **throws `LoaderException` on any failure**; `saveDataSet` zips snapshot + DB into `<target>.jasp.tmp`; pops the snapshot queue *before* zipping (leaks snapshot dir on later failure) |
| `Desktop/mainwindow.cpp` ~1794 | `FileSave` branch calls `createSnapshot` with **no try/catch** → any throw = crash |
| `Desktop/engine/enginesync.cpp` ~1027 | `startSlaveProcess` → `WinContainerManager::launchSandboxedEngine(slave, engineExe, args)` (falls back to plain start) |
| `Engine/main.cpp` ~85 | engine opens its log at `AppDirs::logDir()` = `JASP_Sandbox\Logs` (only when log-to-file enabled) |
| `QMLComponents/utilities/appdirs.cpp` | `sandboxedDocuments()` = `Documents\JASP_Sandbox`; `logDir`, `clipboardDir` inside it |
| `QMLComponents/utilities/messageforwarder.cpp` | `constrainToSandboxStartDir/Result`: with sandbox enabled, file dialogs start in & remap results into `JASP_Sandbox` |
| `Tools/windows/msix/AppxManifest-*.xml.in` | `packagedClassicApp`, `mediumIL`, `runFullTrust` → Desktop has package identity but runs full-trust (not an AppContainer) |

Identity split today:

| Process | Token |
|---|---|
| JASPDesktop (MSIX) | normal user token, medium IL, **package identity** |
| JASPEngine | restricted token, low IL, custom AC SID `_JASP_JASPENGINE_V1`, **no package identity** |

---

## 6. The plan

### Phase 0 — Make the machines tell the truth (diagnostics, no behavior change)

**0.1 Mode-aware real-I/O permission checker** — rewrite `Desktop/container_permission_checker/main.cpp`:

```
ContainerFilePermissionChecker <-r|-rw> <path> [<path>...]
```

- `-r` (read probe — used for the install dir, must never write there):
  1. enumerate the directory;
  2. open the first regular file, read a few bytes (in the install dir that's a DLL — reading the `MZ` header is the perfect probe), close.
- `-rw` (scratch dirs): the same existing-file read probe **plus** create → write → read-back → delete of a uniquely named probe file (`.jasp_probe_<pid>_<ts>`), cleaned up on all paths.
- Exit code = first failing `GetLastError()` (0 = ok, -2 = usage, -1 = no testable file found). **Different probes catch different EFS failure modes:** create-probe = "directory encrypted, AC can't encrypt" (F3); existing-file-read = "files written by the other identity can't be decrypted" (the #4566 family).
- The checker is a standalone exe (no Qt) — keep it dependency-free.

**0.2 `checkIfAccessible` upgrade** (`wincontainermanager.cpp`): add a mode parameter, forward it as the checker's first argument, capture the checker's **stderr** (one line per failure: path + stage + error) into `Log::log()`. Map call sites:

```cpp
checkIfAccessible(si, _fullAccessList, ProbeMode::ReadWrite);
checkIfAccessible(si, {AppDirs::programDir().absolutePath()}, ProbeMode::ReadOnly);
```

**0.3 EFS attribute logging**: at each `launchSandboxedEngine`, `GetFileAttributesW` every `_fullAccessList` entry; log `FILE_ATTRIBUTE_ENCRYPTED` state. Every engine launch self-reports whether it's walking into an encrypted tree.

**0.4 Issue #4566**: request WER faulting module + `cipher /c` output on the LocalCache and save target from the reporter.

### Phase 1 — Kill the crash family (all installers)

**1.1 `createSnapshot` rewrite** — stream-copy primary, non-throwing:

```cpp
// jaspexporter.h
static bool createSnapshot(const std::string &snapshotPrefix = "jasp_snapshot_",
                           std::string *errorOut = nullptr);
```

```cpp
namespace {
bool copyFileStreamed(const std::filesystem::path &src, const std::filesystem::path &dst, std::string &error)
{
    std::ifstream  in (src,  std::ios::binary);
    std::ofstream  out(dst, std::ios::binary | std::ios::trunc);
    if (!in)  { error = "cannot open source";        return false; }
    if (!out) { error = "cannot create destination"; return false; }

    std::vector<char> buf(1 << 16);
    while (in.read(buf.data(), buf.size()) || in.gcount() > 0)
    {
        out.write(buf.data(), in.gcount());
        if (!out) { error = "write failed (disk full?)"; return false; }
    }
    return true;
}
}
```

Failure policy:

| What fails | Policy |
|---|---|
| snapshot dir creation / enumeration | **fatal** → return false + error string |
| `internal.sqlite` copy | **fatal** (a .jasp without data isn't a save) |
| plot/state/other file copy | **log + skip** (matches existing release-build tolerance) |

Implementation notes:
- iterate with `std::filesystem::recursive_directory_iterator` + `error_code`; recompute relative paths; skip non-regular files;
- DB filename from `DatabaseInterface::singleton()->dbFile(true)` (`"internal.sqlite"`), compared against the relative path;
- on fatal failure, `remove_all` the partial snapshot dir (leave nothing in `%TEMP%`);
- keep `printSnapshotContents` debug logging; keep `_snapshotQueue` mechanics identical.

Why stream-copy is safe here: the snapshot is a throwaway staging dir; the zip entries get timestamps forced to `_now` anyway, so losing CopyFile's metadata preservation costs nothing. Streams read plaintext (transparent decrypt for the rightful identity) and create fresh plain files.

**1.2 Call-site fix** (`mainwindow.cpp`, `FileSave` branch):

```cpp
std::string snapshotError;
if (!JASPExporter::createSnapshot(event->isTmp() ? "jasp_autosave_snapshot_" : "jasp_snapshot_", &snapshotError))
{
    event->setComplete(false, tr("Could not prepare the data for saving: %1").arg(tq(snapshotError)));
    return;
}
_loader->io(event);
```

(`FileEvent::setComplete(bool success, const QString &message, bool cancelled)` confirmed.)

**1.3 Snapshot-dir leak fix**: in `saveDataSet`, after the queue pop, any exception path (e.g. `archive_write_open_filename` failure) must `cleanupSnapshot(sourceDir)` — currently the popped dir leaks in `%TEMP%` forever.

### Phase 2 — Classic EFS on `Documents\JASP_Sandbox` (all installers)

Startup sequence, **every launch** (re-encryption can happen between runs):

1. `GetFileAttributesW` on `sandboxedDocuments()` (+ Logs/Clipboard): if `FILE_ATTRIBUTE_ENCRYPTED` →
2. `DecryptFile()` our own subtree: remove the dir flag + walk existing files (only `Logs/`, `Clipboard/` silently — **ask consent for user-placed files**), before `initLog()` opens anything; skip locked files (retry next launch);
3. still encrypted (foreign cert / policy / missing smartcard) → existing "Security Sandbox Failure" → sandbox-off fallback;
4. add `sandboxedDocuments()` to the final fatal check in `launchSandboxedEngine` (today only programDir + appData are fatal-checked; JASP_Sandbox failures pass silently).

Honest limitation: `DecryptFile` works only when the current user's EFS cert is among the file's key recipients. Files encrypted by another user/cert or by policy are stuck → that's what step 3 is for.

### Phase 3 — MSIX: engine joins the package identity

**3.1 Derivation** (Desktop, MSIX builds only):

```cpp
PSID derivePackageAppContainerSid()
{
    WCHAR familyName[PACKAGE_FAMILY_NAME_MAX_LENGTH + 1] = {};
    UINT32 len = ARRAYSIZE(familyName);
    if (::GetCurrentPackageFamilyName(&len, familyName) != ERROR_SUCCESS)
        return nullptr;   // not packaged → caller falls back to custom profile

    PSID sid = nullptr;
    // WinAppSDK#787: succeeds-but-null when asking for OUR OWN family name
    if (FAILED(::DeriveAppContainerSidFromAppContainerName(familyName, &sid)) || !sid)
        sid = manuallyHashAppContainerSid(familyName);  // CNG SHA-256 fallback
    return sid;
}
```

- Manual fallback: `lowercase(familyName)` → SHA-256 (BCrypt/CNG; needs `bcrypt.lib`) → first 7 dwords → `S-1-15-2-h0-…-h6`. Fully deterministic, no system state.
- **Verification step** (log-only, no gating): `GetNamedSecurityInfo` on `%LOCALAPPDATA%\Packages\<family>\`; the derived SID should appear in the DACL (skip well-known `S-1-15-2-1`/`S-1-15-2-2`). Match ⇒ derivation provably correct on that machine.
- `DerivePackageSidFromPackageFamilyName` does **not** exist (researched; don't chase it).

**3.2 Launch branch** (`launchSandboxedEngine`):

```cpp
PSID appContainerSid = nullptr;
if (DynamicRuntimeInfo::getInstance()->getRuntimeEnvironment() == RuntimeEnvironment::MSIX)
{
    appContainerSid = derivePackageAppContainerSid();
    Log::log() << (appContainerSid ? "Engine using JASP package AppContainer SID"
                                   : "Package SID derivation failed — custom profile fallback") << std::endl;
}
if (!appContainerSid)
{
    // existing CreateAppContainerProfile("_JASP_JASPENGINE_V1") path, unchanged
}
```

Everything downstream is SID-agnostic (grants write whatever SID they're handed; checker spawns with the built attribute list). Capabilities stay empty → no network, low IL unchanged.

**3.3 Grant simplification (verify, then delete)** — first iteration changes nothing; once the SID is confirmed, A/B on MSIX:
- `%TEMP%`/appData grants likely redundant (LocalCache pre-granted to the package SID — the verification step literally reads that ACE);
- `grantAccessToExeDir` likely redundant (Store packages ship AAP ACLs);
- `sandboxedDocuments()` grant **stays** (Documents grants nothing to containers).
Each removal validated by the Phase-0 checker. End state for MSIX: *derive SID → spawn engine*, and the slow "preparing sandbox" box disappears from MSIX.

**3.4 Spike checklist** (1–2 days, needs a 25H2 machine where `cipher /c` on the LocalCache shows Encrypted/Application Protected — verify first, else the money test can't run):

- [ ] `CreateProcess` succeeds with the package-family SID
- [ ] Engine token (`whoami /groups`): exact SID, low IL, zero capabilities
- [ ] Derived SID present in `Packages\<family>` DACL
- [ ] Read a file under `WindowsApps\<pkg>` → succeed
- [ ] **Read an Application-Protected LocalCache file → succeed** ← the money test
- [ ] `CreateFile` in session dir → succeed
- [ ] Outbound socket → **fail** (sandbox intact)
- [ ] Read `Documents\<user file>` → **fail** (sandbox intact)
- [ ] Engine named objects now under `\Sessions\…\AppContainerNamedObjects\<package-SID>\` — Desktop↔engine IPC (pipes/sockets) unaffected
- [ ] Full analysis + save round-trip under the package SID

If the money test fails → plan B: relocate engine working set out of the encrypted tree / broker I/O through the Desktop (Phase 4 territory).

### Phase 4 — Optional hardening

- Narrow the exe-dir grant on MSI/ZIP: engine's own AC SID instead of `ALL APPLICATION PACKAGES` (today every container on the machine can read JASP's install dir).
- Broker engine logs over IPC → engine never needs write access to `JASP_Sandbox\Logs`.
- Cache `checkIfAccessible` results per machine/session instead of per engine launch.

---

## 7. What NOT to do

- Don't try to fix EFS with ACLs. There is no SID you can grant that unwraps a key. The only levers are: change identity (Phase 3), change location (relocation), or change who performs the I/O (broker).
- Don't decrypt anything outside `JASP_Sandbox` subtree, and ask before touching user-placed files inside it.
- Don't remove the custom-profile path — it's the fallback for MSI/ZIP/dev builds and for derivation failures on MSIX.

## 8. Order of operations

Phase 0 (instrument) → Phase 1 (survive) can land immediately and shrink the crash reports. Phase 2 next (small, installer-agnostic). Phase 3 is gated on the spike; Phase 0's checker is its instrument. Phase 4 as time allows.
