# Binary packages as direct R library paths (`binary_pkgs/<hash>/<pkg>`)

Status: implemented (additive rescue + farm retirement) · October 2026
Related: jasp-issues #4586, #4566 · `Docs/development/windows-sandbox-efs-save-crash.md`

## Problem

On Windows (MSIX/MSI) the modules' R packages are deduplicated in `binary_pkgs/<hash>/`,
where the hash dir *is* the package root (the hash replaces the package name). R can only
discover packages through name-keyed library dirs (`.libPaths()` entries are searched for
`<lib>/<pkgname>/DESCRIPTION`), so `jaspModuleBundleManager::installJaspModuleBundle()`
creates `module_libs/<module>/<pkg>` **junctions** into `binary_pkgs/<hash>` — on the build
machine when assembling the shipped tree, and on the user machine when installing modules.
Because junctions can't ship inside an MSIX, `junction_tool` re-creates the whole farm in
appData (`BundledJASPModules_<version>_<commit>_<date>/`) on first run.

The farm is fragile (jasp-issues #4586):

- Junction creation can fail per entry (AV interference, `MAX_PATH` without
  `LongPathsEnabled`) while the tool still exits 0, so a half-built farm is marked
  initialized and persists forever: modules fail with `unable to load R code in package
  '<mod>'` and `DLL not found: maybe not installed for this architecture?`
- The farm lives in appData where newer Windows 11 builds (24H2/25H2) may EFS-encrypt
  files created by packaged processes, which the sandboxed engine — a different
  AppContainer identity — then cannot read.

The engine *can* read the install tree directly (jaspBase loads from `R/library` in
`WindowsApps` today), so loading deps straight from `binary_pkgs` removes both failure
modes at once.

## Key realizations

1. **The `.JASPModule` bundles never change.** Every module — bundled and user-installed —
   passes through `jaspModuleBundleManager` (submodule `Engine/jaspModuleBundleManager`,
   this repo): `Modules/install-modules.R.in` calls it at build time into the shipped tree,
   and `DynamicModules::getJsonForBundleInstallRequest()` calls it at install time into the
   user modules dir. One routine to change covers everything.
2. **The manifest already contains the needed mapping.** Every bundle manifest
   (`<module>_manifest.json`, copied to `manifests/`) carries `mapping` entries
   `"<hash> => <pkgname>_<version>"` for every package *including the module itself*.
   No new manifest field is needed — JASP derives the hash dirs from `mapping`.

## Changes

**jaspModuleBundleManager (submodule `Engine/jaspModuleBundleManager`, this repo):**

after extraction and repair, **nest** every hash dir into a micro-library (a move, not a re-extract —
so old installs upgrade in place):**

```
old:  binary_pkgs/<hash>/DESCRIPTION            (hash dir == package root, junction needed)
new:  binary_pkgs/<hash>/<pkgname>/DESCRIPTION  (hash dir == micro-library, .libPaths()-able)
```

- `nestBinaryPkgIfNeeded()` (R/utils.R) moves a flat `<hash>` dir to `<hash>/<pkgname>` — idempotent,
  skips already-nested and missing dirs; `installJaspModuleBundle()` applies it over the manifest's
  `from`/`to` mapping, and points the `module_libs` junctions one level deeper accordingly;
- **Windows-only** (`.Platform$OS.type == 'windows'`): Linux/macOS keep the flat layout and their
  proper symlinks, which ship fine in the packages;
- the hash (content hash) is unchanged, so dedup, already-present detection and uninstall keep working.

**junction_tool (`Desktop/junction_tool/main.cpp`) — retired in the farm-retirement step:** the
farm it built is gone; the target, the build-time `-s` scan (`collect-junctions` in
`Tools/CMake/Pack.cmake` / `Tools/windows/BuildBotScript.cmd`), the `junctions_map.txt` shipment
and the first-run creation in `Desktop/main.cpp` were all removed.

**JASP Desktop (`QMLComponents`):**

- `AppDirs::moduleExtraLibPaths(moduleRLibrary, moduleName)`: **Windows-only** (`#ifdef _WIN32`; on
  Linux/macOS it returns an empty list — symlinks already solve it there). Reads the module's own
  manifest (`<root>/manifests/<name>_manifest.json`, where `<root>` is two levels up from the module's
  library dir — install-tree Modules root for bundled, user modules root for installed), parses `mapping`, resolves
  `binary_pkgs/<hash>` against that root, and returns every hash dir that exists and does **not** have
  a `DESCRIPTION` at its root (old-layout guard).
- `DynamicModule::getLibPathsToUse()` now emits
  `c('<moduleRLibrary>', '<hashdir1>', …, '<rHome>/library')`:
  1. farm dir first — identical behaviour when the farm is healthy;
  2. hash dirs next — they *rescue* loading when farm entries are missing or unreadable,
     serving identical content (the farm junctions point into the same hashes);
  3. R's own library last — module-pinned versions win over base-library versions.

## Platform gating

All of this is Windows-only, in both places:

- manager: nesting + deeper junction targets behind `.Platform$OS.type == 'windows'`;
- JASP: `moduleExtraLibPaths` behind `#ifdef _WIN32` — on Linux/macOS `.libPaths()` stays
  `c(moduleRLibrary, R library)` exactly as today, and their trees keep the flat layout;
- junction_tool is Windows-only by construction.

## Farm retirement (step 3, implemented)

Once the tree ships only new-extraction packages the farm is pure dead weight. Changes:

- **Manager copy rule** (`installJaspModuleBundle`, Windows-only): `module_libs/<mod>/` gets real
  directory copies of exactly those packages that must exist under their own name inside the
  importer's entry — the module package itself and every dependency that is itself a JASP module
  (detected as `jasp*` minus the infra set jaspBase/jaspGraphs/jaspTools/jaspResults/
  jaspWorkarounds), because modules import each other's QML through relative paths that resolve
  positionally. ~34 cross-module edges ≈ 70 MB on disk. All other deps are served solely by
  their hash micro-libraries. `repairJaspModuleBundleByManifest` nests freshly downloaded hashes
  and rebuilds the entry, so repairing fully migrates old-layout installs. `createLink()` (and
  with it every junction creation for NEW installs) is no longer called on Windows; Linux/macOS
  symlinks are untouched.
- **Shared-hash heal for legacy installs** (`nestAndHealSharedHashes`, Windows-only): when a
  still-flat hash is nested and another installed (old-layout) module's manifest also references
  it, that module's `module_libs` junction is re-pointed one level deeper — the legacy entry's own
  idiom, instant and deduplicating — with a real dir copy as fallback if junction creation is
  blocked (AV, jasp-issues #4586). Junction doctrine: none in the bundled/shipped path, none for
  new user installs; legacy user entries may be repaired with junctions until the module is
  reinstalled/updated, at which point it migrates to the copy layout where only read/execute
  permissions are needed (no junction creation, no EFS/SID surface).
- **`AppDirs::bundledModulesDir()`**: always `programDir()/Modules` on Windows (like ZIP/portable).
  All consumers — manifests, `Tools`, `modules-settings.json`, module files — only ever read
  from the install tree.
- **Deleted machinery**: `createJunctions()` + first-run dialog + `bundledModulesInitialized` gate
  in `Desktop/main.cpp`, the `JunctionTool` target and `Desktop/junction_tool/`, the
  `collect-junctions` build step and every `junctions_map.txt` copy. Stale farm dirs in appData
  are harmless leftovers.

Old user installs keep their already-built junctions (module loading still starts from
  `module_libs/<mod>`); nothing new ever creates one.

## Old installs (the compat story)

Detection is purely structural — a hash dir with `DESCRIPTION` at its root was extracted by
the old manager:

| situation | behaviour |
|---|---|
| old extraction, bundled or user | hash dirs skipped → vector = `c(moduleRLibrary, R lib)` — status quo |
| old JASP + new extraction | old JASP ignores nothing — farm junctions still built from the map |
| new JASP + new extraction | farm + direct hash libpaths; engine works even with a broken farm |

A user updating a module through a new JASP reinstalls with the new manager and migrates
automatically; never-updated installs keep working through their existing farms.

## Performance: the find.package map (measured & implemented)

`find.package()` collects every match: one vectorized `file.exists()` sweep over *all* of
`.libPaths()` per lookup (no short-circuit — it must gather all candidate dirs before prepending
the loaded namespace's path and version-checking each; verified in the R 4.5.2 source), and
`system.file()` pays the same sweep on **every** call. Measured on Windows/NTFS (dev machine,
Defender active), R 4.5.2, 412 libpaths:

- per lookup ~20 ms (~50 µs/stat vs 3.5 µs on Linux)
- `library(<module>)` at engine start: +0.3–0.4 s (≈15 `loadNamespace` calls at load time)
- load-everything worst case: +9 s cumulative; realistic sessions sit far below that, but runtime
  `system.file()` calls made the unmitigated cost unbounded in principle

**Solution: a pkg → lib-dir map consulted before the sweep.** The manifest already knows which
micro-library holds which package, so the desktop sends a named vector alongside the libpaths:

- `AppDirs::modulePkgMap()` (Windows-only, cached) builds ordered `pkg => dir` pairs mirroring the
  `.libPaths()` order: the `module_libs` entry first, then the manifest's micro-libraries, then
  R's own library (covers base/recommended packages too).
- `DynamicModule::requestJsonForPackageLoadingRequest()` adds it to the module-load request as
  `modulePkgMap`; `Engine::receiveModuleRequestMessage()` stores it in
  `options(JASP.find.package.map)` and installs the fast-path **right there — the only moment
  that needs it**: the module-load request is what starts package loading (even jaspBase resolves
  during it), and patch + options persist for the session (`.libPaths` resets on later analysis
  calls cannot undo them). A module switch in-session just refreshes the option; the wrapper
  reads the map at call time.
- The fast-path (R snippet in `engine.cpp`) wraps — not replaces — stock `find.package`: for a
  **mapped** package it returns the loaded namespace's path when loaded (exactly what stock does)
  and otherwise the first `.libPaths()` hit (the map is built in that order); anything unmapped
  (base packages included) falls through to the original untouched. One `file.exists` check
  validates the mapped dir.
- Shipping mechanism: runtime rebinding (`unlockBinding` on base — JASP ships its own R) inside a
  `tryCatch`; if a future R forbids it we silently fall back to stock behavior (slow but correct)
  and log a warning. Chosen over patching the R build because the wrapper survives R upgrades by
  construction (it never reimplements the stock logic), and it deploys at JASP's cadence. A proper
  R-build patch remains the escape hatch if rebinding ever stops working.

Measured effect of the prototype: lookup 20.6 → 0.1 ms; `library()` 2.1–2.3 → **1.75 s (faster
than the junction farm ever was)**; load-everything 16.6 → 11.9 s (residual = R-library packages,
closed by including them in the map — done in the shipped version). Harness: `perftest.R`/
`perfmap.R` patterns from the dev build tree; re-verify on an installed MSIX before release.

## Rollout

1. ✅ Land the JASP-side + junction_tool changes (safe on old layouts by construction).
2. ✅ Switch the manager to nested extraction; verify a full Windows bundle and a user install.
3. ✅ Farm retirement — see the dedicated section above.

## Testing checklist

- [ ] Old-format bundle: `.libPaths` received by engine unchanged (regression check).
- [ ] Old-format user-installed module: unchanged (regression check).
- [ ] New-format bundle: engine log shows hash dirs in `moduleLibPaths`; analyses of every
      module family run on MSIX; delete a farm junction manually and confirm the analysis
      still runs (rescue property).
- [ ] New-format user install (and update-of-bundled-module): reads the *user* manifest,
      correct versions.
- [ ] Farm `binary_pkgs` junction resolves to the install tree (bundled hash dirs found).
- [ ] First-run with junction creation sabotaged (e.g. read-only appData): JASP retries on
      next start (exit-code fix).
- [ ] Upgrade old→new on a machine with an existing farm and existing user modules.
- [ ] Farm retirement: fresh MSIX/MSI install creates **no** `BundledJASPModules_*` dir in
      appData and no first-run dialog; bundled analyses still run.
- [ ] Shipped tree: `module_libs/<mod>/` contains real copies of the module pkg and its
      JASP-module deps only; no `junctions_map.txt`/`JunctionTool.exe` in the install prefix.
- [ ] Cross-module QML forms load (jaspLearnStats/jaspTimeSeries/jaspVisualModeling).
- [ ] Repair flow: delete a `binary_pkgs/<hash>`, trigger module repair — hash downloads,
      nests, and the module_libs entry is rebuilt.
- [ ] Pkg map: engine log shows `JASP find.package map active (N entries)` on module load;
      `find.package('<dep>')` resolves instantly; verify a package present in both R/library
      and a micro-library (e.g. Matrix) returns the loaded path when loaded and the micro-lib
      otherwise; module update in-session refreshes the map without reinstalling the wrapper.
