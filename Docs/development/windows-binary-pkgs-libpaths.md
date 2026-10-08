# Binary packages as direct R library paths (`binary_pkgs/<hash>/<pkg>`)

Status: proposed / in implementation · October 2026
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

**junction_tool (`Desktop/junction_tool/main.cpp`):**

- junction `binary_pkgs` into the farm alongside `manifests` and `Tools`, so the farm root
  mirrors the full module root and manifest-relative hash paths resolve for bundled modules;
- exit non-zero when any junction failed so JASP retries on the next start instead of
  persisting a half-built farm.

**JASP Desktop (`QMLComponents`):**

- `AppDirs::moduleExtraLibPaths(moduleRLibrary, moduleName)`: **Windows-only** (`#ifdef _WIN32`; on
  Linux/macOS it returns an empty list — symlinks already solve it there). Reads the module's own
  manifest (`<root>/manifests/jasp<name>.json`, where `<root>` is two levels up from the module's
  library dir — farm root for bundled, user modules root for installed), parses `mapping`, resolves
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

## Performance note

`find.package()` does one vectorized `file.exists()` sweep over `.libPaths()` per package
lookup (no short-circuit, verified in the R 4.5.2 source). With ~170–280 hash dirs per
module that is ~30–50k attribute checks per engine start — measured at ~3.5 µs/check warm
(Linux), so expect roughly 0.1–0.5 s once per engine start, zero per subsequent analysis.
DLL loading, lazy-load DBs and `.onLoad` are unaffected (they use the resolved path).
Backstop if a real-Windows benchmark disagrees: a ~5-line early-exit patch in
`find.package` (JASP already maintains R patches).

## Rollout

1. Land the JASP-side + junction_tool changes (safe on old layouts by construction).
2. Switch the manager to nested extraction; verify a full Windows bundle and a user install.
3. **Farm retirement (the goal)** — once a release ships only new-extraction packages:
   skip `createJunctions()` and the first-run dialog, stop creating `module_libs` junctions
   in the manager, read `manifests`/`Tools`/`modules-settings.json` directly from the
   install tree, and retire `junction_tool` + the `bundledModulesInitialized` flag. Old
   user installs keep their already-built junctions; nothing new ever creates one.

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
