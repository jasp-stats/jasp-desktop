# Handover — `wipJunction` (kill the Windows junction farm)

October 2026 · origin: jasp-issues #4586/#4566 · full spec: `Docs/development/windows-binary-pkgs-libpaths.md`

> **Update:** stage 2 (farm retirement) is now implemented on top of the additive transition —
> manager copy rule, `bundledModulesDir()` → install tree, junction machinery deleted.
> See "Farm retirement" in the spec. Still unvalidated: nothing is compiled or tested yet.

## Why

On Windows (MSIX/MSI) bundled-module loading depends on the "junction farm" rebuilt in
appData on first run (`BundledJASPModules_<ver>_<commit>_<date>/`). It is fragile:
per-entry junction creation can fail silently (AV, MAX_PATH) while junction_tool exits 0,
and newer Win11 (24H2/25H2) may EFS-encrypt ("Application Protected") farm files the
sandboxed engine can't read. Result: `unable to load R code in package …` /
`DLL not found: maybe not installed for this architecture?` crash loops (#4586).

Key insight: the engine can always read the install tree directly (jaspBase loads from
`WindowsApps/…/R/library` today). R only needs **name-keyed library dirs** — which the
nested `binary_pkgs/<hash>/<pkgname>/` layout provides without any links.

## The design (validated with real data)

- **Hash micro-libraries**: `nestBinaryPkgIfNeeded()` (manager) moves flat
  `binary_pkgs/<hash>` → `binary_pkgs/<hash>/<pkgname>` after install/repair. Each hash dir
  is then a valid `.libPaths()` entry. Idempotent; upgrades old installs in place.
- **Manifest is the source of truth**: no schema changes — the existing `mapping`
  (`"<hash> => <pkg>_<ver>"`) lists every package a module needs, incl. the module itself.
  `AppDirs::moduleExtraLibPaths()` parses it and returns the hash dirs (Windows-only,
  old-layout dirs skipped via `DESCRIPTION`-at-root check).
- **Engine**: `.libPaths()` becomes `c(moduleRLibrary, <hashdirs…>, R_home/library)` —
  farm first (identical behaviour when healthy), hash dirs rescue broken farms,
  R library last (module-pinned versions win). Engines are single-module → no version
  conflicts possible.
- **Cross-module QML is the trap**: modules import each other's QML via *relative* paths
  (`import "../../../jaspDescriptives"` — real cases: jaspLearnStats, jaspTimeSeries,
  jaspVisualModeling). These resolve positionally through the **importer's own
  module_libs dir**, so name-keyed entries for JASP-module deps must survive.
- **Measured facts**: 38 modules, **34 cross-module edges** (16 importers, avg 2.1, max 5;
  jaspTTests ×12, jaspDescriptives ×10 dominate). Bundled module package ≈ **0.6 MB
  compressed / ~2 MB extracted** (source trees are 6–20× bigger — don't measure from
  source). Perf: the longer `.libPaths` costs one vectorized `file.exists()` sweep per
  package lookup in `find.package` (no short-circuit, verified in R 4.5.2 source) ≈
  30–50k stats ≈ 0.1–0.5 s once per engine start (measured 3.5 µs/stat warm on Linux);
  DLL/lazyload/.onLoad unaffected. Backstop if needed: ~5-line early-exit patch in
  `find.package` (JASP already maintains R patches).

## What's in this branch

| file | change |
|---|---|
| `QMLComponents/utilities/appdirs.h.in` + `.cpp` | `AppDirs::moduleExtraLibPaths()` — manifest `mapping` parser (`manifests/<name>_manifest.json`), `#ifdef _WIN32`, mutex-guarded cache, old-layout guard (`appdirs.h` is generated — build regenerates it) |
| `QMLComponents/modules/dynamicmodule.cpp` | `getLibPathsToUse()` appends the hash dirs |
| `Desktop/junction_tool/main.cpp` | exit ≠ 0 when any junction fails (no more silent half-farms; Desktop already retries because the `bundledModulesInitialized` flag isn't written) + `binary_pkgs` added to farm special-dirs |
| `Engine/jaspModuleBundleManager` (submodule, branch `wipJunction`) | `nestBinaryPkgIfNeeded()` in `utils.R`; nesting pass + junctions one level deeper in `installJaspModuleBundle()` — both Windows-only |
| `Docs/development/windows-binary-pkgs-libpaths.md` | full spec: problem, changes, compat matrix, perf, rollout, testing checklist |
| `Docs/development/windows-sandbox-efs-save-crash.md` | (was untracked) EFS/sandbox research doc, cross-linked |

## What is NOT done yet (the farm-retirement step)

The branch is the **additive transition**: farm still built, engine no longer depends on
it. Full removal — decided design, not yet implemented:

1. **Manager copy rule** (replaces the junction creation on Windows): `module_libs/<mod>/`
   gets **real dir copies** of (a) the module's own package and (b) every dep that is
   itself a JASP module (~34 edges × ~2 MB ≈ **70 MB on disk** — fine). All other deps
   exist only as hash micro-libraries. Detection: dep name in manifest `to` ∩ Official
   module names (or pragmatic: name starts with `jasp` minus the R/library infra set
   jaspBase/jaspGraphs/jaspTools/jaspResults/jaspWorkarounds). `createLink()` on Windows
   gets deleted; Linux/macOS keep symlinks untouched.
2. **`AppDirs::bundledModulesDir()`**: MSIX/MSI → `programDir()/Modules` (like portable).
   All consumers derive from it (manifests, Tools, modules-settings.json, module files)
   and only ever read.
3. **Delete machinery**: `createJunctions()` + dialog + `bundledModulesInitialized` in
   `Desktop/main.cpp`, the `junction_tool` target, and the build-time `junction_tool -s/-sd`
   scan + `junctions_map.txt` shipment (build pipeline, outside this repo's CMake?).
4. **Stale farm dirs** in appData: harmless leftovers; old versions already get cleaned.
5. Optional sweetener: auto-repair of broken old user modules —
   `repairJaspModuleBundle` downloads missing hashes standalone (`complete:false`
   bundles already rely on it) + nesting heals them without junctions.

## Compatibility guarantees (verified reasoning, not yet field-tested)

- Old-format installs (manager < this change): manifest has no usable micro-libraries →
  extra libpaths empty → vector = today's `c(moduleRLibrary, R lib)` → farm serves them.
- Old JASP + new tree: junction_tool map targets are simply one level deeper — works.
- Known accepted caveat: a *new*-manager user install can't be read by an *older* JASP
  still installed side-by-side (expects junctions/flat layout). Rare; document in release
  notes or keep junction creation for user installs one extra release.
- Dev modules, renv static cache, user-module installs into Roaming: unaffected.

## Validation status — IMPORTANT

**Nothing is compiled or tested.** (No Windows toolchain here; the project's clangd can't
resolve includes even for untouched files.) Before merge:

- [ ] Windows build of jasp-desktop + submodule; regenerate `appdirs.h` from `.h.in`.
- [ ] Run the manager's testthat suite; build one test bundle, install on Windows,
      check `binary_pkgs/<hash>/<pkg>/DESCRIPTION` and the manifest-driven libpaths
      appear in the engine log (`moduleLibPaths` in moduleLoadRequest).
- [ ] Farm-sabotage test: delete junctions → analyses still run (rescue property).
- [ ] Old-format regression: engine vector unchanged, farm path used.
- [ ] Cross-module QML: jaspLearnStats/jaspTimeSeries/jaspVisualModeling forms load.
- [ ] Full checklist in `windows-binary-pkgs-libpaths.md`.

## Submodule note

`Engine/jaspModuleBundleManager` changes live on its own `wipJunction` branch pushed to
github.com/jasp-stats/jaspModuleBundleManager; this branch records the bumped submodule
pointer. Merge both PRs together.
