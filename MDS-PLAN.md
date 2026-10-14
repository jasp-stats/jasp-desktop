# MDS-PLAN — Multi-Dataset Provenance: Filter-Scoped Encoders, Term Provenance, Component-Owned State

> **Status conventions for implementing agents**
> - Tick cards (`- [ ]` → `- [x]`) as they land. Commit the tick together with the code it describes.
> - If reality forces a deviation, do **not** silently diverge: append a `> NOTE (agent, date): ...` line under the affected card explaining what changed and why. All plan edits are tracked in git — good deviations get reviewed, bad ones get reverted.
> - Never rewrite or reorder existing cards. Append only.
> - **Phase 9 deletes this file.** That is the completion signal for the whole plan; until then, it stays in the repo.
> - Commit prefix: `mds-plan D<n>:` (e.g. `mds-plan D1.3: canonical mint split`). Submodule pins commit *with* the code they pair with.

---

## 1. Design constitution

These are settled decisions from a full design session (2026-10-14). Each has a rationale; do not "optimize" one away without re-litigating it in the commit message.

- **C1 — Encoded name format: `JASPColumn_D<dsId>_F<filterId>_<index>_Encoded`.** Both segments, self-describing for inspection. Minting semantics unchanged (alphabetical counter per dataset, `.scale`/`.ordinal`/`.nominal` triples + bare name — `columnencoder.cpp:194-237`). The wire format is **identical** under every alternative architecture considered (per-filter views / canonical-blocks / global-table); if internals must change later, the format must not.
- **C2 — `DataSet` owns the canonical encoder = the *mint* only** (`name → (index, type)` + reverse). No decode maps on the canonical.
- **C3 — `Filter` = namespace = slice.** A `Filter` owns its prefix (`D<ds>_F<f>`) and a thin **encode handle** (non-owning `{canonical*, prefix}`; encode = canonical index lookup + own prefix compose). No per-filter materialized encode maps.
- **C4 — One global decode table** (static on `ColumnEncoder`, where `_currentEncoder`/`_otherEncoders` live today): `fullEncodedString → (originalName, type)`, containing **all** filter blocks of all loaded datasets **plus** the extras block. Single writer: `rebuildBlock()` — **erase the block's bounded key space first, then write** (closed under: rename re-mint, column count shrink, filter drop). Static `ColumnEncoder::decode(string)` is *the* decode entry point; O(1), context-free, strict (no hit ⟹ raw). Decode stays dumb and garbage-proof — **no grammar parsing anywhere** (that was the rejected Option B: false-positive-prone, and R already suffered it).
- **C5 — `_otherEncoders` survives only in an encode-only role** (extras-union in `encodeAll`/`encodeJson` used by options encoding, engine-side — `columnencoder.cpp:263-317,596-604`). All decode-side merge (static getters `:319-414`, `decodeJsonAcrossEncoders`/safeHtml `:616-654`) dies with the global table. The **replacer-encoder** (`_For_Replacement` prefix, `:99-113`, used by `replaceColumnNamesInRScript` `:749-764`) is untouched and registers nowhere.
- **C6 — Extras encoder (`JaspExtraOptions_`, DataBridge-owned, `databridge.cpp:28`, `databridge.h:126-127`)**: stays **engine-side**, populated **per request** from send-time `.meta["encodeThis"]` (`factorlevellistbase.cpp:107-121` → `setCurrentNamesFromOptionsMeta`, `columnencoder.cpp:701-737`; callers `engine.cpp:804`, `syntaxbridge.cpp:660` — **both must survive the meta inversion in Phase 3**). Its block is a first-class citizen of the global decode table: extras tokens decode at *every* decode site. (Known current gap to fix along the way: `enginerepresentation.cpp:763` decodes dataset-only.)
- **C7 — `currentEncoder` flipping ends on aware paths.** Encode always via an explicit handle resolved from the term's `filterId`/slice; decode is context-free by construction. The safeHtml `currentEncoder` flip dies. `currentEncoder` survives solely as the documented **legacy fallback** for non-aware runs and `filterId == -1` values. Bonus invariant: aware runs mutate no process-global state.
- **C8 — `.meta` survives but is generated at send time.** It never contains `dataSetId`/`filterId` — provenance is **in the name**. Everything else in today's meta (`shouldEncode`, `isRCode`, `filterForValues`, `loadFilteredData`, `encodeThis`) keeps working downstream unchanged. **Engine→R downstream is not rewritten.**
- **C9 — `Term` carries a typed `filterId`** (int, `-1` = unqualified, part of `operator==`/hash), tracked exactly like `types`. Enables same-name-different-source within one list.
- **C10 — Selection lives on `AvailableVariablesList`, not `VariablesForm`.** Each selection host registers with the analysis. **`AnalysisBase` implements `VariableInfoProvider`** — the `Filter::columnEncoder()` analogue (`filter.cpp:473-479`) — over the registered selected filters (one ⟹ its handle; many ⟹ merged). No `AnalysisVarForm` — that name/class does not and will not exist.
- **C11 — Components own state; JSON is generated on demand** in two shapes: `Engine` (current R contract + send-time `.meta` + **transient** per-term `filterId`) and `Storage` (clean typed state). `_boundValues` is deleted from `AnalysisBase`/`AnalysisForm`. Transient filterIds are consumed by the slice-queue build and **stripped at the engine boundary** — R never sees provenance.
- **C12 — Pre-attach options ride the existing `setAnalysisUp`/bindTo handoff** (`analysisform.cpp:838-883`): pending JSON is applied once when the form attaches, then discarded and **never read** by run/dataSpec/save paths. QML load failure ⟹ no analysis ⟹ no state case.
- **C13 — One resolution invariant everywhere** (see §4 table): qualified always wins; fallback is per-context — *fixed* for filters (own DataSet, always), *selectable* for computed columns/datasets (input-filter selection **is** the fallback encoder), *host-bound* for analyses, `currentEncoder` for legacy.
- **C14 — No token grammar in R. Ever.** jaspSyntax's regex decoders (`columnDecoder.R:143-158`, `readDatasetFromJaspFile.R:256-257`) are **deleted, not reformatted** (unmerged ⟹ zero compat debt). All R encode/decode goes through the bridge into C++ (`syntaxfunctions.cpp:540`, rbridge callbacks `rbridge.cpp:185-218`). R-side decode callbacks themselves are **permanent** (plot re-render `writeImage.R:185-338`, read-back `jaspTable.cpp:533-581`, print/toHtml) — they just delegate to the global table.
- **C15 — Version stays 7.0.0** (bump at `jaspexporter.cpp:45` is branch-local/unreleased). **No `.meta` stamp migration** (stamps never shipped). Released files: plain names in options (⟹ `filterId = -1` ⟹ fallback rules, *by design, forever*) and old bare-prefix `JaspColumn_<i>_Encoded` (`columnencoder.h:199`) in persisted results — handled by a small best-effort **legacy-prefix result decoder** built from current column order.
- **C16 — Deleted-filter tokens** in old persisted results go raw (block erased on filter destroy). Tombstones explicitly **not planned** (see Follow-ups).
- **C17 — Plot decode reality check:** interactive plotly JSON is decoded **C++-side** at `Engine::sendString` (`engine.cpp:812-836`); R-side decode covers static images/recipes and read-backs. Both routes stay; both become consumers of the one global table.

## 2. Repos & branches

| Repo | Branch | Upstream state | Notes |
|---|---|---|---|
| jasp-desktop | `dataSetAwareVarForm` | 3 behind `upstream/development` → **rebase (Phase 0)** | C++/QML |
| Engine/jaspBase (submodule) | `multiDataSetAware` | 0 behind `origin/master` ✅ | pin contains master tip |
| Tools/jaspTools (submodule) | `multiDataSetAware` | 0 behind `origin/master` ✅ | pin contains master tip |
| ../jaspSyntax | `multiDataSetAware` | 0 behind `origin/master` ✅ | separate checkout, not a submodule |
| ../jaspTestModule | `master` | 0 behind, 28 ahead ✅ | RPC verifier + demo analyses |

## 3. Phases

### Phase 0 — Rebase

**Goal:** branch tip = upstream/development + our 42 commits, everything green, before any new work.

- [x] **D0.1** `git fetch upstream && git rebase upstream/development` in jasp-desktop. Known conflict set: `AGENTS.md`, `CommonData/dataset.*`, `CommonData/filter.*`, `Desktop/analysis/analysis*.cpp`, `Engine/engine.*`, `QMLComponents/analysisbase.cpp`, `QMLComponents/analysisform.cpp`, `QMLComponents/models/listmodelfiltereddataentry.cpp`, `Resources/JASP_RPC.json`, `Tests/testall.*`, `Tests/CMakeLists.txt`. Resolve preserving multi-dataset semantics (upstream side is mostly #6313 fuzz tests + tooltip fix).
  > NOTE (agent, 2026-10-14): Rebase landed: branch = upstream/development (tip 36135026a) + our 43 commits (was counted as 42; upstream had also gained the Weblate translations commit #6346). Real conflicts only on 3 commits — the first big feature commit (analyses.cpp / JASP_RPC.json: upstream's new `analysis_remove` kept alongside our `analysis_setFilter`; Tests/CMakeLists.txt: gatetest smokes + our Rlogic test both kept), `fed147e` (listmodelfiltereddataentry.cpp: upstream's re-bind crash-fix merged with our `effectiveVarInfo()` resolution), and `6ab9ad3` (AGENTS.md factual merge). Everything else auto-merged; CommonData, QMLComponents, JASPEngine, JASPDesktopLib all compile post-rebase, no fixup commit needed.
- [x] **D0.2** Submodule pins: keep **ours** for `Engine/jaspBase` (bd4720d) and `Tools/jaspTools` (82a877a) — both already contain current `origin/master` tips; take **upstream's** bump for `Engine/jaspModuleBundleManager`.
  > NOTE (agent, 2026-10-14): jaspBase/jaspTools pins kept (verified: upstream's new pins a1f5d9b/1a10943 are ancestors of ours). For jaspModuleBundleManager our range genuinely moved the pin (c150f56 → a9e61a4, the Arch-friendly Rcpp/RInside renv work), so per the fallback rule **ours (a9e61a4) is kept** instead of upstream's a7eb44e. The pins diverged at c150f56: upstream's side carries `moduleName` param for createJaspModuleBundle + junction-creation retirement — not in our pin; revisit if the bundle manager needs those. New upstream submodule `Tests/gatetest/jasp-mcp` (72d9d7be) initialized.
- [x] **D0.3** Full test matrix (see §6); rerun the two flaky binaries before diagnosing anything.
  > NOTE (agent, 2026-10-14): Matrix executed post-rebase @ 3223d8413. **Rebase verified clean** — every failure reproduces byte-identically on pure upstream/development (36135026a, built in a scratch worktree). D0.3 is ticked with this **known-red baseline** (upstream-inherited, do NOT chase in later phases): (1) `testScriptConstructorGoldenR` 2 rows + `testScriptConstructorMirroredKeys` (merged red in upstream a7369831d #6306); (2) `TestDoublefieldDecimals::test_4setValueWithTooManyDecimals` (merged red in fd42a3413); (3) `JASPTestDbMigration` in-memory `DatabaseInterface::create()` `_dbCreator`-ordering failure (deterministic; one-liner fix identified upstream-side, left for an upstream PR); (4) `testMainWindowShowsFilterWindow` qmlLoaded-spy fails in full-suite order on our branch, passes standalone (broken upstream in every config; ours is the better symptom). Obsolete caveat: `testDataImport` is now GREEN (upstream trimming change). Matrix drift: `Tests/CMakeLists.txt` additionally registers `JASPTestBatchResult` + `JASPTestBatchInputsModel` (ctest) — run those too; effective matrix is 10 executables.

**Acceptance:** rebase complete, all 7 test executables green, no pins regressed.

### Phase 1 — Encoder architecture: canonical mint + global decode table + filter handles

**Goal:** C1–C7 in place. The entire name-space layer runs on the new structure while `.meta` provenance (still meta-driven) keeps working — this phase changes *machinery*, not *ownership of state*.

- [x] **D1.1** Split `ColumnEncoder` (`Common/columnencoder.{h,cpp}`): canonical instance per `DataSet` keeps only mint state (encoding map `name→(index,type)`, reverse `index→(name,type)`); delete instance-rooted decode map usage from the decode path; introduce **static global decode table** + `rebuildBlock(prefix, entries)` (erase-then-write) + static `decode(string)` / `columnTypeFromEncoded(string)` / `decodeJson(json)` over it. Kill decode-side merge (`:263-317` decode halves, `:319-414` statics, `:616-654` across/safeHtml). Keep encode-union per C5. Add `resetForTests()`; document per-process ownership (desktop, engine, syntax-bridge each keep their own — deterministic minting keeps them identical).
  > NOTE (agent, 2026-10-14): Implemented as three modes on the one class: `createCanonical()` (mint-only: `_mintForward` name→index + `_mintReverse` index→(name,type), refuses encode/decode/decodeAll/decodeJson/columnTypeFromEncoded — throws; `shouldEncode` = mint membership), `createView(canonical, prefix)` (non-owning {canonical*, prefix}, composes `prefix + index + "_Encoded"` per call, **no materialized per-view maps**; encode-side `encodeAll/encodeJson/encodeRScript` compose on the fly and still union `_otherEncoders` per C5), and FULL (today's self-contained instance: mint + materialized legacy string maps + registers its own table block — covers the process-global fallback encoder, the extras encoder (C6 — feeds the table through its own remint, not through caller merging) and ad-hoc/test instances). The replacer-encoder (`_For_Replacement`) registers nowhere per C5 and keeps its hacked private `_decodingMap` path. The global table is `std::map<std::string, DecodeEntry>` behind static `ColumnEncoder::_decodeTable()`; single writer `rebuildBlock()` erases the block's bounded key space (`lastPrefix + [0, lastCount) + lastPostfix`) before writing, which closes over rename re-mints, shrinks and prefix reforges. Static entry points are named `tableDecode/tableShouldDecode/tableColumnTypeFromEncoded/tableDecodeAll/tableDecodeJson` for now because C++ forbids coexistence with the instance shims kept for D1.3–D1.6 (same name+signature); **D1.5 renames these to the C4 names as the shims retire**. `decodeJsonAcrossEncoders` keeps its signature but ignores the encoder vector (context-free sweep); the safeHtml currentEncoder-flip is gone (delegation-only removal, as sanctioned). Canonical `setCurrentNames` rewrites every registered view's block automatically — that is the D1.4 funnel.
- [ ] **D1.2** `Filter`: own prefix `JASPColumn_D<ds>_F<f>_` (set after `dbCreate`/`dbLoad` where rowid is final — same "not in ctor" discipline as `dataset.cpp:566-573`; ids are stable persisted rowids, pinned by `testDataSetFilterIdsSurviveStorageReload`); thin encode handle returned by `Filter::columnEncoder()` (`filter.cpp:473-479` — no longer delegates to the dataset's map for encoding); block minted at create/dbLoad, **erased at destroy**.
- [ ] **D1.3** `DataSet::encoder()` becomes canonical (mint-only) + compatibility shim → shown dataset's shown/default-filter handle. Migrate the 18 call sites: `filter.cpp:478`, `workspace.cpp:240/436/871`, `databridge.cpp:177/178/232/367`, `column.cpp:396`, `datasetprovider.h:61` + `datasetprovider.cpp:165/189`, `qmlutils.cpp:55`, `enginerepresentation.cpp:763`, `analysis.cpp:825/828`, `syntaxbridge.cpp:242`.
- [ ] **D1.4** `rebuildBlock` trigger funnel (single writer, all mutations funnel): `setCurrentNames`, `updateColumnTypesOnly`, filter create/destroy, dataset load/unload, extras `setCurrentNamesFromOptionsMeta`, `Workspace::dbLoad`. **This list is also what makes the engine process propagate for free** — engine is a DataBridge on the same sqlite db (`engine.cpp:74`) running the identical `dbLoad → setupEncoderPrefix → eager loadStoredFilter → Filter::dbLoad` path (`dataset.cpp:813/820-826`, `filter.cpp:130`) with pull-based per-request refresh (`checkForUpdates`, `dataset.cpp:946-990` + `provideAndUpdateDataSet` `databridge.cpp:158-184`).
- [ ] **D1.5** Converge every decode site on the static entry: `Engine::sendString` (`engine.cpp:812-836`, safeHtml flip removed), `Workspace::decodeJsonAcross`/`decodeJson` **deleted** (`workspace.cpp:427-448`, `:871`), `enginerepresentation.cpp:763` (now decodes extras too — fixes the dataset-only gap), `Analysis::loadPlotlyJsonInResults` (`analysis.cpp:818-827`), rbridge callbacks (`rbridge.cpp:185-218` — the hand-rolled "extras first, then current" chain collapses into one static call).
- [ ] **D1.6** Encode boundaries: `provideAndUpdateDataSet` (`databridge.cpp:177`) keeps `setCurrentEncoder` **only** as the legacy/non-aware fallback (C7); `prepareMultiDataSetRun` resolver (`databridge.cpp:232`) → **filter handle** (per meta stamp *this* phase — stamps die in Phase 3; dual-read the resolver so it survives the transition); slice handoff (`rbridge.cpp:510-564`) `encodeAll` at `:553` via the slice filter's **explicit handle**, not the current encoder.
- [ ] **D1.7** `ColumnEncoderContext` → v2 (`columnencodercontext.h`, `syntaxbridge.cpp:230-244`): context = host filter's prefix/view; replay decode via global table.
- [ ] **D1.8** jaspSyntax: **delete** `.containsEncodedBridgeColumnTokens()` (`columnDecoder.R:143-158`) and `readDatasetFromJaspFile.R:256-257` regexes; all decode/encode through native (`RcppExports.R:102-107` → `syntaxfunctions.cpp:540`) and rbridge callbacks. Re-pin contract tests via **native roundtrips**: `test-desktop-jasp-contract.R:130-135`, `test-native-column-context.R:43-59`, `test-module-options.R:137-140` ⟹ `^JASPColumn_D[0-9]+_F[0-9]+_[0-9]+_Encoded$`. Any testthat suite decoding without a live bridge must boot the bridge.
- [ ] **D1.9** Test matrix (full sweep; see §6).

**Acceptance:** same-name column in two filters of one dataset ⟹ two distinct encoded tokens, both decode; deleted filter's tokens ⟹ raw; no regex survives in jaspSyntax; extras tokens decode at every site; engine-process registry verified (add/rename column in desktop ⟹ next engine request decodes correctly).

### Phase 2 — Term provenance + selection on the list + analysis as provider

- [ ] **D2.1** `Term` (`term.h:34-97`, `term.cpp:36-77,198-225`): `_filterId` member + accessors, default `-1`, part of equality/hash; `Terms` carries it through rows (`terms.cpp:520-548,551-573,588-608`). C++/QML typed state only — serialization arrives with Phase 3's generators.
- [ ] **D2.2** `HostFilterSelection` (`hostfilterselection.{h,cpp}`) from `VariablesFormBase` (`variablesformbase.h:39-115`) → `VariablesListBase`: QProps `dataSetSelectionOption`/`selectionAvailable`/`filterSelectionValues`/`selectedFilterId` land on the list; `FilterSelect` (`FilterSelect.qml:38-95`) re-homed into list chrome; `VariablesForm`/`VariablesForm.qml:93-124` stripped hard (no shims — pre-release feature).
- [ ] **D2.3** Selection-host registry on `AnalysisForm` (extends `registerDataSetSelectionOption` path, `analysisform.cpp:640-710`); `selectionDependency`/`sortControls` (`:313-330`) order each list's FilterSelect before what it serves.
- [ ] **D2.4** `AnalysisBase : VariableInfoProvider` (C10): `columnEncoder()` = single selected filter's handle, else merged; `provider()`/varInfo aggregates the registered filters. `TextAreaBase` host (`textareabase.h:50-124`) registers alike.
- [ ] **D2.5** Host-switch propagation: analysis-side (dataSpecChanged, re-run trigger, queue membership) + list-side (VariableInfo signals → models → **typed** term re-stamp; replaces `restampValuesUnderHost`, `hostfilterselection.cpp:181-205`).
- [ ] **D2.6** Test matrix + rework `tst_dataSetSelectionVariablesForm.qml` (16 tests) to list-level selection.

**Acceptance:** two lists, two selections, one form ⟹ independent namespaces; switching a list restamps its own controls' terms only; `testSelectionRegistryLateHostAppliesPendingValue`, `test_forms_select_independently`, `test_textarea_host_selects_independently` green (adapted).

### Phase 3 — The inversion: components own state, JSON generated

**Goal:** C11–C13. **Big-bang cutover** (typed state ⟷ JSON coexistence is forbidden — two sources of truth is the disease). Staged commits: per-control-family generators → flip read paths → delete the tree. **Start only immediately after a full-matrix-green.**

- [ ] **D3.1** `BoundControlBase`: typed value state + `changed` signals; recursive `createJson(JsonFormat)` (`Engine` | `Storage`) on every bound control → `AnalysisForm` → `AnalysisBase`/`Analysis`.
- [ ] **D3.2** Engine json = today's R contract + **send-time `.meta`** (`shouldEncode`/`isRCode`/`filterForValues`/`loadFilteredData`/`encodeThis` — content of `boundcontrolbase.cpp:35-73` minus provenance) + **transient per-term `filterId`**.
- [ ] **D3.3** Consume-and-strip: `prepareMultiDataSetRun` (`databridge.cpp:186-282`) builds the slice queue from term filterIds (+ analysis' own filter); strip at `engine.cpp` (~:893, just before `rbridge_runModuleCall`) and the syntax twin (`syntaxbridge.cpp:675-683`). Engine-side `updateOptionsAccordingToMeta` (`databridge.cpp:370-467`, loadFilteredData injection) **stays**, fed by send-time meta. Extras population calls (`engine.cpp:804`, `syntaxbridge.cpp:660`) untouched (C6).
- [ ] **D3.4** Delete: `_boundValues` (`analysisbase.h:69/79/153`, `analysisbase.cpp:269-298`), `createMeta` stamping (`boundcontrolbase.cpp:35-73`), `collectDataSetIdsFromMeta`/`collectFilterIdsFromMeta` (`columnencoder.cpp:993-1030`), `encodeColumnNamesinOptionsPerDataSet` lockstep resolver (`columnencoder.h:47-49,133-140`), `_adjustBindingValue`/`_adjustBindingType` (`boundcontrolterms.cpp:50-100` — bindTo copies typed `Terms`, provenance rides the object), `getOptionsWithRelatedValues` JSON shuffling (`terms.cpp:551-573` — rows aggregate children on demand), `optionsMeta()`.
- [ ] **D3.5** Kept as the JSON→state appliers (they already write *through* controls — that *is* the target shape): `AnalysisForm::parseOptions` (`analysisform.cpp:402`), `setOptions` (`:136`), `setAnalysisUp` (`:838-883`), RPC `analysis_run` (`analyses.cpp:1391-1479`, `:1411`), `analysis_create`/`setFilter` (`:1302-1389`). Pre-attach window: pending JSON applied at `setAnalysisUp`, discarded, never read (C12).
- [ ] **D3.6** `referencedDataSets()`/`usesDataSet()`/`dataSpec()` (`analysisbase.cpp:330-401`) computed on demand from component state (term filterIds + host registry).
- [ ] **D3.7** Request encoding = single walk of generated engine json: term `filterId ≥ 0` ⟹ its filter handle; else `currentEncoder` fallback. `BoundControlFilteredTableView`'s anchor (`boundcontrolfilteredtableview.cpp:43-55`) reads its bound list/host, not stored meta.
- [ ] **D3.8** Test matrix + RPC verifier + jaspTestModule round-trip checks.

**Acceptance:** no `_boundValues` in the tree; a run request produces byte-comparable options to Phase-2 behavior (modulo provenance location); RPC create/update/run and syntax parse all write through component setters; race-class tests (mid-session host switch, reopen-never-run) green.

### Phase 4 — Delete the varInfo-resolution walk

- [ ] **D4.1** Remove `JASPControl::effectiveVarInfo/effectiveVarInfoImpl/sourceVarInfo/selectionHost/effectiveSelectionFilter` (`jaspcontrol.cpp:950-1031`, `jaspcontrol.h:173-216`, `jasplistcontrol.cpp:46-70`).
- [ ] **D4.2** Migrate consumers to: their own list's host, the analysis provider, or `term.filterId → Workspace::filterById → handle`: `sourceitem.cpp:106,209-216`, `listmodelcustomcontrasts.cpp:44-47`, `listmodelfiltereddataentry.cpp:22-36`, `textareabase.cpp:66-76`, `formulasource.cpp:382`, `boundcontroltextarea*.cpp`, `listmodel.cpp` type-request path (`:340-342`).
- [ ] **D4.3** Test matrix.

**Acceptance:** zero references to the deleted API; grep-clean.

### Phase 5 — Storage format break (stays 7.0.0) + minimal upgrade

- [ ] **D5.1** `options` in analyses.json = `createJson(Storage)`: terms as `{name, filterId?, types, optionKey?}` (filterId omitted when `-1`); analysis-level fields unchanged (`analysis.cpp:867-911` — `filterId`/`dataSetId`/`multiDataSetAware` stay).
- [ ] **D5.2** **No stamp migration** (C15). Old options parse as today with all-`-1` filterIds ⟹ fallback rules, permanently supported.
- [ ] **D5.3** Legacy-prefix result decoder: one best-effort shim materializing old bare `JaspColumn_<i>_Encoded` tokens from current column order, run once at load for pre-existing results (hook beside the `do019Fix`/`do0961Fix` pattern — `workspace.cpp:99-117` threading + `Analyses::loadAnalysesFromJaspFileJson`, `analyses.cpp:490`). Honest limitation in code comment: column churn since save ⟹ raw fallback; deleted filters ⟹ raw (C15/C16).
- [ ] **D5.4** Upgrade-path tests: released-format file ⟹ loads, options resolve via fallback, old result tokens decode (unchanged columns) or go raw (churned) without crashing; re-save writes Storage format; `testJaspArchiveVersionBlocksOlderBuilds` stays green (version unchanged).

### Phase 6 — Test the component zoo (jaspTestModule)

**Goal:** every control family demonstrably handles multi-source provenance.

- [ ] **D6.1** New analysis `testMultiDataSetControls` (qml + R echo fn) exercising: **AssignedVariablesList** (second source), **TableFields-style list with row controls** (per-row terms keep own filterId; replaces `getOptionsWithRelatedValues` machinery), **InteractionTermList** (components share term-level filterId — validates per-term granularity), **FactorLevelList** (encodeThis ⟹ extras, end-to-end), **textarea with own host + qualified references**.
- [ ] **D6.2** Selection on individual lists + at least one **bindTo across differently-selected hosts** (typed copy carries source filterId).
- [ ] **D6.3** R echo fn reports received (encoded) names per control; RPC verifier asserts: same-name columns via different filters ⟹ **distinct** encodings per control type; scalars carry no provenance anywhere (M5 contract); mid-session list switch ⟹ downstream re-encode correct.
- [ ] **D6.4** `testMultiDataSet.qml` updated to list-level selection (Phase 2 API); stale `getSelectionSlice` doc line in `R/multiDataSet.R:10` fixed.

### Phase 7 — Data-side slices (⚠ own design note first)

- [ ] **D7.0** **Design doc** (small md, separate commit): attached-`data` model ⟶ generated fully-qualified paths; how multi-slice references inside a filter resolve at run; how easy-mode constructor emits qualified refs. Do not code before this is reviewed.
- [ ] **D7.1** `FilterWindow.qml` (easy constructor `:175`, R editor `:315`): **no fallback selection UI** (own DataSet is the fallback **by definition**, C13); add outward multi-slice referencing (qualified-name insertion/autocomplete from other loaded slices; rCode stays raw in storage, `column.cpp:547` unchanged contract).
- [ ] **D7.2** `ComputeColumnWindow.qml` / `ComputeDataSetPanel.qml`: existing input-filter selection (`ComputeDataSetPanel.qml:109-125`, `defaultInputFilterId`) codified as *the fallback encoder* for bare names; qualified always wins.
- [ ] **D7.3** Engine: `runFilter`/`runComputedColumn` (`engine.cpp:573-607`) take explicit slice context instead of process-global current (`:592`).
- [ ] **D7.4** Visual script constructor: `ScriptNodeColumn::toR` (`scriptnode.cpp:554-569`) emits the qualified surface form when `_dataSetName` is set (the field already exists, unused — `scriptnode.cpp:545-546`).

### Phase 8 — Syntax / RSyntax / user grammar + final matrix

- [ ] **D8.1** User-qualified grammar (§5): parse+render helpers mirroring `Workspace::resolveDataSetSelectionValue`/`datasetSelectionSurface` (`workspace.cpp:567-669`); wire into syntax-bridge parsing, textarea context replay, filter editor references, script re-parse.
- [ ] **D8.2** RSyntax term emission: `_transformInteractionTerms` (`rsyntax.cpp:594-629`) and `FormulaSource::toString`/`generateInteractionTerms` (`formulasource.cpp:202-212,262`) qualify terms outside the context default namespace (render: qualified only when needed — scripts stay readable; re-parse resolves via D8.1 into typed terms with filterIds).
- [ ] **D8.3** Wrapper generation: replace `data <- datasets[[1]]` ambiguity pin (`rsyntax.cpp:313-314`) with explicit per-slice selection defaults (title surface already renders, `:41-63,287-289`); **fix** `Analysis::generateWrapper()` not passing `multiDataSetAware` (`analysis.cpp:1133-1143` vs bridge path `syntaxbridge.cpp:729,759`). `.meta` skipping in script output (`rsyntax.cpp:141,263`) stays correct.
- [ ] **D8.4** jaspSyntax per-list selection surface (ids invisible — C4-era rule holds); `columnEncoderContext` inventory: shrink to host-filter-prefix or delete if unused.
- [ ] **D8.5** Docs: `multiDataSet.R` contract rewrite (generated `.meta`, provenance-in-name), AGENTS.md encoder-architecture paragraph.
- [ ] **D8.6** New regressions: term.filterId Storage-json roundtrip; bindTo typed copy across hosts; pre-attach buffering; 7.0.0 legacy-token upgrade pass; same-name-different-filter flagship case (also per-control in D6).
- [ ] **D8.7** **Full final matrix** (§6) across all five repos.
- [ ] **D8.8** **Gate/fuzz smoke (deferred here per plan owner, 2026-10-14):** run `gateSmoke`/`fuzzSmoke` (ctest `gatetest` label; needs built `JASP` + `Tests/gatetest/jasp-mcp`) as final validation, including against a `jaspTestModule`-driven session (multi-dataset analyses included). Fixing the pre-existing upstream reds from the D0.3 baseline is *not* in scope; new reds introduced by this plan are.

### Phase 9 — Cleanup + plan deletion

- [ ] **D9.1** Dead-code sweep of everything this plan orphaned (old resolvers, legacy test fixtures, unused shims — grep for `collectFilterIds`, `encodeColumnNamesinOptionsPerDataSet`, `effectiveVarInfo`, `decodeJsonAcross`, regex decoders).
- [ ] **D9.2** `git rm MDS-PLAN.md` — the completion signal. Commit message: `mds-plan: plan complete, remove plan`.

## 4. Resolution rules (C13, normative)

| Context | Unqualified name resolves against | Qualified name resolves against |
|---|---|---|
| Analysis term (aware) | host list's selection (legacy: `currentEncoder`) | term's `filterId` → filter handle |
| Analysis textarea | its host's selection | FQ token in text |
| Filter expression | **always the filter's own DataSet — fixed** | any referenced slice |
| Computed column | selected input filter | any slice |
| Computed dataset | selected input filter (existing dropdown) | any slice |
| Results decode (C++, all processes) | — (map membership only; unknown ⟹ raw) | self-routing token |

## 5. User-qualified name grammar (decided)

- `"Dataset"."Filter"."Column"("Type")` — full form, always valid.
- `"Dataset"."Column"` — resolves to that dataset's **default filter**. (Inside a filter editor this form is never *needed* — bare names already mean own dataset there — but it is accepted for other datasets.)
- `"Filter"."Column"("Type")` — accepted **only when the filter name is unique across loaded datasets**; ambiguity ⟹ rejection listing candidate datasets.
- Bare `"Column"` — context fallback per §4.
- Type segment optional everywhere; when present it wins over lookup.
- Render side (scripts/syntax) qualifies only when the reference lies outside the context's default namespace.

## 6. Test matrix (every phase gate; final in D8.7)

```
cmake --build build --target JASPTest JASPTestParsedArgs JASPTestEngine JASPTestDebugData JASPTestCsvPrev JASPQuickTest JASPTestColumnEncoderContext
xvfb-run build/Tests/JASPTest
xvfb-run build/Tests/JASPTestParsedArgs
xvfb-run build/Tests/JASPTestEngine
xvfb-run build/Tests/JASPTestDebugData
xvfb-run build/Tests/JASPTestCsvPrev
QT_QPA_PLATFORM=offscreen xvfb-run build/Tests/JASPQuickTest
xvfb-run build/Tests/JASPTestColumnEncoderContext
```
Plus: `Tests/qmlTests/tst_*.qml` (alphabetical-order trap — restore state!), jaspSyntax/jaspTools/jaspBase testthat suites, jaspTestModule RPC verifier. **`JASPTestDebugData` and `JASPTestEngine` abort intermittently (empty-log coredump) — rerun before blaming the change.**

## 7. Risk register

| Risk | Mitigation |
|---|---|
| Rename re-mints entire token space (`setCurrentNames` counter restart) | inherited hazard, unchanged semantics; single rebuild funnel (D1.4) keeps blocks atomic; registry roundtrip tests per mutation type |
| Phase 3 cliff (no green intermediate mid-cutover) | start only after full-matrix-green; staged bisect-safe commits; RPC verifier + reopen tests as the canaries |
| Global decode table = process-global mutable state | it is purely *derived*: single writer, erase-then-write, resettable via `resetForTests()` — unlike `_boundValues` it is never independently edited. Any new mutation path must go through `rebuildBlock` or die in review |
| Cross-repo drift (desktop ↔ jaspSyntax ↔ bridge) | format pinned by native-roundtrip contract tests from D1.8 onward; submodule pins commit with paired code |
| Engine-process divergence | identical dbLoad code path (D1.4) + explicit engine-registry test (desktop changes ⟹ engine request decodes right) |
| jaspTools/jaspBase R contracts | R never sees provenance (strip at boundary, D3.3) — contracts `datasets[[filter-id]]`, `.meta` stripping unchanged; verified in D8 |
| Dev-branch fixtures written by intermediate formats | regenerate, no compat code (C15) |
| Subagents editing the plan beyond ticks/notes | git diff review; only Phase 9 may delete the file |

## 8. Explicit follow-ups (out of scope, do not sneak in)

- **Extras encoding → desktop pre-encode** at request build (engine-side extras stay load-bearing for run-composed strings: `jaspColumn.cpp:22-29`, `formula.R:126`; retirement needs an audit of jaspAnalyze/jaspStats, not in this checkout). Would fully retire `_otherEncoders`.
- **Tombstoning** deleted-filter decode blocks (currently: stale tokens go raw — C16).
- **Concurrent engine work** (this plan's context-free aware paths are a prerequisite, not the work).
- Multi-dataset **result-page rendering** niceties beyond what D6 verifies.
