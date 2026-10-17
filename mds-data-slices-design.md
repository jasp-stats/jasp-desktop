# MDS data-slices design note (D7.0 — for review, do not implement from yet)

Phase 7 card D7.0 of MDS-PLAN.md. Status: **draft for plan-owner review** — Phase 7 forbids coding
before this note is reviewed; the card is ticked only after that review.

Design question: the main-data UIs (Filter window, Computed-column window, Computed-dataset panel)
currently attach *one* data context each. How do Filters, Computed Columns and Computed Datasets
operate **on slices of multiple loaded datasets, with fully-qualified references**?

All anchors verified on `dataSetAwareVarForm` @ 2fbf63917.

---

## 1. Target UX

### 1.1 What "attaching data" becomes

Nothing new is invented: each of the three editors *already* anchors a slice, and C13 (§4 of the
plan) codifies exactly that anchor as the **fallback namespace** for bare names:

| Editor | Fallback (bare names) | Where it lives today |
|---|---|---|
| Filter window | the Filter being edited — its own DataSet, **fixed** (C13, not selectable) | `FilterWindow.qml:93-131` tabs switch between the *own dataset's* filters; the edited `Filter` is `filterModel.filter` |
| Computed column | the column's **compute filter** (own dataset's filters) | `ComputeColumnWindow.qml:273-288` `computeFilterDropDown` → `Column::computeFilter` (column.cpp:354-362) |
| Computed dataset | the selected **input filter** (any dataset except itself) | `ComputeDataSetPanel.qml:109-125` inputDropDown → `DataSet::defaultInputFilterId` (dataset.cpp:1183-1202) — the existing precedent for cross-dataset selection |

So D7 does not add an "attach data" concept; it **codifies the existing anchor as the fallback
encoder** (per-editor host selection, the analysis-`HostFilterSelection` analogue) and adds the
*outward* surface: references to **other** loaded slices.

Per-editor "host selection" registration: the editors are not analysis forms, so the D2.3
selection-host registry does not apply. What registers instead is the *resolution fact* — already
fully determined at run time by where the request carries its filter id (see §3); the editor-side
equivalent is trivial: `FilterWindow` resolves through `filterModel.filter->columnEncoder()`
(filter.cpp:542-553), the compute windows through the selected input filter's handle. No new
registry object is needed; a per-editor `VariableInfoProvider` *view* for the picker is (§1.3).

### 1.2 How outward references are entered and what they look like

The qualified surface form is exactly plan §5 — **reuse, don't reinvent**:

- `"Dataset"."Filter"."Column"("Type")` — full form, always valid.
- `"Dataset"."Column"` — that dataset's default filter.
- `"Filter"."Column"("Type")` — only when the filter name is unique across loaded datasets;
  ambiguity ⟹ rejection listing candidate datasets (both at insert time and at run time).
- Bare `"Column"` — context fallback per the table above.
- Render side qualifies only what lies outside the context's default namespace (scripts stay
  readable); own-dataset code stays byte-identical to today (backward compatible).

Entry paths per UI:

- **R editors** (`FilterWindow.qml:417-453` filterEdit, `ComputeColumnWindow.qml:135-171`
  computeColumnEdit, `ComputeDataSetPanel.qml:144-167` codeEdit): typing with an **autocomplete
  popup** (trigger on `"` and on word start), fed by a workspace-level slice+column catalog
  (§2.1). Insertion emits the **minimal valid form** per §5 (bare when own dataset, `"DS"."Col"`
  for another dataset's default filter, `"F"."Col"` when unambiguous) — matching the render-side
  rule so stored code stays readable.
- **Easy filter constructor** (`ScriptConstructor` mode Filter, `FilterWindow.qml:173-220`) and
  **computed-column constructor** (`ComputeColumnWindow.qml:190-201`): the column palette gains a
  second source — "other datasets" — via the same catalog. A drag from another dataset drops a
  `ScriptNodeColumn` with `_dataSetName` set (scriptnode.h:203-216; the field already serializes,
  scriptnode.cpp:199-200/545-546). **Scope cut (recommended):** in v1 the easy constructors emit
  dataset-level refs only (other dataset ⟹ its **default filter**); filter-qualified outward refs
  are entered via the R editors. This keeps the constructor drag model simple while the full
  grammar stays available where users type R.
- **Computed-dataset panel**: R-code editor only (it has no constructor); same autocomplete.
- **DataTableView / FilterWindow tabs** (`FilterWindow.qml:93-131`): unchanged — tabs remain the
  own-dataset filter switcher; they are not an outward-reference surface.

### 1.3 The slice catalog (picker/autocomplete data)

New small read-only surface on `Workspace`, built entirely from existing pieces:
per loaded dataset — title, per filter — (name, id), per filter — column names + types.
Sources: `Workspace::dataSetFilterDropDownList()` (workspace.cpp:516-521) for the slices and
`Filter::provideInfo(VariableNames)`/`rowFilteredVarInfo()` (filter.cpp:532-606) for columns and
types. Delivered to QML as a `QVariantList`/model next to `inputFilterDropDownList` (shape
follows the existing dropdown lists). Resolution of a typed surface form reuses the §5 rules and
the `Workspace::resolveDataSetSelectionValue`/`datasetSelectionSurface` precedent
(workspace.cpp:592-658) — implemented C++-side (see §6, the D8.1 re-cut).

---

## 2. Data model changes

### 2.1 Where slices' columns come from for the editors

No slice materialization is needed for *editing*: each `Filter` is already a
`VariableInfoProvider` scoped to its own dataset with rows filtered and names encoded by its own
view (filter.cpp:532-606) — the same object the analysis hosts consume (D2.4). The catalog in
§1.3 aggregates those; the editors never hold column data themselves.

### 2.2 Storage stays raw; qualified only for outward refs

- Filter `rFilter`/`constructorR`: stored raw (Filter::setRFilter, filter.cpp:346) — unqualified
  own-dataset names unchanged; outward refs stored as the §5 surface form. `column.cpp:547`
  contract (`Column::setRCode` stores raw) unchanged. Nothing re-encodes stored code; the
  encode-to-tokens happens at run (§3).
- Constructor JSON: `ScriptNodeColumn::_dataSetName` already round-trips
  (scriptnode.cpp:199-200, 545-546). D7.4 makes `toR` emit the qualified surface when it is set
  (scriptnode.cpp:554-569). No filterId in constructor JSON v1 — title/name-based, resolved at
  run; a stale name degrades to a loud run error (same doctrine as
  `datasetSelectionSurface`'s "unknown id renders empty and fails loudly", workspace.cpp:612).
- **D5 implications: none.** D5's Storage format concerns *analysis options* JSON
  (`Terms::getStorageOptions`, per-term `filterId`); filter/computed-column code lives in
  dedicated DB columns (`_rCode`, `_constructorJson`, `_computeFilter`,
  `dbUpdateComputedColumnStuff` column.cpp:452) that Phase 5 never touched. The
  `defaultInputFilterId` is likewise a `dataSet` table column (dataset.cpp:1124). No migration,
  no format bump.

### 2.3 Dependency tracking across datasets (the real model change)

`Column::findDependencies` (column.cpp:2773-2777) scans own-dataset names only
(`DataSet::findUsedColumnNames`, dataset.cpp:1389-1409 — which scans through `shownEncoder()` +
`tableDecode`, namespace-blind). Two extensions:

1. **Scan the surface form**: the dependency scanner additionally recognizes qualified refs and
   records `(dataSetId, filterId, column)` triples (resolved via the §5 helpers) beside the
   plain name set.
2. **Cross-dataset invalidation**: today invalidation is intra-dataset (`DataSet::runFilters`
   dataset.cpp:1368-1377, `Column::checkForDependentColumnsToBeSent` column.cpp:2788-2805).
   `Workspace` gains forwarding: when dataset *B*'s columns/filters/rows change, datasets holding
   dependents on *B*'s slices invalidate the affected computed columns / mark affected filters
   `setInvalidated(true)`. `Workspace::dataSetRemoved` already clears dangling
   `defaultInputFilterId`s (dataset.cpp:241-245, workspace.cpp:275) — the same hook pattern
   extends to referenced-slice bookkeeping: a removed dataset leaves dependents invalidated with
   a loud error (see OQ4).

---

## 3. Engine paths

### 3.1 Explicit slice context (D7.3)

Today the compute paths lean on the process-global current encoder:

- `Engine::runComputeColumn` (engine.cpp:575-622): output-name encode via
  `ColumnEncoder::columnEncoder()->encode(...)` at **:594**; the input slice is selected by
  showing the column's compute filter inside `rbridge_evalRComputedColumn`
  (rbridge.cpp:975-1005, `computedColumnFilter` :982-984).
- `rbridge_encodeColumnNamesInScript` (rbridge.cpp:853-856) — the encode pass over *user* R code
  — always uses the process-global `ColumnEncoder::columnEncoder()`.
- Filter runs (`Engine::runFilterByName` :326-368, `runDefaultFilter` :370-414) show their filter
  explicitly but encode through the same global (rbridge_applyFilter :897-950).

D7.3 threads an **explicit handle** through these: the input filter's `Filter::columnEncoder()`
view (compute column: the `computeFilter`'s view; filter: the filter's own view; computed dataset:
the `defaultInputFilterId` filter's view — engine.cpp:624-685 already resolves that filter
explicitly at :644-654). The output-name encode at :594 must use the **same view as the data
frame R reads**, i.e. the input filter's view (they are same-dataset filters for computed
columns, so today's global happens to coincide; making it explicit removes the coincidence).
`rbridge_encodeColumnNamesInScript` gains a handle parameter; the process-global remains the
C7 legacy fallback for callers that are not yet aware. The `-1` input-filter refusal for computed
datasets (engine.cpp:635-642) is kept verbatim.

### 3.2 Cross-dataset references inside a filter/compute expression — the hard part

**Current reality:** a filter/compute run attaches exactly one `data` frame in R
(rbridge_setupRCodeEnvReadData rbridge.cpp:879-888 reads `.readFilterDatasetToEnd()` /
`.readCompColDatasetToEnd()` — the *shown* slice only). There is **no `datasets` list** in these
contexts (that exists only in analysis runs, common.R:110-141). A qualified reference today would
simply be an undefined name in R.

**Semantics proposal (the honest answer):** cross-slice references give R *data availability*,
not a join.

- **Mechanism:** at request build, the desktop scans the rCode/constructor for qualified surface
  forms (the D8.1-helpers scanner), resolves each to `(dataSetId, filterId)` and sends a
  transient `referencedFilterIds` array with the request (same doctrine as the `.filterIds`
  sidecar: desktop owns provenance, engine consumes and it never reaches user R). The engine
  materializes each referenced slice through the **existing** machinery —
  `provideAndUpdateDataSet` + `showFilter` + slice read with the *foreign* filter's encoder
  (rbridge_readDataSetRequested's D1.6 slice handling, rbridge.cpp:515-589, reuse unchanged) —
  and attaches each foreign slice as its own frame on the R search path (named per slice key,
  tokens inside are globally unique by construction: `JASPColumn_D<ds>_F<f>_…`, C1). The own
  slice stays `data` — the existing contract is untouched.
- **Row semantics:** the filter's row iteration is its own dataset's rows (the returned boolvec
  must be own-rowCount long — already enforced at rbridge.cpp:936-942; computed columns are
  length-checked by `.setColumnDataAs…` at engine.cpp:601). R's own vector semantics then decide
  everything else, and the existing boundary checks surface misuse loudly:
  - `mean("B"."x")` — whole-column aggregate over B's slice: correct regardless of row counts.
  - `"B"."x" > 3` element-wise inside a filter: defined only when row counts match (positional
    alignment); with mismatched counts R's recycling/length error hits the existing boolvec
    length check ⟹ clean filter error, not silent wrongness.
  - There are **no join keys** in the JASP data model and no row-id concept exposed in these
    editors; proposing per-row *join* semantics would be inventing a merge-on-what question with
    no answer. Key-based merging is what the computed-dataset's free-form R code is for
    (`merge()`), and it already works once the slices are attached.

**Options considered, for the record:**

| Option | Verdict |
|---|---|
| O1: positionally cbind foreign columns into `data`, equal row counts required | rejected — breaks whole-column aggregates on unequal counts, which are the *common* case |
| O2: cbind with NA-padding | rejected — `mean()` silently includes padding NAs; wrongness by construction |
| **O3 (recommended): foreign slices as attached frames; R vector semantics + existing result-length checks decide** | no join pretense, aggregates correct, element-wise misuse fails loudly at a check that already exists |

### 3.3 The D3.3 slice-queue extension

`prepareMultiDataSetRun` (databridge.cpp:193-314) builds the queue from the `.filterIds` sidecar
for *analysis* runs. D7 reuses the queue/`MultiDataSetSlice` plumbing but feeds it differently:
the filter/compute request paths (enginesync's `RFilterStore` / `RComputeColumnStore` /
`RComputeDataSetStore`, enginesync.cpp:620-637) gain the desktop-side scan (§3.2) and stamp the
request; the engine pops the referenced slices before evaluating. No change to
`rbridge_readDataSetRequested`'s slice handling itself — only a new *producer* of queues.

---

## 4. Storage / syntax summary

- Own-dataset code: stored **unqualified**, byte-identical to today — zero compat debt
  (existing saved files keep working; engine still encodes bare names through the fallback/C13
  anchor).
- Outward refs: stored as the §5 **surface form** (titles/names — readable in scripts, per the
  render-side rule). Encoded to unique tokens only at run, C++-side; R never parses the grammar
  (C14 holds: the scanner operates on *surface* forms, not tokens, and lives in C++).
- Rename/re-mint hazards: `Filter`'s rename path already rewrites rFilter via
  `replaceColumnNamesInRScript` (filter.cpp:669-676) for own names; outward refs stored as
  surface forms are *not* token-renames — a renamed dataset/filter makes the stored surface form
  stale, degrading to a loud run error (same doctrine as OQ4 / workspace.cpp:612). This is
  accepted for v1 (see OQ5).
- D5 format: **no implications** (§2.2).

## 5. Reuse inventory

**Slots in unchanged (Phases 1–6):**
- Per-filter views + global decode table (C3/C4): foreign tokens are globally unique and decode
  at every site, including in error messages (`decodeAll` at rbridge.cpp:924/969/1000).
- `rbridge_readDataSetRequested` slice handling + slice encoders (D1.6) — the materialization
  core of §3.2.
- The D3.3 slice queue plumbing (`MultiDataSetSlice`, `setMultiDataSetQueue`) — new producer
  only.
- `Workspace::filterById` materialization (workspace.cpp:405-426) — surface-form resolution
  needs named filters of reopened files, already handled.
- `Filter` as `VariableInfoProvider` / `rowFilteredVarInfo` (§2.1); `dataSetFilterDropDownList`.
- `datasetSelectionSurface`/`resolveDataSetSelectionValue` as the *pattern* for the §5 helpers.
- HostFilterSelection/FilterSelect UI pattern where a selectable anchor is needed (it already
  exists in all three editors as the dropdowns of §1.1).
- Zoo/verifier test patterns (jaspTestModule RPC verifier, PROVENANCE_AUDIT discipline) for the
  D7 test matrix; `ColumnEncoder::ScopedDecodeBlock` is *not* needed here.

**New (the honest shortlist):**
1. §5 surface-form parse/render/resolve helpers in C++ (the D8.1 re-cut, §6).
2. The workspace slice catalog model for autocomplete/picker (§1.3).
3. Desktop-side reference scan + `referencedFilterIds` request stamp; engine queue producer for
   filter/compute runs (§3.2–3.3).
4. Attach-foreign-slices env setup in rbridge (§3.2).
5. Cross-dataset dependency scan + Workspace invalidation forwarding (§2.3).
6. Autocomplete/insertion UI in the three editors; `ScriptNodeColumn::_dataSetName` wiring in
   the constructor palette + `toR` emission (D7.4).

## 6. Phasing — proposed re-cut

The critical dependency is the **§5 surface-form helper core** (parse/render/resolve), which the
plan currently places in D8.1 *after* the D7 UI cards that need it. Proposal:

- **D7.0a (new)** = the helper core: `QualifiedColumnRef` parse/render/resolve in CommonData
  (beside workspace.cpp:592-658), with unit tests. Small, standalone, no UI. This is the
  "D8.1a" half of D8.1; D8.1 shrinks to its wiring half (syntax-bridge parsing, textarea
  context replay, script re-parse) and keeps its number.
- **D7.3 next** (engine explicit slice context + referenced-slice materialization + attach
  env): independently testable C++-side with hand-written qualified refs, before any UI exists.
- **D7.1 / D7.2** (filter window / compute windows UI + catalog + autocomplete) — can proceed
  in either order after D7.0a; both depend on D7.3 only for end-to-end runs.
- **D7.4 last** (constructor `_dataSetName` wiring + `toR` qualified emission) — depends on
  D7.0a's renderer and the palette source (D7.1/D7.2 UI work).
- Dependency/invalidation work (§2.3) is proposed as an explicit new card **D7.5** rather than
  being smuggled into D7.3 — it touches `Workspace` signals and needs its own tests.

Nothing else in D7.1–D7.4 scopes shifts; the original card texts remain accurate descriptions of
their UI work.

## 7. Open questions for the plan owner

1. **Cross-dataset row semantics** (§3.2): accept O3 (foreign slices as attached frames; no
   join, R vector semantics, existing length checks as the guardrail)? *Recommended: yes* — O1/O2
   are silently wrong in common cases; a join needs keys JASP does not have.
2. **D8.1 re-cut** (§6): split the helper core out as D7.0a so Phase 7 is self-contained?
   *Recommended: yes*; alternative is implementing D7 strictly after D8.1, which inverts the
   phase order for no gain.
3. **Easy-constructor scope cut** (§1.2): dataset-level (default-filter) outward refs only in
   the drag&drop constructors for v1, full grammar in the R editors? *Recommended: yes.*
4. **Referenced slice disappears** (dataset closed/filter deleted while a filter or computed
   column references it): block unload vs allow-with-loud-invalidation? *Recommended: allow* —
   mark dependents invalidated with an error naming the missing slice (no tombstoning, C16
   spirit; consistent with dataset.cpp:241-245's dangling-input-filter handling).
5. **Surface-form staleness on rename** (§4): store surface forms (stale on rename ⟹ loud run
   error) vs store stable ids in stored code? *Recommended: surface forms for v1* — readable
   scripts were the §5 decision; id-storing inside R code strings would need escaping rules and
   a decode path that violates C14's spirit. Revisit only if rename-staleness proves painful in
   practice.
6. **Also thread the explicit handle through `rbridge_evalRCodeWhiteListed`** (the R-code debug
   eval, rbridge.cpp:952-972)? *Recommended: yes* — same one-parameter change, keeps the
   process-global fallback from being resurrected by a side door.
