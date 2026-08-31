# RoboReport Script Guide

How to write RoboReport scripts for JASP analyses.

---

## Overview

A RoboReport script is an R file that generates an annotated statistical
report for a specific JASP analysis type. The script is deterministic —
no LLM, no user interaction. It reads the analysis's options and results,
applies statistical reasoning, and composes a narrative report.

Scripts ship with their module at:
```
<module>/inst/scripts/roboreport/<AnalysisName>.R
```

For example, in the `jaspTTests` module source:
`inst/scripts/roboreport/TTestIndependentSamples.R`

During development you can instead drop a script into the **user override**
location, which takes priority over the module copy — no module reinstall or
JASP rebuild needed:
```
<appData>/roboreport/<module>/<AnalysisName>.R
```
e.g. on Linux:
`~/.local/share/JASP/JASP/roboreport/jaspTTests/TTestIndependentSamples.R`.

There is also a lowest-priority, app-bundled fallback at
`Resources/roboreport/<module>/<AnalysisName>.R`, intended for **demos** (it
lets a demo ship a script even when the module ships none). See the
`jaspRoboReport` README ("Where scripts live") for the per-OS `<appData>` paths
and the full resolution order.

The goal of a script is to **reproduce, deterministically, the kind of
report an AI would generate** — same structure, same statistical reasoning,
same tone — but without calling a model at runtime. The workflow below is
built around that idea.

---

## Workflow: Start from an AI Reference Annotation

Don't design the report from scratch. **Generate (or obtain) an AI-written
annotation for the analysis first, study it, then reproduce its structure
deterministically.** The AI annotation is your specification: it tells you
the section layout, what to interpret, and the tone to aim for.

### 1. Get the reference annotation

Run the analysis in JASP and produce an AI annotation (the "Annotation of …"
output), then export it to HTML, e.g. `analysis_9.html`. The HTML is heavily
styled, so don't read it raw — strip it down to structure first.

### 2. Extract the skeleton (headings + prose)

This one-liner prints the heading outline and the paragraph text, which is
all you need to reverse-engineer the layout:

```bash
python3 -c "
import re
html = open('analysis_9.html').read()
print('=== HEADINGS ===')
for m in re.finditer(r'<(h[1-6])[^>]*>(.*?)</\1>', html, re.DOTALL):
    t = re.sub(r'<[^>]+>', '', m.group(2)).strip()
    if t: print(m.group(1).upper(), '::', t[:100])
print('=== PROSE ===')
for p in re.findall(r'<p[^>]*>(.*?)</p>', html, re.DOTALL):
    t = re.sub(r'\s+', ' ', re.sub(r'<[^>]+>', '', p)).strip()
    if t: print('P:', t[:300]); print()
"
```

### 3. Map each section to an element

For every heading/prose block in the AI output, decide whether it is:

- **A real JASP result** (table, plot, or collection) → reproduce it with
  `el_ref("<name>")` so the live, formatted result appears in the report.
- **Generated prose** (interpretation, rationale, conclusion) → reproduce it
  with `el_md("…")`, computing the numbers from the RDS.

A typical AI annotation for a t-test yields this outline:

```
Abstract                         (prose)
Descriptives                     (table + raincloud plots collection)
Descriptive Statistics           (prose)
Assumption Checks                (normality + variance tables)
Assumption Checks & Test Select. (prose + rationale)
Inferential Tests                (main table + per-variable prose)
Conclusion                       (prose)
```

Match that order. Read the AI prose to calibrate *what* it interprets
(assumption verdicts, effect-size magnitude, direction of effects,
practical significance) and mirror it.

### 4. Trim to taste

The AI output often includes Limitations and Suggested Follow-ups. These are
optional — drop them unless there's genuine low-hanging fruit (e.g. a
"significant p but negligible effect size" caveat is worth keeping inline).

---

## The Script Contract

Every script must define exactly one function:

```r
roboreport_main <- function(analysisId) {
  # ... pipeline ...
}
```

This is called by `jaspRoboReport::run_script()`, which:
1. Sets the RPC endpoint (host/port from the C++ launcher)
2. Sources the script into a fresh environment (parent = package namespace)
3. Calls `roboreport_main(analysisId)`

The `analysisId` is the integer ID of the source analysis the user pressed
the button on.

---

## The Pipeline Pattern

Every script follows the same five-step pipeline:

```
1. Read    — get the source analysis's options
2. Plan    — map options (enable what the report needs) + decide flow
3. Run     — create a sibling analysis with mapped options, run it
4. Extract — read results from the sibling's RDS
5. Compose — build markdown + result references, write into the sibling
```

### Step 1: Read options

```r
state <- rr_get_analyses_state(as.integer(analysisId),
                               include_options = TRUE,
                               options_meta_diff = FALSE)
src <- state$analyses[[1]]
initial_opts <- src$options
```

Or use the convenience wrapper: `initial_opts <- rr_get_options(analysisId)`.

Options are a named R list matching the analysis's form definition. For
example, the independent-samples t-test has:
```r
opts$dependent     # list(value = c("var1", "var2"), types = c("scale", "scale"))
opts$group         # list(value = "groupVar", types = "nominal")
opts$alternative   # "twoSided", "less", or "greater"
opts$student       # TRUE/FALSE  (also $welch, $mannWhitneyU, ...)
```

### Step 2: Plan (map options + decide flow)

One function that returns both the mapped options and the flow decisions:

```r
plan_report <- function(opts) {
  report_opts <- modifyList(opts, list(
    # Enable what the report needs, preserving user's selections
    student = TRUE, welch = TRUE, mannWhitneyU = TRUE,
    effectSize = TRUE, effectSizeCi = TRUE,
    normalityTest = TRUE, equalityOfVariancesTest = TRUE,
    descriptives = TRUE, raincloudPlot = TRUE
  ))

  flow <- list(
    dependent_vars = opts$dependent$value,
    group_var      = opts$group$value,
    hypothesis     = opts$alternative
    # Add flow flags for complex analyses (design type, post-hoc, etc.)
  )

  list(options = report_opts, flow = flow)
}
```

Key principle: `modifyList` preserves everything the user set. You only
override what the report needs.

Prefer the plot option that matches the AI reference (e.g. `raincloudPlot`
over `descriptivesPlot`) — it reads better and matches the annotation style.

### Step 3: Create and run a sibling

```r
sibling_id <- rr_create_and_run(
  module   = src$module,    # from get_analyses_state
  analysis = src$name,      # from get_analyses_state
  options  = plan$options
)
```

This creates a new analysis with the mapped options and runs it. The
sibling becomes the RoboReport output vessel.

### Step 4: Extract results

```r
get_results <- function(analysisId) {
  raw <- rr_results(analysisId)  # reads + strips the RDS

  # The statistic column is "t" with one test, "Statistic" with several.
  stat_col <- if ("Statistic" %in% names(raw$ttest)) "Statistic" else "t"
  main <- rr_select(raw$ttest, c("v", "test", stat_col, "df", "p", "d"))

  # Optional tables — guard with rr_get
  assumptions <- rr_get(raw, "AssumptionChecks")
  normality <- rr_select(rr_get(assumptions, "ttestNormalTable"), c("dep", "W", "p"))

  list(main = main, normality = normality)
}
```

**Key utilities:**
- `rr_results(analysisId)` — reads the RDS, returns a clean list of
  data.frames (tables) and lists (containers, plots)
- `rr_select(df, cols)` — select columns that exist, preserving order.
  Missing columns are silently dropped (for optional features)
- `rr_get(x, name)` — safe `[[` accessor, returns NULL if missing

**Coerce numbers defensively.** The RDS stores `""` for inapplicable cells
(e.g. Mann-Whitney's `df` and `sed`), so wrap reads before arithmetic:
```r
num <- function(x) suppressWarnings(as.numeric(x))
```

**Column naming:** columns come from `addColumnInfo(name=...)` calls in the
module R code. Check the module source for exact names.

### Step 5: Compose the report

Build a list of elements **in reading order** and call `rr_compose_results`.
Interleave prose (`el_md`) with result references (`el_ref`) — don't dump
all the prose first and all the tables last:

```r
build_elements <- function(data, opts, flow) {
  elements <- list()
  elements <- c(elements, list(el_md(build_abstract(data, flow))))   # prose
  elements <- c(elements, list(el_ref("ttestDescriptives")))         # table + plots
  elements <- c(elements, list(el_md(build_desc_prose(data))))       # prose
  elements <- c(elements, list(el_ref("AssumptionChecks")))          # tables
  elements <- c(elements, list(el_md(build_assumption_prose(data)))) # prose
  elements <- c(elements, list(el_ref("ttest")))                     # main table
  elements <- c(elements, list(el_md(build_conclusion(data))))       # prose
  elements
}

rr_compose_results(sibling_id, build_elements(data, initial_opts, plan$flow))
```

**Element types:**
- `el_md(text)` — a markdown/HTML block (headings, bold, lists, …).
- `el_ref(name, sourceAnalysisId=NULL)` — embeds an existing result element
  by its **`.meta` name** (see below). Defaults to the sibling being composed.

---

## Element Names: the `.meta` Rule (important)

The single biggest gotcha. `el_ref()` does **not** take the RDS key — it
takes the exact `name` string from the analysis results `.meta`.

- **Top-level** elements use the bare key: `ttest`, `AssumptionChecks`,
  `ttestDescriptives`.
- **Nested** elements use the full underscore-joined path:
  `ttestDescriptives_table`, `AssumptionChecks_ttestNormalTable`,
  `ttestDescriptives_plotsRainCloud_contNormal`.

The rule: **the `name` field in `.meta` is exactly what you pass to
`el_ref()`.** Discover these names by running the analysis (Step 3) and
inspecting its `.meta` — this also confirms which elements actually exist
for the enabled options. Never guess nested names.

### Collections render their children (and their title)

A `collection` element renders its title as a heading plus all its children.
Two consequences:

- Referencing a **collection of plots** (e.g. the per-variable inferential
  or raincloud collection) lays the plots out **together / side-by-side**.
  Referencing each plot as a *separate* `el_ref` **stacks them vertically**.
  To reproduce the AI example's side-by-side plots, reference the collection.
- A collection already prints its own title, so **don't add a markdown
  heading that duplicates it** (e.g. don't write `##### contNormal` right
  above the `contNormal` plot collection — you'll get a double header).

---

## Reading jaspResults Structure

The RDS from `rr_results()` mirrors the jaspResults object tree. Each module
defines its structure via `jaspResults[["key"]] <- obj` calls in its R source.

To find keys and columns for your analysis:
1. Read the module's R source (e.g. `<module>/R/`)
2. Search for `jaspResults[["..."]]` (top-level keys)
3. Search for `container[["..."]]` (nested keys)
4. Search for `addColumnInfo(name = "...")` (table columns)

Example (frequentist jaspTTests):
```
jaspResults[["ttest"]]                    → table          (el_ref "ttest")
jaspResults[["AssumptionChecks"]]         → container      (el_ref "AssumptionChecks")
  container[["ttestNormalTable"]]         → table          (el_ref "AssumptionChecks_ttestNormalTable")
  container[["equalityVariance"]]         → table          (el_ref "AssumptionChecks_equalityVariance")
jaspResults[["ttestDescriptives"]]        → container      (el_ref "ttestDescriptives")
  container[["table"]]                    → table          (el_ref "ttestDescriptives_table")
  container[["plotsRainCloud"]]           → collection     (el_ref "ttestDescriptives_plotsRainCloud")
    subcontainer[[variable]]              → plot           (el_ref "ttestDescriptives_plotsRainCloud_<var>")
```

The left column is how you navigate the RDS in `get_results()` (via
`rr_get`); the `el_ref` strings on the right are what you pass to compose.

---

## Interpretation Logic

The report's value is in the interpretation, not restating numbers. Good
scripts:

1. **Check assumptions first** — normality, variance equality, etc.
2. **Select the appropriate test** from the assumptions.
3. **Interpret effect sizes** — magnitude *and* practical importance.
4. **Flag contradictions** — e.g. significant p but negligible effect size.

Example test-selection logic:
```r
if (!normality_ok) {
  recommended <- "Mann-Whitney"; reason <- "normality violated"
} else if (!variances_equal) {
  recommended <- "Welch";        reason <- "variances unequal"
} else {
  recommended <- "Student";      reason <- "assumptions met"
}
```

**Effect-size metric depends on the test:** Cohen's d for Student/Welch,
rank-biserial correlation for Mann-Whitney. Label it correctly in prose.

When the AI reference reports a "primary" vs "supplementary" analysis, mirror
that: lead with the assumption-justified test (citing the driving statistic,
e.g. `W = 0.97, p = .017`), then list the others briefly "for completeness".

---

## Formatting Helpers

The package provides APA-style formatters (see `jaspRoboReport/R/format.R`):

```r
fmt_p(0.0034)                # "p = .003"
fmt_p(0.0001)                # "p < .001"
fmt_stat(2.34, 98, "t")      # "t(98) = 2.34"   (df is rounded to an integer)
fmt_ci(0.12, 0.45)           # "95% CI [0.12, 0.45]"
fmt_effect_size(0.45)        # "Cohen's d = 0.45 (small)"
fmt_mean_sd(5.23, 1.45)      # "M = 5.23, SD = 1.45"
fmt_bf(4.5)                  # "BF10 = 4.50 (moderate evidence for H1)"
bf_evidence_category(0.25)   # list(direction="H0", bf=4, bf_label="BF01 = 4.00", category="moderate")
```

All coerce to numeric defensively. Note `fmt_stat` rounds `df` with `%.0f`
(so a Welch df of 94.25 prints as `t(94)`).

**Bayes factors:** `fmt_bf` / `bf_evidence_category` use Jeffreys'
categories (anecdotal / moderate / strong / very strong / extreme) and
automatically flip direction: a BF below 1 is reported as the reciprocal
`BF01` with evidence *for H0*.

**Inside `glue::glue`, format numbers with `sprintf`, not Python specs:**
```r
glue::glue("r = {sprintf('%.3f', width)}")   # correct
glue::glue("r = {width:.3f}")                # WRONG — Python syntax, errors in R
```

---

## Report Structure Template

The canonical layout (matches the AI annotation), as an element sequence:

```
Abstract                          el_md   — variables, groups, N, primary test + why
Descriptives                      el_ref  — collection: descriptives table + raincloud plots
Descriptive Statistics            el_md   — per-variable means/SDs
Assumption Checks                 el_ref  — collection: normality + variance tables
Assumption Checks & Test Selection el_md  — verdicts + per-variable rationale
Inferential Tests                 el_md (heading) + el_ref (main table)
  per-variable                    el_md   — primary analysis + supplementary results
Conclusion                        el_md   — per-variable significance, magnitude, direction
```

Keep prose and references interleaved in this reading order.

---

## Validating a Script

The MCP `jasp_analysis_composeResults` tool can be unavailable, and direct
HTTP to the RPC port is sandbox-blocked — so validate in layers that don't
depend on a live compose:

1. **Parse check:** (point at wherever your working copy lives — the module
   `inst/scripts/roboreport/`, or your `<appData>/roboreport/<module>/`
   override)
   ```bash
   Rscript -e 'parse(file="inst/scripts/roboreport/<Analysis>.R"); cat("OK\n")'
   ```

2. **Discover element names + confirm they exist:** run the analysis with the
   plan's options via MCP (`jasp_analysis_run`) and read the `.meta` of the
   result. Use those exact `name` strings in `el_ref()`.

3. **Simulate the prose builders standalone.** Source the script, stub the
   formatters to match `format.R`, reconstruct a mock `data`/`flow` from a
   real run's values, call each prose builder, and print the markdown:
   ```r
   fmt_p    <- function(p) { p <- suppressWarnings(as.numeric(p)); if (p < .001) "p < .001" else sprintf("p = %.3f", p) }
   fmt_stat <- function(stat, df, stat_name="t", digits=2) {
     df <- suppressWarnings(as.numeric(df))
     if (is.na(df)) sprintf("%s = %.*f", stat_name, digits, stat)
     else sprintf("%s(%.0f) = %.*f", stat_name, df, digits, stat)   # note: df rounded
   }
   source("inst/scripts/roboreport/<Analysis>.R")   # or your <appData>/roboreport/<module>/<Analysis>.R override
   # ...build mock data/flow from a real run, then cat(build_abstract(...)), etc.
   ```
   This catches `glue`/NA/logic bugs without needing the compose tool.

4. **End-to-end:** the RoboReport **button** is the definitive test — the
   script calls `rr_compose_results` over HTTP at runtime, independent of the
   MCP tool. **Make sure you look at the freshly generated annotation**: each
   button press creates a *new* annotation, so a stale export (an older
   `analysis_N.html`) will show an older script's output and mislead you.

---

## Full Examples

- `jaspTTests/inst/scripts/roboreport/TTestIndependentSamples.R` — frequentist:
  assumption-driven test selection, raincloud plots, primary/supplementary
  results, effect-size + practical-significance interpretation.
- `jaspTTests/inst/scripts/roboreport/TTestBayesianIndependentSamples.R` —
  Bayesian: Bayes-factor evidence categories, prior/posterior + robustness
  plots (via per-variable collections), effect size from the posterior,
  robustness statement, overall conclusion.

App-bundled demo scripts (in `Resources/roboreport/`, priority-3 location —
these modules ship no module-side script yet, so the fallback is what runs):

- `Resources/roboreport/jaspAnova/Anova.R` — between-subjects ANOVA:
  annotated F-table (Vovk–Sellke MPR, eta² / partial eta² / omega²),
  descriptives + descriptive plot, Levene's homogeneity test + residual Q-Q
  plot, Tukey post-hoc per model term, conclusion with limitations and
  follow-ups.
- `Resources/roboreport/jaspRegression/RegressionLinear.R` and
  `RegressionLinearBayesian.R` — multiple regression, frequentist + Bayesian.
- `Resources/roboreport/jaspRegression/Correlation.R` and
  `CorrelationBayesian.R` — correlation, frequentist + Bayesian
  (`CorrelationBayesian.R` is the reference for the zero-length guard
  pattern below).

---

## Tips

- **Study the AI annotation first** — it's your spec. Strip it to headings +
  prose, then reproduce that layout deterministically.
- **`el_ref` takes the `.meta` `name`** — full underscore path for nested
  elements. Discover names by running the analysis; never guess.
- **Reference plot collections, not individual plots**, for side-by-side
  layout; and don't duplicate a collection's own title with a heading.
- **Interleave** prose and references in reading order.
- **Guard optional tables** with `rr_get()` — descriptives/assumptions may be
  disabled in the source options.
- **Use `modifyList`** for option mapping — preserves user selections.
- **Coerce with `suppressWarnings(as.numeric())`** — the RDS stores `""` for
  inapplicable cells (e.g. Mann-Whitney df/sed).
- **Guard zero-length cells before `is.na()`/`sprintf()`** — an empty table
  can yield a `length-0` value, and `if (!is.na(x))` then throws
  `argument is of length zero` (this crashed CorrelationBayesian's abstract).
  Reduce first: `scalar <- function(x) if (length(x) >= 1) x[1] else NA`,
  and check `!is.null(data$main)` / guard the variable list before use.
- **`glue` numbers via `sprintf`**, not `{x:.3f}`.
- **The `flow` list** carries interpretation context (hypothesis direction,
  variable names, design type) from `plan_report` to `build_elements`.
- **Label the effect-size metric per test** (Cohen's d vs rank-biserial).
- **Validate in layers** (parse → `.meta` discovery → standalone prose sim →
  button), and always check the *fresh* annotation, not a stale export.
