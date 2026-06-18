# test-client.R — Integration tests for the jasprpc package.
#
# These tests require a running JASP instance at JASP_RPC_TEST_HOST:JASP_RPC_TEST_PORT
# (default 127.0.0.1:48164) and the debug.csv dataset.
#
# The test file is designed to be source()-able standalone (without testthat
# or jasprpc being installed as a package) via the helper shims below.  When
# run under testthat, those shims are no-ops and the real testthat functions
# are used instead.

# ---- Shims for standalone runs (no-op when testthat is loaded) --------------

if (!"testthat" %in% loadedNamespaces()) {
  test_that <- function(name, expr) {
    cat("\n---", name, "---\n")
    tryCatch({
      eval(substitute(expr), parent.frame())
      cat("  PASS\n")
    }, error = function(e) {
      cat("  FAIL:", conditionMessage(e), "\n")
    })
  }
}

# ── Connection ──────────────────────────────────────────────────────────────

jasp_test_host <- Sys.getenv("JASP_RPC_TEST_HOST", "127.0.0.1")
jasp_test_port <- as.integer(Sys.getenv("JASP_RPC_TEST_PORT", "48164"))
debug_csv      <- Sys.getenv("JASP_TEST_DATA",
  normalizePath("Resources/Data Sets/debug.csv", mustWork = FALSE))

jasp_connect(host = jasp_test_host, port = jasp_test_port)

# ── Meta / introspection ────────────────────────────────────────────────────

test_that("ping works", {
  expect_true(jasp_ping())
})

test_that("rpc_discover returns methods", {
  d <- jasp_discover()
  expect_s3_class(d, "jasp_rpc_discover")
  expect_true(length(d$methods) >= 1L)
})

test_that("modules_list returns a data frame", {
  mods <- modules_list()
  expect_s3_class(mods, "jasp_modules_list")
  expect_true(is.data.frame(mods))
  expect_true(nrow(mods) >= 1L)
  expect_setequal(names(mods),
                  c("module", "module_title", "analysis", "analysis_title"))
})

test_that("analyses_list returns a data frame", {
  al <- analyses_list()
  expect_s3_class(al, "jasp_analyses_list")
  expect_true(is.data.frame(al))
  expect_true(is.numeric(attr(al, "active_analysis_id")))
})

test_that("data_info works (even if no dataset is loaded)", {
  info <- data_info()
  expect_s3_class(info, "jasp_data_info")
  expect_true(is.logical(info$loaded))
})

# ── Error handling ──────────────────────────────────────────────────────────

test_that("jasp_rpc_error is raised for unknown methods", {
  expect_error(
    jasp_call("nonexistent_method", list()),
    class = "jasp_rpc_error"
  )
})

test_that("jasp_api_error is raised for invalid analysis ID", {
  expect_error(
    analysis_results(99999, wait = FALSE),
    class = "jasp_api_error"
  )
})

test_that("defaults are sensible", {
  d <- jasp_defaults()
  expect_equal(d$host, jasp_test_host)
  expect_equal(d$port, jasp_test_port)
  expect_true(d$connected)
})

# ── Data loading (requires debug.csv) ───────────────────────────────────────

test_that("data_load succeeds", {
  skip_if_not(file.exists(debug_csv), "debug.csv not found")

  ds <- data_load(debug_csv, wait = TRUE, timeout_ms = 10000)
  expect_s3_class(ds, "jasp_data_result")
  expect_equal(ds$status, "success")
  expect_equal(ds$rowCount, 100L)
  expect_equal(ds$columnCount, 31L)
  expect_true(length(ds$columns) == 31L)

  cols <- ds$columns
  names_vec <- vapply(cols, `[[`, "name", FUN.VALUE = "")
  expect_true("contNormal" %in% names_vec)
  expect_true("facGender" %in% names_vec)
})

test_that("data_info reflects the loaded dataset", {
  skip_if_not(file.exists(debug_csv), "debug.csv not found")

  info <- data_info()
  expect_true(info$loaded)
  expect_equal(info$rowCount, 100L)
  expect_equal(info$columnCount, 31L)
})

# ── Analysis lifecycle ──────────────────────────────────────────────────────

test_that("analysis_create returns expected fields", {
  a <- analysis_create("jaspTTests", "TTestIndependentSamples")
  expect_s3_class(a, "jasp_analysis_result")
  expect_equal(a$status, "success")
  expect_type(a$analysisId, "integer")
  expect_equal(a$module,   "jaspTTests")
  expect_equal(a$analysis, "TTestIndependentSamples")
  expect_true(is.list(a$options))
  expect_true(is.list(a$optionMeta))
  expect_true("dependent" %in% names(a$options))
  expect_true("group"     %in% names(a$options))
})

test_that("analysis_get_options works", {
  a <- analysis_create("jaspTTests", "TTestIndependentSamples")
  opts <- analysis_get_options(a$analysisId)
  expect_s3_class(opts, "jasp_analysis_result")
  expect_equal(opts$analysisId, a$analysisId)
})

test_that("analysis_context returns help text", {
  ctx <- analysis_context("jaspTTests", "TTestIndependentSamples")
  expect_s3_class(ctx, "jasp_analysis_context")
  expect_true(is.character(ctx$help))
  expect_true(nchar(ctx$help) > 0L)
})

# ── Running analyses against debug.csv ──────────────────────────────────────

test_that("analysis_run produces a real t-test table", {
  skip_if_not(file.exists(debug_csv), "debug.csv not found")
  data_load(debug_csv)

  a <- analysis_create("jaspTTests", "TTestIndependentSamples")
  r <- analysis_run(a$analysisId, list(
    dependent    = list("contNormal", "contGamma"),
    group        = "facGender",
    descriptives = TRUE,
    student      = TRUE
  ), wait = TRUE, timeout_ms = 15000)

  expect_equal(r$status, "complete")
  expect_true(!is.null(r$results$ttest))
  expect_equal(r$results$ttest$status, "complete")

  tt_data <- r$results$ttest$data
  expect_true(length(tt_data) >= 2L)

  first <- tt_data[[1]]
  expect_true("Statistic" %in% names(first))
  expect_true("df" %in% names(first))
  expect_true("p" %in% names(first))
  expect_true("v" %in% names(first))
})

test_that("analysis_run with wait=FALSE and polling works", {
  skip_if_not(file.exists(debug_csv), "debug.csv not found")
  data_load(debug_csv)

  a <- analysis_create("jaspTTests", "TTestIndependentSamples")

  ar <- analysis_run(a$analysisId, list(
    dependent = list("contcor1"),
    group     = "facGender"
  ), wait = FALSE)

  expect_equal(ar$status, "running")

  final <- analysis_results(a$analysisId, wait = TRUE, timeout_ms = 15000)
  expect_equal(final$status, "complete")
  expect_true(!is.null(final$results$ttest))
  expect_equal(final$results$ttest$status, "complete")
})

test_that("analysis_compose_results succeeds", {
  skip_if_not(file.exists(debug_csv), "debug.csv not found")
  data_load(debug_csv)

  a <- analysis_create("jaspTTests", "TTestIndependentSamples")
  analysis_run(a$analysisId, list(
    dependent = list("contNormal"),
    group     = "facGender"
  ), wait = TRUE, timeout_ms = 15000)

  cr <- analysis_compose_results(a$analysisId, list(
    list(md_text = "## Test Annotation\n\nCompose works."),
    list(name   = "ttest")
  ))

  expect_equal(cr$status, "success")
  expect_equal(cr$analysisId, a$analysisId)
})

test_that("empty options list is handled correctly", {
  # list() -> {} in JSON; this exercises the empty-options fix
  a <- analysis_create("jaspTTests", "TTestIndependentSamples")
  r <- analysis_run(a$analysisId, list(), wait = FALSE)
  expect_equal(r$status, "running")
})

test_that("options with explicit types:[] is valid", {
  skip_if_not(file.exists(debug_csv), "debug.csv not found")
  data_load(debug_csv)

  a <- analysis_create("jaspTTests", "TTestIndependentSamples")
  # types:[] is valid — JASP infers column types
  r <- analysis_run(a$analysisId, list(
    dependent = list(types = list(), value = list("contNormal")),
    group     = list(types = list(), value = "facGender")
  ), wait = TRUE, timeout_ms = 15000)
  expect_equal(r$status, "complete")
})
