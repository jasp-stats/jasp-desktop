#'
#' RPC transport and tool wrappers for JASP RoboReport.
#'
#' The low-level layer: one HTTP call function (`rr_call`) and thin 1:1
#' wrappers for every method in the JASP OpenRPC spec
#' (`Resources/JASP_RPC.json`).
#'
#' Endpoint configuration is stored in a package-level environment
#' (`.rr_config`) set by `run_script()` when the C++ launcher fires it.
#' Scripts never touch host/port directly.

# ============================================================================
# Package-level config environment
# ============================================================================

.rr_config <- new.env(parent = emptyenv())
.rr_config$host      <- "127.0.0.1"
.rr_config$port      <- 48164L
.rr_config$timeoutMs <- 60000L

#' Set the RPC endpoint. Called by `run_script()` on entry.
#'
#' @param host Hostname or IP (default `"127.0.0.1"`).
#' @param port Port number (default `48164L`).
#' @param timeoutMs Request timeout in milliseconds (default `60000L`).
#' @keywords internal
.rr_set_endpoint <- function(host = "127.0.0.1",
                             port = 48164L,
                             timeoutMs = 60000L) {
  .rr_config$host      <- host
  .rr_config$port      <- as.integer(port)
  .rr_config$timeoutMs <- as.integer(timeoutMs)
}

.rr_endpoint <- function() {
  sprintf("http://%s:%d/rpc", .rr_config$host, .rr_config$port)
}

# ============================================================================
# Core transport
# ============================================================================

#' Call a JASP JSON-RPC method over HTTP.
#'
#' Sends a JSON-RPC 2.0 POST to the JASP RPC server and returns the parsed
#' `result` object on success. Throws an error on HTTP failure or if the
#' response contains a JSON-RPC `error` object.
#'
#' The JASP dispatcher enforces a single in-flight call at a time
#' (`m_inFlight` guard). This blocking call naturally satisfies that
#' constraint - never issue concurrent `rr_call`s.
#'
#' @param method The JSON-RPC method name (e.g. `"analysis_run"`).
#' @param params A named list of parameters. Defaults to an empty list.
#' @return The parsed `result` field as an R list (or scalar).
#' @export
rr_call <- function(method, params = list()) {
  body <- list(
    jsonrpc = "2.0",
    id      = 1L,
    method  = method,
    params  = params
  )

  req <- httr2::request(.rr_endpoint()) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(.rr_config$timeoutMs / 1000) |>
    httr2::req_error(is_error = ~ FALSE)  # we handle errors from the body

  resp <- httr2::req_perform(req)
  # The JASP RPC server sends Content-Type: text/plain (QHttpServer default)
  # even though the body is valid JSON. Skip the content-type check.
  json <- httr2::resp_body_json(resp, check_type = FALSE)

  if (!is.null(json$error)) {
    code <- json$error$code %||% -1L
    msg  <- json$error$message %||% "unknown error"
    stop(sprintf("[RPC %d] %s", code, msg), call. = FALSE)
  }

  json$result
}

# ============================================================================
# Internal helpers
# ============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ============================================================================
# Tool wrappers - 1:1 with the OpenRPC spec
# ============================================================================
# Each wrapper maps an R function to the corresponding JSON-RPC method.
# Parameter names match the spec for discoverability. Optional params with
# defaults are only sent when the caller overrides them (or always sent if
# the server's default matters - we err on the side of explicit for
# booleans the dispatcher applies defaults to).

# ---- Analysis lifecycle ----------------------------------------------------

#' Create a new analysis by module and analysis name.
#'
#' @param module JASP module name (e.g. `"jaspTTests"`).
#' @param analysis Analysis name within the module (e.g. `"TTestIndependent"`).
#' @return List with `status`, `analysisId`, `module`, `analysis`, `options`,
#'   `optionMeta`.
#' @export
rr_analysis_create <- function(module, analysis) {
  rr_call("analysis_create", list(
    module   = module,
    analysis = analysis
  ))
}

#' Set options on an analysis and run it, blocking up to `timeoutMs`.
#'
#' Combines setOptions + run + (optionally) wait-for-results in one call.
#' If `wait` is `TRUE` and the analysis finishes within the timeout, the
#' response includes `results`. If the timeout fires, `status` is
#' `"running"` - poll with `rr_analysis_results()`.
#'
#' @param analysisId Integer analysis identifier.
#' @param options Named list of option name/value pairs to set.
#' @param wait If `TRUE` (default), block until finished or timeout.
#' @param timeoutMs Max milliseconds to wait (default 30000).
#' @param optionMetaDelta If `TRUE` (default), return only changed option
#'   metadata. Set `FALSE` for the full `optionMeta`.
#' @return List with `status` (`"success"`/`"running"`/`"error"`),
#'   `analysisId`, `results` (if finished), `optionMetaDelta` or
#'   `optionMeta`.
#' @export
rr_analysis_run <- function(analysisId, options,
                            wait = TRUE,
                            timeoutMs = 30000L,
                            optionMetaDelta = TRUE) {
  rr_call("analysis_run", list(
    analysisId       = analysisId,
    options          = options,
    wait             = wait,
    timeoutMs        = timeoutMs,
    optionMetaDelta  = optionMetaDelta
  ))
}

#' Poll for analysis results. Only call after `rr_analysis_run()` returned
#' `status = "running"`.
#'
#' @param analysisId Integer analysis identifier.
#' @param wait If `TRUE` (default), block until finished or timeout.
#' @param timeoutMs Max milliseconds to wait (default 30000).
#' @return List with `status`, `results` (if finished).
#' @export
rr_analysis_results <- function(analysisId,
                                wait = TRUE,
                                timeoutMs = 30000L) {
  rr_call("analysis_results", list(
    analysisId = analysisId,
    wait       = wait,
    timeoutMs  = timeoutMs
  ))
}

#' Retrieve state for one or more analyses: options and optionally results.
#'
#' Use this to observe the workspace before mutating it, or to get a
#' complete snapshot including the RDS results path.
#'
#' @param analysisIds Integer vector of analysis IDs.
#' @param include_options If `TRUE` (default), include options + optionMeta.
#' @param options_meta_diff If `TRUE` (default), return only changed
#'   optionMeta keys.
#' @param include_results If `TRUE`, include full results JSON (default
#'   `FALSE` - results can be large).
#' @param include_descriptions If `TRUE`, include human-readable option
#'   descriptions (default `FALSE`).
#' @return List with `status`, `analyses` (list of per-analysis state),
#'   `missing` (vector of unknown IDs).
#' @export
rr_get_analyses_state <- function(analysisIds, include_options = TRUE,
                                options_meta_diff = TRUE,
                                include_results = FALSE,
                                include_descriptions = FALSE) {
  rr_call("get_analyses_state", list(
    analysisIds          = I(as.integer(analysisIds)),
    include_options      = include_options,
    options_meta_diff    = options_meta_diff,
    include_results      = include_results,
    include_descriptions = include_descriptions
  ))
}

#' Retrieve help/context markdown for a module::analysis.
#'
#' @param module JASP module name.
#' @param analysis Analysis name.
#' @return List with `status`, `module`, `analysis`, `help` (markdown
#'   string, may be empty).
#' @export
rr_analysis_context <- function(module, analysis) {
  rr_call("analysis_context", list(
    module   = module,
    analysis = analysis
  ))
}

# ---- Discovery -------------------------------------------------------------

#' List all installed modules and their analyses.
#'
#' @return List with `modules` - an array of `{name, title, analyses}`.
#' @export
rr_modules_list <- function() {
  rr_call("modules_list")
}

#' List all analyses currently in the workspace.
#'
#' @return List with `analyses` (array of `{id, module, analysis, title}`)
#'   and `activeAnalysisId`.
#' @export
rr_analyses_list <- function() {
  rr_call("analyses_list")
}

# ---- Data ------------------------------------------------------------------

#' Get information about the currently loaded dataset.
#'
#' @return List with `status`, `path`, `rowCount`, `columnCount`,
#'   `columns` (array of `{name, type, distinctCount}`).
#' @export
rr_data_info <- function() {
  rr_call("data_info")
}

# ---- Result composition ----------------------------------------------------
# These build the `elements[]` array that the dispatcher's
# `composeResultJSON()` renders into JASP output. Each element is either:
#   { name, sourceAnalysisId? }  - reference an existing result element
#   { md_text }                  - insert a markdown block
# Elements are rendered in order, so prose and results interleave.

#' Compose results into an existing analysis.
#'
#' @param analysisId Target analysis to write into.
#' @param elements List of element objects (see `el_ref()`, `el_md()`).
#' @param status Optional: `"fatalError"` to mark the analysis as failed.
#' @return List with `status`, `analysisId`, `module`, `analysis`.
#' @export
rr_compose_results <- function(analysisId, elements, status = NULL) {
  params <- list(analysisId = analysisId, elements = elements)
  if (!is.null(status)) params$status <- status
  rr_call("analysis_composeResults", params)
}

#' Duplicate an analysis, mark it as an annotation, and compose results
#' into the duplicate.
#'
#' This is the primary output path for RoboReport scripts: the source
#' analysis stays pristine, and the annotation appears alongside it.
#'
#' @param analysisId The source analysis to annotate (will be duplicated).
#' @param elements List of element objects (see `el_ref()`, `el_md()`).
#' @param status Optional: `"fatalError"` to mark the annotation as failed.
#' @return List with `status`, `analysisId` (the new annotation's ID),
#'   `module`, `analysis`.
#' @export
rr_create_annotation <- function(analysisId, elements, status = NULL) {
  params <- list(analysisId = analysisId, elements = elements)
  if (!is.null(status)) params$status <- status
  rr_call("analysis_createAnnotation", params)
}

#' Create or update a top-level Report analysis.
#'
#' Reports can aggregate elements from multiple analyses (each element
#' specifies `sourceAnalysisId`). Use for cross-analysis summaries.
#'
#' @param elements List of element objects (see `el_ref()`, `el_md()`).
#' @param title Optional title for the report.
#' @param reportId Optional: update an existing report by ID.
#' @param status Optional: `"fatalError"` to mark the report as failed.
#' @return List with `status`, `reportId`, `title`.
#' @export
rr_write_report <- function(elements, title = NULL, reportId = NULL,
                            status = NULL) {
  params <- list(elements = elements)
  if (!is.null(title))    params$title    <- title
  if (!is.null(reportId)) params$reportId <- reportId
  if (!is.null(status))   params$status   <- status
  rr_call("write_report", params)
}

# ---- Element constructors (build `elements[]` entries) ---------------------

#' Create a result-reference element.
#'
#' References an existing named result element (table, plot, container)
#' from `sourceAnalysisId` (defaults to the analysis being annotated).
#'
#' @param name The result element name (e.g. `"ttest"`, `"descriptives"`).
#' @param sourceAnalysisId Analysis ID that owns the element. If `NULL`,
#'   the server uses the default source (the analysis being annotated or
#'   reported on).
#' @return A list suitable for inclusion in an `elements` array.
#' @export
el_ref <- function(name, sourceAnalysisId = NULL) {
  el <- list(name = name)
  if (!is.null(sourceAnalysisId))
    el$sourceAnalysisId <- sourceAnalysisId
  el
}

#' Create a markdown-text element.
#'
#' Inserts a block of markdown (rendered as HTML by JASP) into the output.
#' Use for prose: abstracts, interpretations, conclusions.
#'
#' @param text Markdown string. Supports headings, bold, italic, lists,
#'   inline code, etc.
#' @return A list suitable for inclusion in an `elements` array.
#' @export
el_md <- function(text) {
  list(md_text = text)
}
