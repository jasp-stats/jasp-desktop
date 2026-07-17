#'
#' Utilities for RoboReport script authors.
#'
#' These functions help script authors write getters and compose reports
#' without depending on jaspBase internals. The per-analysis getter logic
#' (which columns, which tables, what keys) lives IN THE SCRIPT, not in
#' this package -- because it is analysis-specific and changes with each
#' JASP module release.

# ============================================================================
# Column selection
# ============================================================================

#' Select columns from a data.frame, keeping only those that exist.
#'
#' Unlike `df[, cols]` (which errors on missing columns), this silently
#' drops requested columns that are absent. This lets getters list the
#' full ideal column set without breaking when the analysis omits columns
#' for disabled options.
#'
#' Columns that ARE present are returned in the requested order, so
#' callers always know the layout.
#'
#' @param df A data.frame (possibly NULL).
#' @param cols Character vector of desired column names.
#' @return A data.frame with the intersection of `cols` and `names(df)`,
#'   in the order of `cols`. NULL if `df` is NULL.
#' @export
rr_select <- function(df, cols) {
  if (is.null(df)) return(NULL)
  present <- cols[cols %in% names(df)]
  df[, present, drop = FALSE]
}

# ============================================================================
# Safe accessors for optional results
# ============================================================================

#' Get an element from a list, returning NULL if missing.
#'
#' A convenience wrapper around `[[` that does not error on absent
#' elements or NULL input. Useful for navigating optional jaspResults
#' containers.
#'
#' @param x A list (possibly NULL).
#' @param name Element name.
#' @return The element, or NULL if not found.
#' @export
rr_get <- function(x, name) {
  if (is.null(x) || !is.list(x)) return(NULL)
  if (!name %in% names(x)) return(NULL)
  x[[name]]
}

# ============================================================================
# Pipeline helpers
# ============================================================================

#' Read the current options of an analysis.
#'
#' Convenience wrapper around `get_analyses_state` that returns just the
#' options list for a single analysis. Used by `roboreport_main` to read
#' the source analysis's initial options before mapping them.
#'
#' @param analysisId Integer analysis identifier.
#' @return Named list of option name/value pairs.
#' @export
rr_get_options <- function(analysisId) {
  state <- rr_get_analyses_state(
    analysisIds       = as.integer(analysisId),
    include_options   = TRUE,
    options_meta_diff = FALSE
  )
  if (length(state$analyses) == 0)
    stop("Analysis not found: ", analysisId, call. = FALSE)
  state$analyses[[1]]$options
}

#' Create a new analysis and run it with the given options.
#'
#' Convenience wrapper for the RoboReport sibling pattern: scripts create
#' a new analysis (typically with enhanced options), run it, and read its
#' results. Returns the new analysis ID.
#'
#' @param module JASP module name (e.g. "jaspTTests").
#' @param analysis Analysis name (e.g. "TTestIndependent").
#' @param options Named list of option name/value pairs.
#' @param wait If TRUE (default), block until finished or timeout.
#' @param timeoutMs Max milliseconds to wait (default 60000).
#' @return Integer analysis ID of the newly created and run analysis.
#' @export
rr_create_and_run <- function(module, analysis, options,
                               wait = TRUE, timeoutMs = 60000L) {
  sib <- rr_analysis_create(module, analysis)
  rr_analysis_run(sib$analysisId, options, wait = wait, timeoutMs = timeoutMs)
  sib$analysisId
}
