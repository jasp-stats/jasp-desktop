#'
#' Results adapter - the jaspBase containment layer.
#'
#' This is the ONLY file that knows about jaspBase's RDS format.
#' Everything above it (getters, scripts) sees clean data.frames and plot
#' objects.
#'
#' Responsibilities:
#'   1. Get the RDS path from the RPC response.
#'   2. readRDS() the file.
#'   3. Recursively strip jaspBase wrapper attributes (the 1000x bloat
#'      source - jaspObjectEnvironment XPtrs drag in the Rcpp module).
#'   4. Strip internal columns (.isNewGroup) and wrapper classes.
#'   5. Fix placeholder values: jaspBase emits "." and "" for uncomputed
#'      string cells instead of NA.
#'   6. Return a clean named list of data.frames (+ plot objects).
#'
#' Known jaspBase warts handled here:
#'   - jaspObjectEnvironment attr: XPtr bloat (729K -> 715 bytes).
#'   - Elements named by localized TITLE, not stable KEY. Getters use
#'     positional indexing to survive this; rr_results() returns whatever
#'     names the RDS has.
#'   - Placeholder strings (".", "") for uncomputed values.
#'   - .isNewGroup internal column leaks into the data.frame.

# ============================================================================
# Public API
# ============================================================================

#' Read and normalize the RDS results for a finished analysis.
#'
#' Calls `analysis_results` to obtain the `jaspResultsRds` path, reads the
#' RDS, strips all jaspBase wrapper junk, and returns a clean named list
#' of data.frames (tables) and lists with `$plotObject` (plots).
#'
#' The returned list elements are named by jaspBase's localized TITLES,
#' not the stable programmatic keys. Per-analysis getters (e.g.
#' `rr_ttest_independent()`) use positional indexing internally so scripts
#' never have to deal with this.
#'
#' @param analysisId Integer analysis identifier (must be finished).
#' @return A named list of data.frames and/or plot lists.
#' @export
rr_results <- function(analysisId) {

  resp <- rr_analysis_results(analysisId)

  rds_path <- resp$jaspResultsRds
  if (is.null(rds_path))
    stop("No jaspResultsRds in RPC response for analysis ", analysisId,
         " - is the analysis finished?", call. = FALSE)

  if (!file.exists(rds_path))
    stop("jaspResults.rds not found at: ", rds_path, call. = FALSE)

  x <- readRDS(rds_path)
  .rr_strip(x)
}

# ============================================================================
# Internal: recursive stripping
# ============================================================================

#' Recursively strip jaspBase wrapper attributes and normalize.
#'
#' @param obj Anything from a jaspResults RDS: data.frame (table), list
#'   (container or plot), or scalar.
#' @return Clean R object - bare data.frame, plain list, or scalar.
#' @keywords internal
.rr_strip <- function(obj) {
  if (.rr_is_jasp_table(obj)) {
    .rr_clean_table(obj)
  } else if (.rr_is_jasp_plot(obj)) {
    .rr_clean_plot(obj)
  } else if (is.list(obj) && !is.data.frame(obj)) {
    .rr_clean_container(obj)
  } else {
    # Scalar or unexpected - return as-is
    obj
  }
}

# ============================================================================
# Internal: type detection
# ============================================================================

#' Does this object look like a jaspTable wrapper?
#'
#' jaspTable::toRObject() returns a data.frame with class
#' c("jaspTableWrapper", "jaspWrapper", "data.frame").
.rr_is_jasp_table <- function(obj) {
  is.data.frame(obj) && "jaspTableWrapper" %in% class(obj)
}

#' Does this object look like a jaspPlot wrapper?
#'
#' jaspPlot::toRObject() returns a list with class
#' c("jaspPlotWrapper", "jaspWrapper") containing $plotObject.
.rr_is_jasp_plot <- function(obj) {
  is.list(obj) && !is.data.frame(obj) &&
    "jaspPlotWrapper" %in% class(obj) &&
    "plotObject" %in% names(obj)
}

# ============================================================================
# Internal: cleaners for each jaspBase type
# ============================================================================

#' Clean a jaspTable wrapper into a bare data.frame.
#'
#' Strips: jaspObjectEnvironment, footnotes attr, wrapper class.
#' Drops: .isNewGroup column (internal jaspBase bookkeeping).
#' Fixes: placeholder strings (".", "", "NaN") -> NA.
.rr_clean_table <- function(df) {

  # Strip wrapper attributes
  attr(df, "jaspObjectEnvironment") <- NULL
  attr(df, "footnotes")             <- NULL
  attr(df, "class")                 <- "data.frame"

  # Drop internal columns
  if (".isNewGroup" %in% names(df))
    df$.isNewGroup <- NULL

  # Fix placeholder strings -> NA
  for (nm in names(df)) {
    col <- df[[nm]]
    if (is.character(col)) {
      col[col %in% c(".", "", "NaN", "null")] <- NA
      df[[nm]] <- col
    }
  }

  df
}

#' Clean a jaspPlot wrapper into a plain list.
#'
#' Strips wrapper attributes. The plot object (ggplot, lattice, etc.) is
#' preserved if present. When the RDS was saved with JASP_RDS_STRIP=1
#' (the JASP default), plotObject is NULL to avoid ~57MB of ggplot
#' environment bloat - in that case RoboReport reads the PNG path from
#' the JSON results instead.
.rr_clean_plot <- function(plot_list) {

  result <- list(plotObject = plot_list$plotObject)

  # Preserve the title if present
  title <- attr(plot_list, "title")
  if (!is.null(title) && title != "")
    attr(result, "title") <- title

  result
}

#' Clean a jaspContainer wrapper into a plain named list.
#'
#' Recursively strips each child. The container itself has wrapper
#' attributes that must be removed before recursing, otherwise the
#' jaspObjectEnvironment XPtr bloat persists.
.rr_clean_container <- function(container) {

  # Strip wrapper attributes FIRST - this kills the XPtr bloat at the root
  attr(container, "jaspObjectEnvironment") <- NULL
  attr(container, "class")                 <- "list"

  # Recursively clean each child
  result <- lapply(container, .rr_strip)

  # Preserve the title if present (useful for debugging)
  title <- attr(container, "title")
  if (!is.null(title) && title != "")
    attr(result, "title") <- title

  result
}
