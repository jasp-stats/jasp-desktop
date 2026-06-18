# ============================================================================
#  meta.R — Meta / introspection RPC methods
# ============================================================================
#
#  Wraps:
#    modules_list   — list all loaded modules and their analyses
#    analyses_list  — list all analyses currently in the workspace
#    ping           — connectivity check
#    rpc_discover   — schema discovery (full OpenRPC method descriptors)
# ============================================================================

# ---- modules_list -----------------------------------------------------------

#' List all loaded JASP modules and their available analyses
#'
#' This is the discovery entry-point: call this first to learn what modules
#' and analyses are available, then use \code{\link{analysis_context}()} to
#' get detailed help for a specific analysis, and
#' \code{\link{analysis_create}()} to instantiate one.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{module}{Module name, e.g. \code{"jaspTTests"}.}
#'     \item{module_title}{Human-readable module title, e.g. \code{"T-Tests"}.}
#'     \item{analysis}{Analysis function name, e.g.
#'       \code{"TTestIndependentSamples"}.}
#'     \item{analysis_title}{Human-readable analysis title, e.g.
#'       \code{"Independent Samples T-Test"}.}
#'   }
#'
#'   The data frame also carries the raw result as an attribute
#'   \code{"raw"} in case you need the nested list structure.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mods <- modules_list()
#' head(mods)
#' # Filter to all T-Tests:
#' subset(mods, module == "jaspTTests")
#' }
modules_list <- function() {
  result <- jasp_call("modules_list", list())

  # Flatten the nested module→analyses structure into a tidy data frame.
  rows <- list()
  for (mod in result$modules) {
    for (ana in mod$analyses) {
      rows[[length(rows) + 1L]] <- data.frame(
        module         = mod$name,
        module_title   = mod$title,
        analysis       = ana$name,
        analysis_title = ana$title,
        stringsAsFactors = FALSE
      )
    }
  }

  df <- do.call(rbind, rows)
  if (is.null(df)) {
    df <- data.frame(
      module = character(0), module_title = character(0),
      analysis = character(0), analysis_title = character(0),
      stringsAsFactors = FALSE
    )
  }

  attr(df, "raw") <- result
  class(df) <- c("jasp_modules_list", class(df))
  df
}

#' @export
print.jasp_modules_list <- function(x, ...) {
  raw <- attr(x, "raw")
  n_modules <- length(raw$modules)
  n_analyses <- nrow(x)
  cat(sprintf("<JASP modules list: %d module(s), %d analys%s>\n",
              n_modules, n_analyses, if (n_analyses == 1L) "is" else "es"))

  # Print a compact tree
  for (mod in raw$modules) {
    cat(sprintf("  %s (%s)\n", mod$name, mod$title))
    for (ana in mod$analyses) {
      cat(sprintf("    └─ %s: %s\n", ana$name, ana$title))
    }
  }
  invisible(x)
}

# ---- analyses_list ----------------------------------------------------------

#' List all analyses currently in the JASP workspace
#'
#' @return A data frame with columns \code{id}, \code{module}, \code{analysis},
#'   and \code{title}.  The currently selected analysis ID is attached as an
#'   attribute \code{"active_analysis_id"} (\code{-1} means none selected).
#' @export
analyses_list <- function() {
  result <- jasp_call("analyses_list", list())

  analyses <- result$analyses
  if (length(analyses) == 0L) {
    df <- data.frame(
      id = integer(0), module = character(0),
      analysis = character(0), title = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    df <- do.call(rbind, lapply(analyses, function(a) {
      data.frame(
        id       = a$id,
        module   = a$module,
        analysis = a$analysis,
        title    = a$title,
        stringsAsFactors = FALSE
      )
    }))
  }

  attr(df, "active_analysis_id") <- result$activeAnalysisId
  attr(df, "raw") <- result
  class(df) <- c("jasp_analyses_list", class(df))
  df
}

#' @export
print.jasp_analyses_list <- function(x, ...) {
  active <- attr(x, "active_analysis_id")
  cat("<JASP workspace analyses>\n")
  cat("  Active analysis ID:", active, "\n")
  if (nrow(x) == 0L) {
    cat("  (no analyses in workspace)\n")
  } else {
    for (i in seq_len(nrow(x))) {
      marker <- if (x$id[i] == active) " ▶" else "  "
      cat(sprintf("%s [%d] %s::%s  \"%s\"\n",
                  marker, x$id[i], x$module[i], x$analysis[i], x$title[i]))
    }
  }
  invisible(x)
}

# ---- ping -------------------------------------------------------------------

#' Ping the JASP RPC server
#'
#' Simple connectivity check.  Returns \code{TRUE} if the server responds
#' with \code{"pong"}, or \code{FALSE} on any error.
#'
#' @return Logical: \code{TRUE} if the server is reachable and responding.
#' @export
jasp_ping <- function() {
  result <- tryCatch(
    jasp_call("ping", list()),
    error = function(e) NULL
  )
  is.list(result) && identical(result$message, "pong")
}

# ---- rpc_discover -----------------------------------------------------------

#' Discover all available RPC methods and their schemas
#'
#' Returns the full OpenRPC method list as exposed by the server.  Each entry
#' includes the method \code{name}, and may include \code{summary},
#' \code{params}, and \code{result} if an OpenRPC spec was provided at
#' registration time.
#'
#' @return An object of class \code{"jasp_rpc_discover"} — a list with a
#'   \code{methods} element (a list of method descriptors).
#' @export
jasp_discover <- function() {
  result <- jasp_call("rpc_discover", list())

  structure(
    result,
    class = c("jasp_rpc_discover", "jasp_rpc_result", "list")
  )
}

#' @export
print.jasp_rpc_discover <- function(x, ...) {
  methods <- x$methods
  cat(sprintf("<JASP RPC discover: %d method(s)>\n", length(methods)))
  for (m in methods) {
    summary <- m$summary %||% "(no summary)"
    # Truncate long summaries
    if (nchar(summary) > 80L)
      summary <- paste0(substr(summary, 1L, 77L), "...")
    cat(sprintf("  %-30s %s\n", m$name, summary))
  }
  invisible(x)
}
