# ============================================================================
#  analysis.R — High-level wrappers for JASP analysis RPC methods
# ============================================================================
#
#  Wraps:
#    analysis_create          — create and start an analysis
#    analysis_run             — set options + run (blocking or fire-and-forget)
#    analysis_get_options     — retrieve current options for an analysis
#    analysis_results         — poll for results of a running analysis
#    analysis_context         — get help text for an analysis
#    analysis_compose_results — reorder / slice / annotate results
#
#  Each returns an S3 object with class "jasp_analysis_result" (or a
#  domain-specific subclass) so that print() and str() show a useful
#  summary instead of dumping raw JSON.
# ============================================================================

# ---- analysis_create --------------------------------------------------------

#' Create and start a new analysis
#'
#' Creates a new analysis of the given type in the JASP workspace and returns
#' its default options and metadata.  The analysis is *started* (queued for
#' execution) immediately — use \code{\link{analysis_results}()} to retrieve
#' its output after it completes.
#'
#' @param module   JASP module name, e.g. \code{"jaspTTests"}.
#' @param analysis Analysis name within the module, e.g.
#'   \code{"TTestIndependentSamples"}.
#'
#' @return An object of class \code{"jasp_analysis_result"} containing:
#'   \describe{
#'     \item{status}{Always \code{"success"}.}
#'     \item{analysisId}{Integer ID you will use with other analysis_* calls.}
#'     \item{module}{Echoed module name.}
#'     \item{analysis}{Echoed analysis name.}
#'     \item{options}{Default option name/value pairs (a named list).}
#'     \item{optionMeta}{Metadata describing each option's kind, valid
#'       choices, allowed column types, etc.}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' a <- analysis_create("jaspTTests", "TTestIndependentSamples")
#' print(a$options)
#' str(a$optionMeta)
#' }
analysis_create <- function(module, analysis) {
  stopifnot(is.character(module),   length(module)   == 1L)
  stopifnot(is.character(analysis), length(analysis) == 1L)

  result <- jasp_call("analysis_create", list(
    module   = module,
    analysis = analysis
  ))
  as_analysis_result(result)
}

# ---- analysis_run -----------------------------------------------------------

#' Set options and run an analysis
#'
#' This is the main "do everything" call.  It sets options on an existing
#' analysis and waits for results (by default up to \code{timeout_ms}
#' milliseconds).  If the timeout fires, the call returns with
#' \code{status = "running"} — you must then poll with
#' \code{\link{analysis_results}()} until the analysis reaches a terminal
#' state.
#'
#' @param analysis_id Integer analysis ID from \code{\link{analysis_create}()}.
#' @param options     Named list of option name/value pairs to set.
#' @param wait        If \code{TRUE} (default), block until the analysis
#'   finishes or \code{timeout_ms} elapses.  If \code{FALSE}, return
#'   immediately with \code{status = "running"}.
#' @param timeout_ms  Maximum milliseconds to wait when \code{wait = TRUE}.
#'
#' @return An object of class \code{"jasp_analysis_result"}.
#'   \describe{
#'     \item{status}{\code{"success"} (finished), \code{"running"} (timeout),
#'       or \code{"error"} (validation failed).}
#'     \item{analysisId}{The analysis ID.}
#'     \item{module}{Module name.}
#'     \item{analysis}{Analysis name.}
#'     \item{options}{Current options (present on success / running).}
#'     \item{optionMeta}{Option metadata (present on success / running).}
#'     \item{results}{Full results JSON (only when status is \code{"success"}).}
#'     \item{message}{Error message (only when status is \code{"error"}).}
#'   }
#'
#' @export
analysis_run <- function(analysis_id,
                         options    = list(),
                         wait       = .jasp_state$default_wait,
                         timeout_ms = .jasp_state$default_timeout_ms) {
  stopifnot(is.numeric(analysis_id), length(analysis_id) == 1L)
  stopifnot(is.list(options))

  # Ensure empty options serializes to {}, not []
  if (length(options) == 0L)
    options <- structure(list(), names = character(0))

  result <- jasp_call("analysis_run", list(
    analysisId = as.integer(analysis_id),
    options    = options,
    wait       = isTRUE(wait),
    timeoutMs  = as.integer(timeout_ms)
  ))

  out <- as_analysis_result(result)

  # If we got a timeout (status = "running"), warn the user.
  if (identical(out$status, "running")) {
    message("analysis_run timed out.  Use analysis_results(", out$analysisId,
            ") to poll for completion.")
  }

  out
}

# ---- analysis_get_options ---------------------------------------------------

#' Retrieve the current options of an analysis
#'
#' @param analysis_id Integer analysis ID.
#' @param include_descriptions Logical.  If \code{TRUE} (default), the
#'   \code{optionMeta} includes description and instruction text.
#'
#' @return An object of class \code{"jasp_analysis_result"} with \code{options}
#'   and \code{optionMeta} fields.
#' @export
analysis_get_options <- function(analysis_id,
                                 include_descriptions = TRUE) {
  stopifnot(is.numeric(analysis_id), length(analysis_id) == 1L)

  result <- jasp_call("analysis_getOptions", list(
    analysisId          = as.integer(analysis_id),
    includeDescriptions = isTRUE(include_descriptions)
  ))
  as_analysis_result(result)
}

# ---- analysis_results -------------------------------------------------------

#' Poll for results of a running analysis
#'
#' Only call this after \code{\link{analysis_run}()} has returned
#' \code{status = "running"} (i.e. the initial timeout fired).  Do **not**
#' call this if \code{analysis_run} already returned \code{"success"} or
#' \code{"error"} — the results are already final.
#'
#' @param analysis_id Integer analysis ID.
#' @param wait        If \code{TRUE} (default), block until the analysis
#'   finishes or \code{timeout_ms} elapses.
#' @param timeout_ms  Maximum milliseconds to wait when \code{wait = TRUE}.
#'
#' @return An object of class \code{"jasp_analysis_result"}.
#' @export
analysis_results <- function(analysis_id,
                             wait       = .jasp_state$default_wait,
                             timeout_ms = .jasp_state$default_timeout_ms) {
  stopifnot(is.numeric(analysis_id), length(analysis_id) == 1L)

  result <- jasp_call("analysis_results", list(
    analysisId = as.integer(analysis_id),
    wait       = isTRUE(wait),
    timeoutMs  = as.integer(timeout_ms)
  ))
  as_analysis_result(result)
}

# ---- analysis_context -------------------------------------------------------

#' Retrieve help text and metadata for an analysis type
#'
#' Intended for AI clients or interactive exploration that need to understand
#' what an analysis does and what options are available.  Returns the help
#' file (if one exists) as a Markdown string.
#'
#' @param module   JASP module name.
#' @param analysis Analysis name within the module.
#'
#' @return An object of class \code{"jasp_analysis_context"} containing
#'   \code{status}, \code{module}, \code{analysis}, and \code{help} (Markdown
#'   string, may be empty).
#' @export
analysis_context <- function(module, analysis) {
  stopifnot(is.character(module),   length(module)   == 1L)
  stopifnot(is.character(analysis), length(analysis) == 1L)

  result <- jasp_call("analysis_context", list(
    module   = module,
    analysis = analysis
  ))

  structure(
    list(
      status   = result$status,
      module   = result$module,
      analysis = result$analysis,
      help     = result$help
    ),
    class = c("jasp_analysis_context", "jasp_rpc_result", "list")
  )
}

# ---- analysis_compose_results -----------------------------------------------

#' Reorder, slice, and annotate analysis results
#'
#' Allows you to restructure the output of an existing analysis: reorder
#' tables/plots, insert Markdown annotations, and set the final status.
#'
#' @param analysis_id Integer analysis ID.
#' @param elements    A list of element specifications.  Each element is a
#'   named list with **exactly one** of:
#'   \itemize{
#'     \item \code{name = "elementName"} — re-insert an existing result
#'           element by its \code{.meta} name.
#'     \item \code{md_text = "Markdown/HTML"} — insert an annotation block
#'           (use \code{#} for headings).
#'   }
#' @param status      Optional analysis status to apply.  Valid values:
#'   \code{"complete"} (default) or \code{"fatalError"}.
#'
#' @return An object of class \code{"jasp_analysis_result"} with status
#'   \code{"success"} or \code{"error"}.
#' @export
#'
#' @examples
#' \dontrun{
#' analysis_compose_results(42, list(
#'   list(name    = "ttest"),
#'   list(md_text = "## Key Finding\n\nNo significant difference was observed."),
#'   list(name    = "ttestDescriptives")
#' ))
#' }
analysis_compose_results <- function(analysis_id,
                                     elements,
                                     status = "complete") {
  stopifnot(is.numeric(analysis_id), length(analysis_id) == 1L)
  stopifnot(is.list(elements))
  stopifnot(status %in% c("complete", "fatalError"))

  result <- jasp_call("analysis_composeResults", list(
    analysisId = as.integer(analysis_id),
    elements   = elements,
    status     = status
  ))
  as_analysis_result(result)
}

# ---- S3 helpers -------------------------------------------------------------

#' Coerce a raw RPC result to an analysis result object
#'
#' @param x A list returned by \code{\link{jasp_call}()}.
#' @return An object of class \code{"jasp_analysis_result"}.
#' @export
as_analysis_result <- function(x) {
  structure(x, class = c("jasp_analysis_result", "jasp_rpc_result", "list"))
}

#' @export
print.jasp_analysis_result <- function(x, ...) {
  cat("<JASP analysis result>\n")
  cat("  Status:      ", x$status %||% "?", "\n")
  if (!is.null(x$analysisId))
    cat("  Analysis ID: ", x$analysisId, "\n")
  if (!is.null(x$module))
    cat("  Module:      ", x$module, "\n")
  if (!is.null(x$analysis))
    cat("  Analysis:    ", x$analysis, "\n")
  if (!is.null(x$message))
    cat("  Message:     ", x$message, "\n")

  # Summarise results if present
  if (!is.null(x$results) && length(x$results) > 0L) {
    n_results <- length(x$results)
    cat("  Results:     ", n_results, " top-level key(s)\n")
  }

  # Summarise options
  if (!is.null(x$options)) {
    n_opts <- length(x$options)
    if (n_opts > 0L) {
      cat("  Options:     ", n_opts, " option(s)\n")
      # Show a few interesting ones
      shown <- 0L
      for (nm in names(x$options)) {
        if (shown >= 8L) { cat("                ...\n"); break }
        val <- x$options[[nm]]
        if (is.atomic(val) && length(val) <= 3L) {
          cat(sprintf("    $%s: %s\n", nm,
                      paste(as.character(val), collapse = ", ")))
        } else {
          cat(sprintf("    $%s: <%s[%d]>\n", nm, typeof(val), length(val)))
        }
        shown <- shown + 1L
      }
    }
  }
  invisible(x)
}

#' @export
print.jasp_analysis_context <- function(x, ...) {
  cat("<JASP analysis context>\n")
  cat("  Module:   ", x$module, "\n")
  cat("  Analysis: ", x$analysis, "\n")
  if (nzchar(x$help %||% "")) {
    nlines <- length(strsplit(x$help, "\n")[[1]])
    cat("  Help:     ", nlines, " line(s) of Markdown\n")
  } else {
    cat("  Help:     (none)\n")
  }
  invisible(x)
}
