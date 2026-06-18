# ============================================================================
#  data.R — High-level wrappers for JASP data-loading RPC methods
# ============================================================================
#
#  Wraps:
#    data_load        — load a data file (blocking or async with jobId)
#    data_load_status — poll / block on an async data-load job
#    data_info        — metadata about the currently loaded dataset
#
#  The data layer is the only part of the API with an explicit async pattern:
#  calling data_load(wait = FALSE) returns a jobId that you later pass to
#  data_load_status().
# ============================================================================

# ---- data_load --------------------------------------------------------------

#' Load a data file into the JASP workspace
#'
#' Supports CSV, SPSS, JASP, Excel, and other formats JASP understands.
#' In blocking mode (\code{wait = TRUE}, the default) this returns dataset
#' metadata directly.  In async mode (\code{wait = FALSE}) it returns a
#' \code{jobId} immediately — use \code{\link{data_load_status}()} to poll
#' for completion.
#'
#' @param path      Absolute path to the data file.
#' @param wait      If \code{TRUE} (default), block until the load finishes
#'   or \code{timeout_ms} elapses.  If \code{FALSE}, return immediately
#'   with a \code{jobId}.
#' @param timeout_ms Maximum milliseconds to wait when \code{wait = TRUE}.
#' @param delimiter CSV delimiter character.  Default \code{","}.  Setting
#'   this skips the interactive preview popup in JASP.
#'
#' @return An object of class \code{"jasp_data_result"}.
#'   \describe{
#'     \item{status}{\code{"success"}, \code{"accepted"} (async),
#'       \code{"running"} (timeout), or \code{"error"}.}
#'     \item{jobId}{Integer job ID (always present; use for polling when
#'       \code{status} is \code{"accepted"} or \code{"running"}).}
#'     \item{path}{The file path that was loaded (present on success).}
#'     \item{rowCount}{Number of data rows (present on success).}
#'     \item{columnCount}{Number of columns (present on success).}
#'     \item{columns}{Data frame of column metadata with \code{name},
#'       \code{type}, and optionally \code{distinctCount} (present on
#'       success).}
#'     \item{message}{Error message (present when status is \code{"error"}).}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Blocking load
#' ds <- data_load("/home/user/data.csv")
#' print(ds$columns)
#'
#' # Async load for large files
#' job <- data_load("/home/user/bigfile.sav", wait = FALSE)
#' # ... do other work ...
#' ds  <- data_load_status(job$jobId)
#' }
data_load <- function(path,
                      wait       = .jasp_state$default_wait,
                      timeout_ms = .jasp_state$default_timeout_ms,
                      delimiter  = ",") {
  stopifnot(is.character(path), length(path) == 1L)
  stopifnot(is.character(delimiter), nchar(delimiter) == 1L)

  result <- jasp_call("data_load", list(
    path      = path,
    wait      = isTRUE(wait),
    timeoutMs = as.integer(timeout_ms),
    delimiter = delimiter
  ))

  out <- as_data_result(result)

  if (identical(out$status, "accepted")) {
    message("data_load queued as job ", out$jobId,
            ".  Use data_load_status(", out$jobId, ") to poll.")
  } else if (identical(out$status, "running")) {
    message("data_load timed out.  Use data_load_status(", out$jobId,
            ") to poll for completion.")
  }

  out
}

# ---- data_load_status -------------------------------------------------------

#' Poll or block for the completion of an async data-load job
#'
#' @param job_id     Integer job ID returned by \code{\link{data_load}()}.
#' @param wait       If \code{TRUE} (default), block until the load finishes
#'   or \code{timeout_ms} elapses.
#' @param timeout_ms Maximum milliseconds to wait when \code{wait = TRUE}.
#'
#' @return An object of class \code{"jasp_data_result"}.
#'   \describe{
#'     \item{jobId}{The job ID.}
#'     \item{status}{\code{"running"}, \code{"complete"}, or \code{"error"}.}
#'     \item{message}{Error message (present when status is \code{"error"}).}
#'     \item{path, rowCount, columnCount, columns}{Dataset metadata (present
#'       when status is \code{"complete"}).}
#'   }
#' @export
data_load_status <- function(job_id,
                             wait       = .jasp_state$default_wait,
                             timeout_ms = .jasp_state$default_timeout_ms) {
  stopifnot(is.numeric(job_id), length(job_id) == 1L)

  result <- jasp_call("data_load_status", list(
    jobId     = as.integer(job_id),
    wait      = isTRUE(wait),
    timeoutMs = as.integer(timeout_ms)
  ))
  as_data_result(result)
}

# ---- data_info --------------------------------------------------------------

#' Retrieve metadata about the currently loaded dataset
#'
#' @return An object of class \code{"jasp_data_info"}.
#'   \describe{
#'     \item{status}{Always \code{"success"}.}
#'     \item{loaded}{\code{TRUE} if a dataset is currently in the workspace.}
#'     \item{path}{Path to the loaded file (if any).}
#'     \item{rowCount}{Number of data rows.}
#'     \item{columnCount}{Number of columns.}
#'     \item{columns}{Data frame of column metadata (\code{name}, \code{type},
#'       \code{distinctCount}).}
#'   }
#' @export
data_info <- function() {
  result <- jasp_call("data_info", list())

  structure(
    result,
    class = c("jasp_data_info", "jasp_rpc_result", "list")
  )
}

# ---- S3 helpers -------------------------------------------------------------

#' Coerce a raw RPC result to a data result object
#'
#' @param x A list returned by \code{\link{jasp_call}()}.
#' @return An object of class \code{"jasp_data_result"}.
#' @export
as_data_result <- function(x) {
  structure(x, class = c("jasp_data_result", "jasp_rpc_result", "list"))
}

#' @export
print.jasp_data_result <- function(x, ...) {
  cat("<JASP data result>\n")
  cat("  Status: ", x$status %||% "?", "\n")
  if (!is.null(x$jobId))
    cat("  Job ID: ", x$jobId, "\n")
  if (!is.null(x$path))
    cat("  Path:   ", x$path, "\n")
  if (!is.null(x$rowCount))
    cat("  Rows:   ", x$rowCount, "\n")
  if (!is.null(x$columnCount))
    cat("  Cols:   ", x$columnCount, "\n")
  if (!is.null(x$columns)) {
    cat("  Columns:\n")
    cols <- x$columns
    if (is.list(cols) && length(cols) > 0L) {
      for (i in seq_len(min(length(cols), 10L))) {
        col <- cols[[i]]
        cat(sprintf("    %-20s %-12s", col$name %||% "?", col$type %||% "?"))
        if (!is.null(col$distinctCount))
          cat(sprintf("  (%d distinct)", col$distinctCount))
        cat("\n")
      }
      if (length(cols) > 10L)
        cat(sprintf("    ... and %d more\n", length(cols) - 10L))
    }
  }
  if (!is.null(x$message))
    cat("  Message:", x$message, "\n")
  invisible(x)
}

#' @export
print.jasp_data_info <- function(x, ...) {
  cat("<JASP dataset info>\n")
  cat("  Loaded: ", x$loaded, "\n")
  if (isTRUE(x$loaded)) {
    cat("  Path:   ", x$path %||% "(unknown)", "\n")
    cat("  Rows:   ", x$rowCount %||% "?", "\n")
    cat("  Cols:   ", x$columnCount %||% "?", "\n")
    if (!is.null(x$columns)) {
      cols <- x$columns
      if (is.list(cols) && length(cols) > 0L) {
        cat("  Columns:\n")
        for (i in seq_len(min(length(cols), 10L))) {
          col <- cols[[i]]
          cat(sprintf("    %-20s %s\n", col$name %||% "?", col$type %||% "?"))
        }
        if (length(cols) > 10L)
          cat(sprintf("    ... and %d more\n", length(cols) - 10L))
      }
    }
  }
  invisible(x)
}
