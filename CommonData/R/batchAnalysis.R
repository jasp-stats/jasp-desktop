#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Helper to create several JASP analyses at once from R code in the R Commander.
#
# Style A (pass the analysis wrapper directly):
#   batchAnalysis(jaspDescriptives::Descriptives,
#                 formula = ~ contGamma, quantilesType = as.character(1:7))
#
# Style B (pass a closure for more elaborate setups):
#   batchAnalysis(function(qt) jaspDescriptives::Descriptives(formula = ~ contGamma,
#                 quantilesType = qt), qt = as.character(1:7))
#
# Arguments after `.fun` are expanded: length 1 is recycled, length n is expanded.
# Wrap a vector in `list()` (or `I()`) to pass it as a single value.
#
# This file is embedded in the JASP binary (make_includable, see CommonData/CMakeLists.txt)
# and sourced into the global environment at engine init, so that the R Commander works
# without waiting for module bundles that ship a jaspBase with batchAnalysis(). It is
# copied verbatim from jaspBase PR #221 (R/batchAnalysis.R) with one change:
# .normalizeBatchResult() serializes with an explicit jsonlite::toJSON() call, because
# jaspBase's own toJSON() wrapper is namespace-internal and unresolvable from the global
# environment.

#' @export
batchAnalysis <- function(.fun, ..., .override = NULL, .dryRun = FALSE) {
  args <- list(...)
  if (length(args) == 0L)
    stop("At least one argument must be supplied to vary across analyses.", call. = FALSE)

  lengths <- .batchArgLengths(args)
  n <- max(lengths)
  if (n == 0L)
    stop("Arguments cannot be empty.", call. = FALSE)

  bad <- !(lengths %in% c(1L, n))
  if (any(bad)) {
    nms <- names(args)
    if (is.null(nms))
      nms <- paste0("arg", seq_along(args))
    stop(
      "Arguments must have length 1 or ", n, "; got ",
      paste0(nms[bad], " (length ", lengths[bad], ")", collapse = ", "),
      ". Wrap a vector in list() or I() to pass it as a single value.",
      call. = FALSE
    )
  }

  nms <- names(args)
  if (is.null(nms))
    nms <- paste0("arg", seq_along(args))
  varying <- nms[lengths == n]

  results <- vector("list", n)
  nFailed <- 0L
  for (i in seq_len(n)) {
    callArgs <- lapply(args, .batchTakeArg, i = i)
    res <- tryCatch(
      .batchParseResult(do.call(.fun, callArgs)),
      error = function(e) {
        cat(sprintf("Error in analysis %d: %s\n", i, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(res)) {
      nFailed <- nFailed + 1L
    } else {
      results[[i]] <- res
    }
  }

  results <- .batchApplyTitles(results, .override, args, varying, n)

  # Drop failed calls (already reported to the log) so the result only contains
  # analyses that can actually be created.
  results <- results[!vapply(results, is.null, logical(1))]

  if (nFailed > 0L)
    cat(sprintf("Batch summary: %d of %d analyses succeeded, %d failed.\n", n - nFailed, n, nFailed))

  class(results) <- c("jaspBatchAnalysis", "list")
  if (isTRUE(.dryRun))
    attr(results, "dryRun") <- TRUE

  results
}

#' @export
print.jaspBatchAnalysis <- function(x, ...) {
  if (isTRUE(attr(x, "dryRun")))
    cat(sprintf("Dry run: %d analyses would be added:\n", length(x)))
  else
    cat(sprintf("<jaspBatchAnalysis> %d analyses:\n", length(x)))
  for (i in seq_along(x)) {
    a     <- x[[i]]
    mod   <- if (!is.null(a$module))   a$module   else "?"
    ana   <- if (!is.null(a$analysis)) a$analysis else "?"
    title <- if (!is.null(a$title) && nzchar(a$title)) a$title else paste0(mod, "::", ana)
    cat(sprintf("  %d. %s\n", i, title))
  }
  invisible(x)
}

# ---- internal helpers ----------------------------------------------------

.batchArgLengths <- function(args) {
  vapply(args, function(a) {
    if (is.null(a) || inherits(a, "AsIs") || inherits(a, "formula")) 1L else length(a)
  }, integer(1))
}

.batchTakeArg <- function(a, i) {
  if (is.null(a))                          return(NULL)        # NULL default (e.g. data = NULL), recycled
  if (inherits(a, "AsIs"))                 return(unclass(a))  # I(...): always a single value
  if (inherits(a, "formula"))              return(a)           # a single formula, recycled
  if (is.list(a) && length(a) == 1L)       return(a[[1L]])     # list(...): a single value
  if (length(a) == 1L)                     return(a)           # recycle scalar
  a[[i]]                                                       # expand
}

# simplifyVector = FALSE so that re-serializing gives back exactly the JSON that
# runWrappedAnalysis produced (otherwise e.g. ["a"] would become "a").
.batchParseResult <- function(res) {
  if (is.character(res) && length(res) == 1L)
    res <- jsonlite::fromJSON(res, simplifyVector = FALSE)
  if (!is.list(res) || is.null(res[["module"]]) || is.null(res[["analysis"]]))
    stop("the function did not return a JASP analysis", call. = FALSE)
  res
}

.batchIsAnalysisJson <- function(x) {
  isTRUE(tryCatch({
    parsed <- jsonlite::fromJSON(x, simplifyVector = FALSE)
    is.list(parsed) && !is.null(parsed[["module"]]) && !is.null(parsed[["analysis"]])
  }, error = function(e) FALSE))
}

.batchApplyTitles <- function(results, .override, args, varying, n) {
  title <- if (is.list(.override)) .override$title else NULL

  # No explicit title: auto-generate one from the varying arguments so that
  # analyses in a batch are distinguishable.
  if (is.null(title)) {
    if (n <= 1L || length(varying) == 0L)
      return(results)
    for (i in seq_len(n)) {
      if (is.null(results[[i]]))
        next
      parts <- vapply(varying, function(nm) {
        paste0(nm, "=", .batchStringify(.batchTakeArg(args[[nm]], i)))
      }, character(1L))
      ana <- results[[i]]$analysis
      if (is.null(ana))
        ana <- "analysis"
      results[[i]]$title <- paste0(ana, ": ", paste(parts, collapse = ", "))
    }
    return(results)
  }

  # Function: full control, receives (index, resolved args, parsed analysis).
  if (is.function(title)) {
    for (i in seq_len(n)) {
      if (is.null(results[[i]]))
        next
      callArgs <- lapply(args, .batchTakeArg, i = i)
      results[[i]]$title <- as.character(title(i, callArgs, results[[i]]))
    }
    return(results)
  }

  # Character: a template (with {arg} placeholders) or a length-n vector.
  if (!is.character(title) || !(length(title) %in% c(1L, n)))
    stop("`.override$title` must be a character string/template of length 1 or ", n, ", or a function.", call. = FALSE)

  for (i in seq_len(n)) {
    if (is.null(results[[i]]))
      next
    template  <- if (length(title) == 1L) title else title[[i]]
    callArgs  <- lapply(args, .batchTakeArg, i = i)
    results[[i]]$title <- .batchInterpolate(template, callArgs)
  }

  results
}

.batchStringify <- function(x) {
  if (is.null(x))             return("NULL")
  if (is.character(x))        return(paste(x, collapse = ", "))
  if (inherits(x, "formula")) return(paste(deparse(x), collapse = " "))
  paste(as.character(x), collapse = ", ")
}

.batchInterpolate <- function(template, callArgs) {
  if (!is.character(template) || length(template) != 1L)
    return(template)
  matches <- regmatches(template, gregexpr("\\{[^{}]+\\}", template, perl = TRUE))[[1L]]
  for (m in matches) {
    nm <- substr(m, 2L, nchar(m) - 1L)
    replacement <- if (nm %in% names(callArgs)) .batchStringify(callArgs[[nm]]) else m
    template <- gsub(m, replacement, template, fixed = TRUE)
  }
  template
}

# Normalizes the return value of arbitrary R code into a JSON array of analysis
# objects. Used by the R Commander engine to transport batch results to the
# desktop, which creates one analysis per entry. Only a jaspBatchAnalysis, or
# (a list of) analysis JSON strings as returned by the wrappers, yield analyses;
# any other value (data frames, model objects, ordinary lists) gives "[]" without
# being serialized.
.normalizeBatchResult <- function(val) {
  tryCatch({
    if (inherits(val, "jaspBatchAnalysis")) {
      if (isTRUE(attr(val, "dryRun")) || length(val) == 0L)
        return("[]")   # dry run: create nothing
      # Explicit parameters instead of jaspBase's namespace-internal toJSON() wrapper:
      # auto_unbox keeps length-1 values scalar (the desktop reads module/analysis/title
      # as strings), digits = NA gives R's maximum precision (15 significant digits,
      # where the jsonlite default of 4 decimal digits would corrupt computed values),
      # and null = "null" serializes NULL as JSON null instead of {}.
      return(jsonlite::toJSON(val, auto_unbox = TRUE, digits = NA, null = "null"))
    }

    strings <- if (is.character(val))
      val
    else if (is.list(val) && !is.object(val) && length(val) > 0L &&
             all(vapply(val, function(x) is.character(x) && length(x) == 1L, logical(1L))))
      unlist(val, use.names = FALSE)
    else
      character(0L)

    if (length(strings) == 0L || anyNA(strings) || !all(startsWith(trimws(strings), "{")))
      return("[]")
    if (!all(vapply(strings, .batchIsAnalysisJson, logical(1L), USE.NAMES = FALSE)))
      return("[]")

    # the strings are already valid JSON, pass them on unchanged
    paste0("[", paste(strings, collapse = ","), "]")
  }, error = function(e) "[]")
}
