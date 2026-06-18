# ============================================================================
#  register.R — Dynamic analysis function registration
# ============================================================================
#
#  Provides:
#    %ns%                  — custom infix operator: (module %ns% analysis)(...)
#    register_analyses     — create wrapper functions for every available analysis
#    unregister_analyses   — remove previously registered wrapper functions
#
#  Each generated function wraps the two-step workflow:
#    1. analysis_create(module, analysis) → analysis_id
#    2. analysis_run(analysis_id, options = list(...))
#
#  Usage:
#    # Ad-hoc (no pre-registration):
#    (jaspTTests %ns% TTestIndependentSamples)(variables = list("x","y"), group = "Species")
#
#    # Batch-register all analyses as literal-named functions:
#    register_analyses()
#    TTestIndependentSamples(variables = list("x","y"), group = "Species")
#
#  Built on rlang for safe AST manipulation — no eval(parse(...)), no closure-
#  over-loop-variable bugs.
# ============================================================================

# ---- %ns% operator ----------------------------------------------------------

#' Dynamic module::function call operator
#'
#' @description
#' `%ns%` captures an unquoted module name and analysis name, then returns a
#' **wrapper function** that, when called with \code{(...)}, creates and runs
#' that JASP analysis.
#'
#' Think of it as a dynamic, JASP-aware version of \code{::} — but instead of
#' looking up a function in a real R namespace, it calls
#' \code{\link{analysis_create}()} followed by \code{\link{analysis_run}()}.
#'
#' @section Operator precedence:
#' Because \code{%ns%} has higher precedence than \code{(}, you **must** wrap
#' the expression in parentheses before calling it:
#' \preformatted{
#'   (jaspTTests %ns% TTestIndependentSamples)(x = TRUE, y = FALSE)
#' }
#'
#' @param module An unquoted module name (e.g. \code{jaspTTests}).
#' @param func   An unquoted analysis name (e.g.
#'   \code{TTestIndependentSamples}).
#'
#' @return A function with signature \code{function(...)} that returns a
#'   \code{jasp_analysis_result} object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' jasp_connect()
#'
#' # Call an analysis directly without registering anything
#' result <- (jaspTTests %ns% TTestIndependentSamples)(
#'   variables = list("Sepal.Length", "Sepal.Width"),
#'   group     = "Species"
#' )
#'
#' # Store the wrapper for reuse
#' my_ttest <- jaspTTests %ns% TTestIndependentSamples
#' result1  <- my_ttest(variables = list("x"), group = "g")
#' result2  <- my_ttest(variables = list("y"), group = "g")
#' }
`%ns%` <- function(module, func) {
  # Non-standard evaluation: capture unquoted symbols as strings
  mod_str  <- rlang::as_string(rlang::ensym(module))
  func_str <- rlang::as_string(rlang::ensym(func))

  # Return a closure that wraps the two-step create + run workflow.
  # The strings mod_str / func_str are captured by-value in this frame,
  # so each invocation of %ns% produces an independent closure.
  function(...) {
    id <- .find_or_create(mod_str, func_str)
    analysis_run(analysis_id = id, options = .clean_options(list(...)))
  }
}

# ---- register_analyses -----------------------------------------------------

#' Register dynamic wrapper functions for every available JASP analysis
#'
#' Queries the JASP server for all loaded modules and analyses via
#' \code{\link{modules_list}()}, then creates a closure for each one in the
#' specified environment.  Function names are the **literal analysis names**
#' as returned by the server (e.g. \code{"TTestIndependentSamples"}).
#'
#' Each generated function:
#' \enumerate{
#'   \item Accepts arbitrary named arguments via \code{...}.
#'   \item Calls \code{\link{analysis_create}()} with the captured
#'         module and analysis name.
#'   \item Calls \code{\link{analysis_run}()} with the returned analysis ID
#'         and your options.
#'   \item Returns the \code{jasp_analysis_result} object.
#' }
#'
#' Functions are built with \code{\link[rlang]{new_function}} and
#' \code{\link[rlang]{expr}} — module and analysis names are **inlined as
#' literals** into each function body via \code{!!} unquoting, so there is no
#' closure-over-loop-variable bug and each function is fully self-contained.
#'
#' @param envir  Environment in which to define the wrapper functions.
#'   Defaults to \code{parent.frame()} (usually the global environment or the
#'   calling function's frame).
#' @param overwrite  If \code{TRUE} (default), silently overwrite existing
#'   bindings.  If \code{FALSE}, skip functions whose name already exists in
#'   \code{envir} and issue a message.
#' @param .progress  If \code{TRUE} (default), print a message for each
#'   registered function.
#'
#' @return Invisibly, a data frame (class \code{"jasp_registry"}) with
#'   columns \code{fn_name}, \code{module}, \code{analysis}, and
#'   \code{analysis_title}.  This can be used to inspect what was registered
#'   or to pass to \code{\link{unregister_analyses}()}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' jasp_connect()
#'
#' # Register all analyses as literal-named functions in .GlobalEnv
#' reg <- register_analyses()
#' print(reg)
#'
#' # Call an analysis directly by name
#' TTestIndependentSamples(
#'   variables = list("Sepal.Length", "Sepal.Width"),
#'   group     = "Species"
#' )
#'
#' # Remove everything we registered
#' unregister_analyses(reg)
#' }
register_analyses <- function(envir      = parent.frame(),
                               overwrite  = TRUE,
                               .progress  = TRUE) {
  mods <- modules_list()

  if (nrow(mods) == 0L) {
    message("No modules or analyses found. Is JASP running and connected?")
    return(invisible(empty_registry()))
  }

  n <- nrow(mods)
  registered <- data.frame(
    fn_name         = character(n),
    module          = mods$module,
    analysis        = mods$analysis,
    analysis_title  = mods$analysis_title,
    stringsAsFactors = FALSE
  )

  skipped <- 0L

  for (i in seq_len(n)) {
    mod   <- mods$module[i]
    ana   <- mods$analysis[i]
    title <- mods$analysis_title[i]

    # Literal analysis name as the function name
    fn_name <- ana

    if (!overwrite && exists(fn_name, envir = envir, inherits = FALSE)) {
      message(sprintf("Skipping %s (already exists in target environment)",
                      fn_name))
      skipped <- skipped + 1L
      registered$fn_name[i] <- NA_character_
      next
    }

    # Build the function with rlang::new_function.
    # !! inlines the string values as literals in the AST — no free variables,
    # no closure-over-loop-variable bug.
    fn <- rlang::new_function(
      args = rlang::pairlist2(... = ),
      body = rlang::expr({
        id <- .find_or_create(!!mod, !!ana)
        analysis_run(analysis_id = id, options = .clean_options(list(...)))
      })
    )

    assign(fn_name, fn, envir = envir)
    registered$fn_name[i] <- fn_name

    if (.progress) {
      message(sprintf("Registered: %s()  →  %s::%s", fn_name, mod, ana))
    }
  }

  # Drop rows that were skipped (marked NA)
  registered <- registered[!is.na(registered$fn_name), , drop = FALSE]
  rownames(registered) <- NULL

  class(registered) <- c("jasp_registry", class(registered))
  invisible(registered)
}

# ---- unregister_analyses ---------------------------------------------------

#' Remove previously registered wrapper functions
#'
#' @param reg   A registry data frame returned by
#'   \code{\link{register_analyses}()}.
#' @param envir Environment from which to remove the functions.
#'   Defaults to \code{parent.frame()}.  Must be the same environment used
#'   during registration, otherwise the removal is a no-op.
#'
#' @return Invisibly, a logical vector (named by function name) indicating
#'   whether each function was successfully removed.
#' @export
unregister_analyses <- function(reg, envir = parent.frame()) {
  stopifnot(inherits(reg, "jasp_registry"))

  removed <- logical(nrow(reg))
  names(removed) <- reg$fn_name

  for (i in seq_len(nrow(reg))) {
    nm <- reg$fn_name[i]
    if (exists(nm, envir = envir, inherits = FALSE)) {
      rm(list = nm, envir = envir)
      removed[i] <- TRUE
    }
  }

  n_removed <- sum(removed)
  if (n_removed > 0L) {
    message(sprintf("Unregistered %d function(s)", n_removed))
  }

  invisible(removed)
}

# ---- Internal helpers -------------------------------------------------------

# Strip JASP-internal parameters (data, version, formula) that are not
# valid JSON-RPC analysis options.  These are R-level conveniences that
# would either crash jsonlite serialisation or be silently ignored.
.clean_options <- function(opts) {
  opts$data    <- NULL
  opts$version <- NULL
  opts$formula <- NULL
  opts
}

# Reuse an existing analysis of the same type if one is already in the
# workspace; otherwise create a new one.  Returns the analysis ID.
.find_or_create <- function(module, analysis) {
  existing <- analyses_list()
  match <- existing$module == module & existing$analysis == analysis
  if (any(match)) {
    return(existing$id[which(match)[[1]]])
  }
  a <- analysis_create(module = module, analysis = analysis)
  a$analysisId
}

# ---- S3 helpers -------------------------------------------------------------

empty_registry <- function() {
  df <- data.frame(
    fn_name        = character(0),
    module         = character(0),
    analysis       = character(0),
    analysis_title = character(0),
    stringsAsFactors = FALSE
  )
  class(df) <- c("jasp_registry", class(df))
  df
}

#' @export
print.jasp_registry <- function(x, ...) {
  n <- nrow(x)
  cat(sprintf("<JASP analysis registry: %d registered function(s)>\n", n))
  if (n == 0L) return(invisible(x))

  pad <- max(nchar(x$fn_name), 0L)

  for (i in seq_len(n)) {
    fn  <- x$fn_name[i]
    mod <- x$module[i]
    ana <- x$analysis[i]
    cat(sprintf("  %-*s  →  %s::%s\n", pad, fn, mod, ana))
  }
  invisible(x)
}
