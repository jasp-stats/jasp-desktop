#'
#' Entry point for RoboReport scripts.
#'
#' Called by the C++ `RoboReportManager` via `engine->runScriptOnProcess()`.
#' The C++ side generates typed R code like:
#'
#' ```r
#' jaspRoboReport::run_script(
#'     path       = "/opt/.../TTestIndependent.R",
#'     analysisId = 123L,
#'     rpcHost    = "127.0.0.1",
#'     rpcPort    = 48164L
#' )
#' ```
#'
#' No environment variables are used - all context is passed as typed
#' function arguments.

# ============================================================================
# run_script - the C++ entry point
# ============================================================================

#' Run a RoboReport script.
#'
#' Sources the script file into a fresh environment (whose parent is the
#' jaspRoboReport namespace, so `rr_*` helpers resolve without a `::`
#' prefix), then calls the `roboreport_main(analysisId)` function that
#' every script must define.
#'
#' Endpoint configuration (`rpcHost`/`rpcPort`) is stored in a
#' package-level environment; script code never touches it directly.
#'
#' @param path Filesystem path to the RoboReport script.
#' @param analysisId The integer ID of the source analysis to report on.
#' @param rpcHost Hostname/IP of the JASP RPC server.
#' @param rpcPort Port of the JASP RPC server.
#' @param timeoutMs RPC request timeout in milliseconds (default 60000).
#' @return The return value of `roboreport_main()`, invisibly.
#' @export
run_script <- function(path, analysisId, rpcHost, rpcPort,
                       timeoutMs = 60000L) {

  if (!file.exists(path))
    stop("RoboReport script not found: ", path, call. = FALSE)

  # Store endpoint config for rr_call() - script code never sees these.
  .rr_set_endpoint(host = rpcHost, port = rpcPort, timeoutMs = timeoutMs)

  # Source into a fresh env whose parent is this package's namespace.
  # This lets script code call rr_results(), rr_analysis_run(), etc.
  # without a jaspRoboReport:: prefix, while keeping the script's own
  # definitions out of the global env.
  script_env <- new.env(parent = asNamespace("jaspRoboReport"))

  tryCatch(
    source(path, local = script_env),
    error = function(e) {
      stop("Failed to source RoboReport script: ", conditionMessage(e),
           call. = FALSE)
    }
  )

  # The script contract: every script defines roboreport_main(analysisId).
  if (!exists("roboreport_main", envir = script_env, inherits = FALSE)) {
    stop(
      "RoboReport script must define: ",
      "roboreport_main <- function(analysisId) { ... }\n",
      "Missing in: ", path,
      call. = FALSE
    )
  }

  main_fn <- get("roboreport_main", envir = script_env)
  if (!is.function(main_fn)) {
    stop("'roboreport_main' must be a function, got: ",
         class(main_fn), call. = FALSE)
  }

  # Fire it. analysisId is the one typed parameter every script gets.
  main_fn(analysisId = analysisId)
}
