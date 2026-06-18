# ============================================================================
#  client.R — Core JSON-RPC 2.0 transport layer for JASP
# ============================================================================
#
#  Manages:
#    - Connection state (host, port, endpoint)
#    - Request ID generation
#    - Low-level jasp_call() dispatch
#    - httr2 request construction with timeout / error handling
#
#  The wire format is standard JSON-RPC 2.0:
#
#    Request  →  POST /rpc  body: { "jsonrpc":"2.0", "id":N,
#                                    "method":"...", "params":{...} }
#    Response ←  HTTP 200   body: { "jsonrpc":"2.0", "id":N,
#                                    "result":{...} | "error":{...} }
#
#  JaspRpcServer listens on 127.0.0.1:48164 by default (see jasprpcserver.h).
# ============================================================================

# ---- package-level state ----------------------------------------------------

.jasp_state <- new.env(parent = emptyenv())
.jasp_state$host         <- "127.0.0.1"
.jasp_state$port         <- 48164L
.jasp_state$endpoint     <- "/rpc"
.jasp_state$connected    <- FALSE
.jasp_state$request_id   <- 0L
.jasp_state$default_wait <- TRUE
.jasp_state$default_timeout_ms <- 30000L

# ---- public functions -------------------------------------------------------

#' Connect to the JASP RPC server
#'
#' Sets the host, port, and endpoint path to use for all subsequent
#' \code{jasp_call()} calls.  No network traffic is sent until the first
#' actual RPC method call — you can verify reachability with
#' \code{\link{jasp_ping}()}.
#'
#' @param host     Hostname or IP address.  Default \code{"127.0.0.1"}.
#' @param port     TCP port.  Default \code{48164} (the JASP default).
#' @param endpoint HTTP path.  Default \code{"/rpc"}.
#' @param wait     Default value for the \code{wait} parameter in all
#'   calls that accept it.  Default \code{TRUE} (blocking mode).
#' @param timeout_ms Default timeout in milliseconds for calls that block.
#'   Default \code{30000}.
#'
#' @return Invisibly returns a list of the current connection parameters.
#' @export
#'
#' @examples
#' \dontrun{
#' jasp_connect()                     # localhost defaults
#' jasp_connect(port = 9999)          # custom port
#' jasp_connect(host = "192.168.1.5") # remote JASP instance
#' }
jasp_connect <- function(host      = "127.0.0.1",
                         port      = 48164L,
                         endpoint  = "/rpc",
                         wait      = TRUE,
                         timeout_ms = 30000L) {
  .jasp_state$host              <- host
  .jasp_state$port              <- as.integer(port)
  .jasp_state$endpoint          <- endpoint
  .jasp_state$connected         <- TRUE
  .jasp_state$request_id        <- 0L
  .jasp_state$default_wait      <- isTRUE(wait)
  .jasp_state$default_timeout_ms <- as.integer(timeout_ms)

  invisible(jasp_defaults())
}

#' Disconnect and reset state
#'
#' Clears the connection parameters.  Subsequent calls will fail until
#' \code{\link{jasp_connect}()} is called again.
#'
#' @return \code{NULL}, invisibly.
#' @export
jasp_disconnect <- function() {
  .jasp_state$host         <- "127.0.0.1"
  .jasp_state$port         <- 48164L
  .jasp_state$endpoint     <- "/rpc"
  .jasp_state$connected    <- FALSE
  .jasp_state$request_id   <- 0L
  invisible(NULL)
}

#' Return current connection defaults
#'
#' @return A named list with \code{host}, \code{port}, \code{endpoint},
#'   \code{connected}, \code{wait}, and \code{timeout_ms}.
#' @export
jasp_defaults <- function() {
  list(
    host         = .jasp_state$host,
    port         = .jasp_state$port,
    endpoint     = .jasp_state$endpoint,
    connected    = .jasp_state$connected,
    wait         = .jasp_state$default_wait,
    timeout_ms   = .jasp_state$default_timeout_ms
  )
}

# ---- low-level dispatch -----------------------------------------------------

#' Call an arbitrary JASP JSON-RPC method
#'
#' This is the low-level workhorse.  Most users should prefer the higher-level
#' wrappers (\code{analysis_create}, \code{data_load}, etc.) which validate
#' parameters and return structured S3 objects.
#'
#' @param method   RPC method name (character).
#' @param params   Named list of parameters (default \code{list()}).
#' @param wait     Override the default \code{wait} behaviour for this call.
#'   If \code{NULL}, the session default is used.
#' @param timeout_ms Override the default timeout for this call.
#'   If \code{NULL}, the session default is used.
#'
#' @return The \code{result} object from the JSON-RPC response, with the
#'   \code{"jasp_rpc_result"} S3 class attached.  If the server returned a
#'   JSON-RPC error, an R error of class \code{jasp_rpc_error} is raised.
#'
#' @export
jasp_call <- function(method,
                      params     = list(),
                      wait       = NULL,
                      timeout_ms = NULL) {

  if (!.jasp_state$connected) {
    stop("Not connected. Call jasp_connect() first.")
  }

  # ---- build request --------------------------------------------------------
  .jasp_state$request_id <- .jasp_state$request_id + 1L
  id <- .jasp_state$request_id

  body <- list(
    jsonrpc = "2.0",
    id      = id,
    method  = method,
    params  = params
  )

  url <- sprintf("http://%s:%d%s",
                 .jasp_state$host,
                 .jasp_state$port,
                 .jasp_state$endpoint)

  # ---- resolve wait / timeout -----------------------------------------------
  if (!is.null(wait))       params$wait       <- wait
  if (!is.null(timeout_ms)) params$timeoutMs  <- timeout_ms

  # Ensure defaults if the method supports them and they weren't set
  if (!is.null(wait) || !is.null(timeout_ms)) {
    body$params <- params
  }

  # ---- coerce empty unnamed lists to empty objects ({}) --------------------
  # R's list() serializes to [], but the server expects {} for object params.
  # We only coerce the top-level params value — nested arrays (e.g. types=[])
  # must stay as arrays, so we do NOT recurse.
  if (is.list(body$params) && length(body$params) == 0L && is.null(names(body$params)))
    body$params <- structure(list(), names = character(0))

  # ---- perform request ------------------------------------------------------
  # httr2 pipeline: build → timeout → perform → parse
  timeout_secs <- (params$timeoutMs %||% .jasp_state$default_timeout_ms) / 1000

  # Apply a small grace period above the server-side timeout so we don't cut
  # the socket before the server has a chance to respond.
  client_timeout <- max(timeout_secs + 5, timeout_secs * 1.2)

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_body_json(body, auto_unbox = TRUE, null = "null") |>
      httr2::req_method("POST") |>
      httr2::req_timeout(client_timeout) |>
      httr2::req_retry(max_tries = 2L) |>
      httr2::req_perform(),
    error = function(e) {
      stop("jasp_call: HTTP request failed for method '", method, "': ",
           conditionMessage(e), call. = FALSE)
    }
  )

  # ---- parse response -------------------------------------------------------
  # Server returns text/plain (not application/json), so we read the raw
  # body and parse it ourselves rather than using resp_body_json().
  parsed <- tryCatch(
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE),
    error = function(e) {
      stop("jasp_call: failed to parse JSON response for method '",
           method, "': ", conditionMessage(e), call. = FALSE)
    }
  )

  # ---- JSON-RPC error handling ----------------------------------------------
  if (!is.null(parsed$error)) {
    err <- parsed$error
    msg <- sprintf("[%d] %s",
                   err$code %||% -32603,
                   err$message %||% "Unknown JSON-RPC error")
    if (!is.null(err$data)) {
      msg <- paste0(msg, "\n  Data: ", jsonlite::toJSON(err$data, auto_unbox = TRUE))
    }
    e <- simpleError(msg)
    class(e) <- c("jasp_rpc_error", "error", "condition")
    e$rpc_code    <- err$code
    e$rpc_message <- err$message
    e$rpc_data    <- err$data
    stop(e)
  }

  result <- parsed$result

  # ---- handle endpoint-level errors embedded in result ----------------------
  if (is.list(result) && identical(result$status, "error")) {
    msg <- result$message %||% "Unknown JASP API error"
    e <- simpleError(msg)
    class(e) <- c("jasp_api_error", "error", "condition")
    e$api_status <- "error"
    stop(e)
  }

  class(result) <- c("jasp_rpc_result", class(result))
  result
}

# ---- helpers ----------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x
