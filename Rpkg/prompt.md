You are updating an R package (`jasprpc`) that wraps the JASP JSON-RPC 2.0 API. The API is described by an OpenRPC 1.x document (`JASP_RPC.json`, attached). Your task: for every method in the spec, ensure an idiomatic R wrapper exists in this package.

## Transport layer (already built — do not modify)

The low-level transport lives in `R/client.R` and exposes:

- `jasp_connect(host, port, endpoint, wait, timeout_ms)` — sets connection state in `.jasp_state`
- `jasp_call(method, params, wait, timeout_ms)` — sends a JSON-RPC 2.0 request via httr2, returns the `result` object with class `"jasp_rpc_result"`, or raises `jasp_rpc_error` / `jasp_api_error` on failure
- `.jasp_state$default_wait` and `.jasp_state$default_timeout_ms` — session defaults for `wait` / `timeoutMs` parameters

All wrapper functions MUST route through `jasp_call()`. Never add httr2 or jsonlite calls directly in wrapper functions.

## File organisation

| File | Contents |
|------|----------|
| `R/client.R` | Connection state, `jasp_connect()`, `jasp_disconnect()`, `jasp_defaults()`, `jasp_call()`. **Do not modify.** |
| `R/analysis.R` | `analysis_*` methods |
| `R/data.R` | `data_*` methods |
| `R/meta.R` | `modules_list`, `analyses_list`, `jasp_ping`, `jasp_discover`, and any methods that don't fit another domain |

New methods go into the file matching their prefix, or `R/meta.R` if they don't fit any domain.

## Naming conventions

- **R function names**: `snake_case` (e.g. `analysis_get_options` not `analysis_getOptions`).
- **R parameter names**: `snake_case` (e.g. `analysis_id` not `analysisId`).
- **Wire names stay `camelCase`** — you pass the JSON method name as a string to `jasp_call()`, and parameter names in the params list match the wire protocol exactly.

## Function template

For each method in the spec, produce:

```r
#' @title <one-line summary from spec>
#'
#' @description <longer description from the spec summary + param
#'   descriptions.  Document every field in the return value.>
#'
#' @param <snake_case_param> <description from spec.params[i].description>
#' ...
#'
#' @return <description of the return S3 class and its fields>
#' @export
<r_function_name> <- function(<params-with-defaults>) {
  stopifnot(...)  # type + length checks for every required param

  result <- jasp_call("<wireMethodName>", list(
    <camelCaseParam> = <snake_case_param>,
    ...
  ))

  <coerce to S3 class>   # as_analysis_result(result), as_data_result(result),
                         # structure(...), etc.
}
```

## Parameter handling

- **Required params** → no default in the R signature; add a `stopifnot()` check.
- **`wait` parameter** → default `.jasp_state$default_wait`.
- **`timeoutMs` parameter** → default `.jasp_state$default_timeout_ms`, R name `timeout_ms`.
- **Other optional params with defaults** → use the default literal from the spec.
- **Type coercion**: integer params → `as.integer()`, boolean → `isTRUE()`.

## Return value S3 classes

| Method family | S3 class | Coercion helper |
|---|---|---|
| `analysis_*` (except `analysis_context`) | `"jasp_analysis_result"` | `as_analysis_result(result)` |
| `analysis_context` | `"jasp_analysis_context"` | `structure(...)` |
| `data_load`, `data_load_status` | `"jasp_data_result"` | `as_data_result(result)` |
| `data_info` | `"jasp_data_info"` | `structure(...)` |
| `modules_list` | `"jasp_modules_list"` (data.frame) | flatten nested JSON → data.frame |
| `analyses_list` | `"jasp_analyses_list"` (data.frame) | flatten nested JSON → data.frame |
| `jasp_discover` | `"jasp_rpc_discover"` | `structure(...)` |
| `jasp_ping` | `logical` scalar | `tryCatch(...)` |

Every new return class MUST have a `print.<class>()` method that shows a structured summary (not raw JSON).

## What NOT to do

- Do NOT add httr2 or jsonlite calls directly in wrapper functions — always go through `jasp_call()`.
- Do NOT change the casing of names sent over the wire.
- Do NOT invent new RPC methods — only wrap methods that exist in the spec.
- Do NOT modify `R/client.R`.
- Do NOT export `jasp_call()` as the primary user API — it's for power users only.

## NAMESPACE

After adding or modifying wrapper functions, update `NAMESPACE` to export every new user-facing function. Keep the existing section comments.

Now inspect the attached `JASP_RPC.json` and the existing package source. Produce a patch that adds or modifies wrapper functions as needed to cover every method in the spec.
