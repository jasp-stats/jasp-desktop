# jasprpc — R Client for the JASP JSON-RPC API

An R package that communicates with a running
[JASP](https://jasp-stats.org/) desktop instance via its JSON-RPC 2.0 HTTP
API.

## Quick start

```r
library(jasprpc)

# Connect (defaults to localhost:48164)
jasp_connect()

# Verify the server is reachable
jasp_ping()  # TRUE

# Discover what's available
modules_list()
#>   module       module_title   analysis                   analysis_title
#> 1 jaspTTests  T-Tests        TTestIndependentSamples    Independent Samples T-Test
#> 2 jaspTTests  T-Tests        TTestPairedSamples         Paired Samples T-Test
#> ...

# Get detailed help for an analysis
analysis_context("jaspTTests", "TTestIndependentSamples")

# Load data
data_load("/path/to/data.csv")
data_info()
```

## Full workflow

```r
jasp_connect()

# 1. Load a dataset
data_load("/tmp/iris.csv")

# 2. Create an analysis
a <- analysis_create("jaspTTests", "TTestIndependentSamples")
print(a)
#> <JASP analysis result>
#>   Status:       success
#>   Analysis ID:  1
#>   Module:       jaspTTests
#>   Analysis:     TTestIndependentSamples
#>   Options:      24 option(s)

# 3. Inspect the default options
str(a$options)
str(a$optionMeta)

# 4. Customise and run
result <- analysis_run(a$analysisId, list(
  variables = list("Sepal.Length", "Sepal.Width"),
  group     = "Species"
))
print(result)
#> <JASP analysis result>
#>   Status:       success
#>   Results:      3 top-level key(s)
```

## Design

- **Transport:** [httr2](https://httr2.r-lib.org) for HTTP with timeout
  handling and retries.
- **Protocol:** JSON-RPC 2.0 over `POST /rpc` (see
  `jasprpcserver.h` in the JASP source).
- **Spec:** All 13 methods from `JASP_RPC.json` are wrapped.

## Developer notes

- `R/regenerate.R` contains a prompt and script for updating the R bindings
  when the OpenRPC spec changes.

## Installation

```r
remotes::install_local("Rpkg")
```
