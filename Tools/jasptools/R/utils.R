.jasptoolsReady <- function() {
  initPaths <- .getInternal("initPaths")
  if (is.list(initPaths)) { # paths specified during .onAttach and still need to be inited
    .initResourcePaths(initPaths)
    return(TRUE)
  } else if (isTRUE(initPaths)) { # paths were initialized previously
    return(TRUE)
  } else if (endsWith(getwd(), file.path("Tools"))) { # user manually set wd
    return(TRUE)
  }
  return(FALSE) # paths were not found during initialization
}

.initResourcePaths <- function(paths) {
  for (pathName in names(paths)) {
    setPkgOption(pathName, paths[[pathName]])
  }
  .setInternal("initPaths", TRUE)
}

.initRunEnvironment <- function(envir, dataset, ...) {
  .setInternal("envir", envir) # envir in which the analysis is executed
  .setInternal("dataset", dataset) # dataset to be found later when it needs to be read
  .libPaths(c(.getPkgOption("pkgs.dir"), .libPaths())) # location of JASP's R packages
  .sourceDir(.getPkgOption("r.dir"), envir) # source all the R analysis files
  .exportS3Methods(envir) # ensure S3 methods can be registered to the associated generic functions
  .setRCPPMasks(...) # set the rbridge globals to the value run is called with
}

# it is necessary to export custom S3 methods to the global envir as otherwise they are not registered
.exportS3Methods <- function(env) {
  if (identical(env, .GlobalEnv))
    return(invisible(NULL))

  objs <- ls(env, all.names=FALSE)
  s3 <- vapply(objs, utils::isS3method, envir=env, FUN.VALUE=logical(1))
  objs <- objs[s3]
  objsUserEnv <- ls(.GlobalEnv, all.names=FALSE)
  objs <- objs[! objs %in% objsUserEnv] # prevent overwriting and removing objects from the user's workspace
  for (method in objs)
    assign(method, get(method, envir=env), envir=.GlobalEnv)

  .setInternal("s3Methods", objs)
}

.removeS3Methods <- function() {
  objs <- .getInternal("s3Methods")
  if (is.null(objs))
    return(invisible(NULL))
  rm(list=objs, envir=.GlobalEnv)
}

.setRCPPMasks <- function(...) {
  setFromRun <- list(...)
  for (mask in .masks) { # .masks is a global
    unlockBinding(mask, env = as.environment("package:jasptools"))
    if (mask %in% names(setFromRun)) {
      value <- setFromRun[[mask]]
    } else {
      value <- .getPkgOption(mask)
    }
    assign(mask, value, envir = as.environment("package:jasptools"))
  }
}

.sourceDir <- function(path, envir) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    source(file.path(path, nm), local=envir)
  }
}

.getJSON <- function(analysis, ...) {
  file <- file.path(.getPkgOption("json.dir"), paste0(analysis, ".json"))
  analysisJSON <- try(readLines(file, warn=FALSE), silent=TRUE)
  if (inherits(analysisJSON, "try-error")) {
    stop("The JSON file for the analysis you supplied could not be found.
         Please ensure that (1) its name matches the main R function
         and (2) your working directory is set properly.")
  }

  args <- list(...)
  if (length(args) == 0)
   return(analysisJSON)

  result <- list()
  optsList <- jsonlite::read_json(file)
  for (arg in args) {
    keys <- unlist(strsplit(arg, "=>", fixed=TRUE))
    value <- optsList
    for (key in keys) {
      value <- value[[key]]
    }
    if (is.null(value))
      result[[key]] <- "null"
    else
      result[[key]] <- jsonlite::toJSON(value)
  }
  return(result)
}

.parseUnicode <- function(str) {
  if (! is.character(str) || length(str) == 0)
    stop(paste("Invalid str provided, received", str))

  # used unicode chars in JASP as of 3/11/17.
  # unfortunately I have not found a way to do this more elegantly.
  lookup <- list(
    "\\u002a" = "*",
    "\\u0042" = "B",
    "\\u0046" = "F",
    "\\u00b2" = "²",
    "\\u00f4" = "ô",
    "\\u03a7" = "χ",
    "\\u03b1" = "α",
    "\\u03b5" = "ε",
    "\\u03b7" = "η",
    "\\u03bb" = "λ",
    "\\u03c3" = "σ",
    "\\u03c7" = "χ",
    "\\u03c9" = "ω",
    "\\u2009" = "	",
    "\\u2013" = "–",
    "\\u2014" = "—",
    "\\u2019" = "’",
    "\\u207a" = "⁺",
    "\\u207b" = "⁻",
    "\\u2080" = "₀",
    "\\u2081" = "₁",
    "\\u2082" = "₂",
    "\\u208a" = "₊",
    "\\u208b" = "₋",
    "\\u209a" = "ᵨ", # close enough
    "\\u221e" = "∞",
    "\\u2260" = "≠",
    "\\u2264" = "≤",
    "\\u273b" = "✻"
  )

  for (unicode in names(lookup)) {
    str <- gsub(unicode, lookup[[unicode]], str, ignore.case=TRUE)
  }

  return(str)
}

.restoreOptions <- function(opts) {
  options(opts) # overwrite changed options
  addedOpts <- setdiff(names(options()), names(opts))
  if (length(addedOpts) > 0) {
    options(Map(function(x) NULL, addedOpts)) # remove added options
  }
}

.restoreNamespaces <- function(nms) {
  addedNamespaces <- setdiff(loadedNamespaces(), nms)
  if (length(addedNamespaces) > 0) {
    addedNamespaces <- rev(addedNamespaces) # assuming new pkgs (i.e. dependents) get added later
    addedNamespaces <- addedNamespaces[addedNamespaces != "jaspResults"]
    for (namespace in addedNamespaces) {
      try(unloadNamespace(namespace), silent=TRUE) # this will fail if the pkg is a dependent
    }
  }
}

# not used. Could possibly make pkg unloading more targeted, but does not include pkgs used in other (linked) analyses
.getAnalysisPkgs <- function(analysis, base=FALSE) {
  analysis <- .validateAnalysis(analysis)
  file <- file.path(.getPkgOption("r.dir"), paste0(analysis, ".R"))
  content <- readLines(file, warn=FALSE)
  content <- gsub('#.*', "", content) # remove comments
  matches <- stringr::str_match_all(content, '([a-zA-Z0-9.]{2,}(?<![.]))(?:::|:::)[a-zA-Z0-9._]+')
  analysisPkgs <- unique(unlist(lapply(matches, function(match) match[, 2])))

  if (! base) {
    basePkgs <- utils::installed.packages(priority="high")
    basePkgs <- basePkgs[basePkgs[, "Priority"] == "base", 1]
    if (length(analysisPkgs) > 0)
      analysisPkgs <- analysisPkgs[! analysisPkgs %in% basePkgs]
  }

  if (length(analysisPkgs) > 0)
    return(analysisPkgs)
  return(NULL)
}

.charVec2MixedList <- function(x) {
  x <- stringi::stri_escape_unicode(x)
  x <- gsub("\\\\u.{4}", "<unicode>", x)
  x <- stringi::stri_unescape_unicode(x)
  lapply(x, function(element) {
    res <- element
    if (is.character(element)) {
      num <- suppressWarnings(as.numeric(element))
      if (! is.na(num)) {
        res <- num
      }
    }
    return(res)
  })
}

collapseTable <- function(rows) {
  if (! is.list(rows) || length(rows) == 0) {
    stop("expecting input to be a list with table rows")
  }

  x <- unname(unlist(rows))
  x <- .charVec2MixedList(x)

  return(x)
}

.validateAnalysis <- function(analysis) {
  if (! is.character(analysis) || length(analysis) != 1) {
    stop("expecting non-vectorized character input")
  }

  analysis <- tolower(analysis)
  analyses <- list.files(.getPkgOption("r.dir"), pattern = "\\.[RrSsQq]$")
  analyses <- gsub("\\.[RrSsQq]$", "", analyses)
  if (! analysis %in% analyses) {
    stop("Could not find the analysis. Please ensure that its name matches the main R function.")
  }

  return(analysis)
}
