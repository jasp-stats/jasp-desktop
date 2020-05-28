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
  # source all the R analysis files
  if (.isModule())
    .initModuleRequisites(envir)
  else
    .sourceDir(.getPkgOption("common.r.dir"), envir)
  .setInternal("envir", envir) # envir in which the analysis is executed
  .setInternal("dataset", dataset) # dataset to be found later when it needs to be read
  .libPaths(c(.getPkgOption("pkgs.dir"), .libPaths())) # location of JASP's R packages
  .exportS3Methods(envir) # ensure S3 methods can be registered to the associated generic functions
  .setRCPPMasks(...) # set the rbridge globals to the value run is called with
}

# it is necessary to export custom S3 methods to the global envir as otherwise they are not registered
# unneeded at present and seems unsupported on later R versions 
.exportS3Methods <- function(env) {
  return()
  
  if (identical(env, .GlobalEnv)) {
    .setInternal("s3Methods", NULL)
    return(invisible(NULL))
  }

  objs <- ls(env, all.names=FALSE)
  s3 <- vapply(objs, utils::isS3method, envir=env, FUN.VALUE=logical(1))
  objs <- objs[s3]
  objsUserEnv <- ls(.GlobalEnv, all.names=FALSE)
  objs <- objs[! objs %in% objsUserEnv] # prevent overwriting and removing objects from the user's workspace
  for (method in objs)
    assign(method, get(method, envir=env), envir=.GlobalEnv)

  .setInternal("s3Methods", objs)
}

# unneeded at present and seems unsupported on later R versions 
.removeS3Methods <- function() {
  return()
  
  objs <- .getInternal("s3Methods")
  if (length(objs))
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

.initModuleRequisites <- function(envir) {
  if (!"JASP" %in% installed.packages()) {
    if (dir.exists(.getPkgOption("common.r.dir")))
      install.packages(file.path(.getPkgOption("common.r.dir"), ".."), type="source", repos=NULL)
    else
      warning("Cannot find jasp-desktop/JASP-Engine/JASP; it won't be possible to evaluate JASP:: calls in your code.\n
              Is the `common.r.dir` specified correctly in `viewPkgOptions()`?")
  }
  .sourceModuleCode(envir)
}

.sourceModuleCode <- function(envir) {
  rFiles <- c("base64", "common", "commonerrorcheck", "commonmessages", "exposeUs")
  .sourceDir(.getPkgOption("common.r.dir"), envir, fileNames=rFiles)
  .sourceDir(file.path(.getPkgOption("module.dir"), "R"), envir)
}

.getModuleDescription <- function() {
  instFolder <- file.path(.getPkgOption("module.dir"), "inst")
  if (!"description.json" %in% list.files(instFolder))
    stop("Cannot find a description.json file in the module provided in `module.dir`")
  
  return(jsonlite::read_json(file.path(instFolder, "description.json")))
}


.sourceDir <- function(paths, envir, fileNames=NULL) {
  for (i in 1:length(paths)) {
    rFilePaths <- list.files(paths[i], pattern = "\\.[RrSsQq]$", recursive=TRUE)
    for (rFilePath in rFilePaths) {
      rFileName <- tools::file_path_sans_ext(basename(rFilePath))
      if (! is.null(fileNames) && ! rFileName %in% fileNames)
        next
      source(file.path(paths[i], rFilePath), local=envir)
    }
  }
}

.convertResultsListToJson <- function(lst) {
  json <- try(jsonlite::toJSON(lst, null="null", auto_unbox=TRUE, digits=NA))
  if (inherits(json, "try-error"))
    json <- paste0("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"Unable to jsonify\" } }")
  
  json <- .parseUnicode(json)
  json <- gsub("<div class=stack-trace>", "<div>", json, fixed=TRUE) # this makes sure the stacktrace is not hidden
  json <- gsub("\\\"", "\\\\\"", json, fixed=TRUE) # double escape all escaped quotes (otherwise the printed json is invalid)
  
  return(json)
}

.insertJsonInHtml <- function(json, htmlFile) {
  html <- readChar(file.path(.getPkgOption("html.dir"), "index.html"), 1000000)
  insertedJS <- paste0(
    "<script>
      var jasp = {}
      jQuery(function($) {
        $(document).ready(function() {
          window.analysisChanged(", json, ")
        })
      })
    </script></body>")
  html <- gsub("</body>", insertedJS, html)
  html <- .changeJsIncludeForAdblockers(html)
  
  writeChar(html, htmlFile)
}

.initializeOutputFolder <- function(folder) {
  if (!dir.exists(folder))
    dir.create(folder, recursive=TRUE)
    
  if (! "js" %in% list.dirs(folder, full.names=FALSE))
    file.copy(list.files(.getPkgOption("html.dir"), full.names = TRUE), folder, recursive = TRUE)

  .renameJsFileForAdblockers(folder)
}

.changeJsIncludeForAdblockers <- function(html) {
  gsub("analysis.js", "jaspanalysis.js", html, fixed = TRUE)
}

.renameJsFileForAdblockers <- function(folder) {
  if (file.exists(file.path(folder, "js", "analysis.js")))
    file.rename(file.path(folder, "js", "analysis.js"), file.path(folder, "js", "jaspanalysis.js"))
}

.usesJaspResults <- function(analysis) {
  qmlFile <- .getQMLFile(analysis)
  if (!is.null(qmlFile))
    return(.jaspResultsExistsInQMLFile(qmlFile))
  stop("Could not find the options file for analysis ", analysis)
}

.jaspResultsExistsInQMLFile <- function(file) {
  fileSize <- file.info(file)$size 
  fileContents <- readChar(file, nchars=fileSize)
  fileContents <- gsub('[[:blank:]]|\\"', "", fileContents)
  
  jaspResults <- TRUE
  if (isTRUE(grepl("usesJaspResults:false", fileContents))) {
    jaspResults <- FALSE
  }
  return(jaspResults)
}

.transferPlotsFromjaspResults <- function() {
  pathPlotsjaspResults <- file.path(tempdir(), "jaspResults", "plots")
  pathPlotsjasptools <- file.path(tempdir(), "jasptools", "html")
  if (dir.exists(pathPlotsjaspResults)) {
    plots <- list.files(pathPlotsjaspResults)
    if (length(plots) > 0) {
      file.copy(file.path(pathPlotsjaspResults, plots), pathPlotsjasptools, overwrite=TRUE)
    }
  }
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

.getInstallLocationDep <- function(dep) {
  pkgs <- installed.packages()
  index <- min(which(row.names(pkgs) == dep))
  return(pkgs[index, "LibPath"])
}

.restoreOptions <- function(opts) {
  options(opts) # overwrite changed options
  addedOpts <- setdiff(names(options()), names(opts))
  if (length(addedOpts) > 0) {
    options(Map(function(x) NULL, addedOpts)) # remove added options
  }
}

.restoreNamespaces <- function(nms) {
  nms <- unique(c(nms, "jasptools", "jaspResults", "JASPgraphs", "Rcpp", "vdiffr", "testthat", "jsonlite"))
  for (i in 1:2) {
    if (length(setdiff(loadedNamespaces(), nms)) == 0)
      break
    addedNamespaces <- setdiff(loadedNamespaces(), nms)
    dependencies <- unlist(lapply(addedNamespaces, tools:::dependsOnPkgs))
    namespaces <- c(dependencies, addedNamespaces)
    namespaces <- names(sort(table(namespaces), decreasing=TRUE))
    addedNamespaces <- namespaces[namespaces %in% loadedNamespaces() & !namespaces %in% nms]
    for (namespace in addedNamespaces) {
      suppressWarnings(suppressMessages(try(unloadNamespace(namespace), silent=TRUE)))
    }
  }
  suppressWarnings(R.utils::gcDLLs())
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
  if (! is.list(rows) || length(rows) == 0)
    stop("expecting input to be a list (with a list for each JASP table row)")

  x <- unname(unlist(rows))
  x <- .charVec2MixedList(x)

  return(x)
}

.validateAnalysis <- function(analysis) {
  if (! is.character(analysis) || length(analysis) != 1) {
    stop("expecting non-vectorized character input")
  }

  if (.isModule())
    analyses <- list.files(file.path(.getPkgOption("module.dir"), "R"), pattern = "\\.[RrSsQq]$", recursive=TRUE)
  else
    analyses <- list.files(.getPkgOption("common.r.dir"), pattern = "\\.[RrSsQq]$", recursive=TRUE)
  analyses <- gsub("\\.[RrSsQq]$", "", analyses)
  if (! tolower(analysis) %in% tolower(analyses)) {
    stop("Could not find the analysis. Please ensure that its name matches the main R function.")
  }

  return(analysis)
}

.getCasedNameMatchWithFunction <- function(name, envir) {
  fnNames <- names(envir)[which(tolower(names(envir)) == tolower(name))]
  for (fnName in fnNames) {
    fnArgs <- formals(envir[[fnName]])
    if (all(c("dataset", "options") %in% names(fnArgs)))
      return(fnName)
  }
  stop(name, " does not have the required options and dataset arguments, not sure how to call this")
}

.insideTestEnvironment <- function() {
  testthat <- vapply(sys.frames(), 
    function(frame) 
      methods::getPackageName(frame) == "testthat", 
    logical(1))
  if (any(testthat)) {
    return(TRUE)
  }
  return(FALSE)
}

.replaceFn <- function(fnName, fn, pkgName) {
  reAssign <- function(env) {
    unlockBinding(fnName, env)
    assign(fnName, fn, env)
    lockBinding(fnName, env)
  }
  
  try(silent=TRUE, {
    reAssign(getNamespace(pkgName)) # if not attached
    reAssign(as.environment(paste0("package:", pkgName))) # if attached
  })
}

.getErrorMsgFromLastResults <- function() {
  lastResults <- .getInternal("lastResults")
  if (jsonlite::validate(lastResults))
    lastResults <- jsonlite::fromJSON(lastResults)
    
  if (is.null(lastResults) || !is.list(lastResults) || is.null(names(lastResults)))
    return(NULL)
  
  if ((lastResults[["status"]] == "validationError" || lastResults[["status"]] == "fatalError") && is.list(lastResults[["results"]]))
    return(.errorMsgFromHtml(lastResults$results$errorMessage))

  return(NULL)
}

.isModule <- function() {
  moduleDir <- .getPkgOption("module.dir")
  if (dir.exists(moduleDir)) {
    return(TRUE)
  }
  return(FALSE)
}

.errorMsgFromHtml <- function(html) {
  indexStackTrace <- unlist(gregexpr("<div class=stack-trace", html, fixed=TRUE))[1]
  if (indexStackTrace > -1) {
    error <- substr(html, 1, indexStackTrace - 1)
    error <- gsub("<br>", " ", error, fixed=TRUE)
    stackTrace <- stringr::str_match(html, "<div class=stack-trace>(.*)<\\/div>")[1, 2]
    html <- paste0(error, "\n\nStacktrace within JASP:\n----------\n", stackTrace, "\n----------\n")
  }
  parsedMsg <- gsub("<br>", "\n", html, fixed=TRUE)
  return(parsedMsg)
}
