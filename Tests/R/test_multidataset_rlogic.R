# Standalone checks for the multi-dataset mode logic in jaspBase's common.R
# (Engine/jaspBase/R/common.R): the run-state flag, the readDataSet* guards and
# the `datasets` handout order that pairs with the engine's slice queue.
#
# Run with:  Rscript Tests/R/test_multidataset_rlogic.R
# (also registered as the ctest "testMultiDataSetRLogic")

jaspSourceDir <- Sys.getenv("JASP_SOURCE_DIR", getwd())
commonR       <- file.path(jaspSourceDir, "Engine", "jaspBase", "R", "common.R")

if (!file.exists(commonR))
	stop("Could not find ", commonR, " - set JASP_SOURCE_DIR to the jasp-desktop checkout")

exprs  <- parse(commonR)
wanted <- c(".multiDataSetState", ".multiDataSetMode", ".stopIfMultiDataSetMode", ".isMultiDataSetJson",
            ".readDataSetCleanNAs", ".readDataSetToEnd", ".readFullDataset", ".readDataSetHeader",
            "readDataSetByVariableTypes")

env <- new.env(parent = baseenv())
found <- character()

for (e in exprs) {
	if (is.call(e) && length(e) == 3L && identical(as.character(e[[1]]), "<-") && is.name(e[[2]])) {
		name <- as.character(e[[2]])
		if (name %in% wanted) {
			eval(e, envir = env)
			found <- c(found, name)
		}
	}
}

missing <- setdiff(wanted, found)
if (length(missing) > 0)
	stop("common.R did not define (as expected): ", paste(missing, collapse = ", "))

# --- stubs for the native R-C++ callbacks -------------------------------------------------------
readCount <- 0
env$.fromRCPP <- function(fname, ...) {
	readCount <<- readCount + 1
	data.frame(reading = readCount)
}
env$.excludeNaListwise <- function(dataset, exclude.na.listwise) dataset

failures <- 0
check <- function(ok, label) {
	if (isTRUE(ok)) cat("PASS :", label, "\n") else {
		failures <<- failures + 1
		cat("FAIL :", label, "\n")
	}
}

# --- .isMultiDataSetJson ------------------------------------------------------------------------
check(identical(env$.isMultiDataSetJson(NULL), FALSE),			"NULL multiDataSetJson is not a multi run")
check(identical(env$.isMultiDataSetJson(""), FALSE),			"empty multiDataSetJson is not a multi run")
check(identical(env$.isMultiDataSetJson("null"), FALSE),		"'null' multiDataSetJson is not a multi run")
check(identical(env$.isMultiDataSetJson("{\"ids\":[]}"), TRUE),	"json multiDataSetJson is a multi run")

# --- mode flag and guards -----------------------------------------------------------------------
check(identical(env$.multiDataSetMode(), FALSE), "mode starts off")

datasetBefore <- env$.readDataSetToEnd(columns = "Score")
check(is.data.frame(datasetBefore), "readDataSetToEnd works while not in multi mode")
check(!inherits(tryCatch(env$.readDataSetHeader(columns = "Score"), error = function(e) e), "error"),
	  "readDataSetHeader callable when not multi")

invisible(env$.multiDataSetMode(TRUE))

for (fnName in c(".readDataSetToEnd", ".readFullDataset", ".readDataSetHeader")) {
	err <- tryCatch({ env[[fnName]](columns = "Score"); "" }, error = function(e) conditionMessage(e))
	check(grepl("not available in multi-dataset aware analyses", err),
		  paste0(fnName, " stops in multi mode"))
}

err <- tryCatch({ env$readDataSetByVariableTypes(list(), character()); "" }, error = function(e) conditionMessage(e))
check(grepl("not available in multi-dataset aware analyses", err), "readDataSetByVariableTypes stops in multi mode")

# --- runJaspResults' datasets handout (must mirror the engine slice queue) ------------------------
readCount <- 0
ids       <- c("5", "12")
datasets  <- lapply(ids, function(id) env$.fromRCPP(".readDataSetRequestedNative"))
names(datasets) <- ids
attr(datasets, "dataSetNames") <- c("5" = "Alpha", "12" = "Beta")

check(identical(readCount, 2), "one native read per referenced dataset, in ids order")
check(identical(names(datasets), ids), "datasets are keyed by dataset id")
check(identical(datasets[["5"]]$reading, 1) && identical(datasets[["12"]]$reading, 2),
	  "first read got the first slice, second read got the second slice")
check(identical(attr(datasets, "dataSetNames")[["5"]], "Alpha"), "titles are carried as attribute")

invisible(env$.multiDataSetMode(FALSE))
datasetAfter <- env$.readDataSetToEnd(columns = "Score")
check(is.data.frame(datasetAfter), "readDataSetToEnd works again after the mode was reset")

# --- reset semantics like runJaspResults' on.exit -----------------------------------------------
wrapper <- function() {
	env$.multiDataSetMode(TRUE)
	on.exit(env$.multiDataSetMode(FALSE), add = TRUE)
	stopifnot(env$.multiDataSetMode())
}
wrapper()
check(identical(env$.multiDataSetMode(), FALSE), "on.exit reset clears the mode (also on error paths)")

if (failures > 0)
	stop(failures, " multi-dataset R-logic check(s) failed")

cat("All multi-dataset R-logic checks passed\n")
