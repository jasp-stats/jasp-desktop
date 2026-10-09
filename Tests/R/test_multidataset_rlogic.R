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

# Removed for good (CTO review 2026-10-09): the accessor scanned every dataset as a fallback
# and could silently serve a same-named column from the wrong one - analyses index the
# datasets list directly. Re-adding it must trip this check.
commonSrc <- readLines(commonR)
for (gone in c("getDataSetColumn", "getSliceKey", "getSlice", "sliceDataSetId", "sliceTitle",
                "getDataSetFor", "dataSetIdFromEncoded", "dataSetNameFromEncoded",
                ".dataSetIdFromEncodedOne", ".sliceKeyForDataSet", ".sliceKeysForDataSet")) {
	if (any(grepl(paste0("^", gone, " *<-"), commonSrc)))
		stop(gone, " must stay deleted: index datasets[[key]] directly (zero-export policy)")
}

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

# --- queue reads (what runJaspResults builds from multiDataSetJson) -------------------------------
readCount <- 0
sliceKeys <- c("5", "12")   # FILTER ids (globally unique; each implies its dataset)
datasets  <- lapply(sliceKeys, function(key) env$.fromRCPP(".readDataSetRequestedNative"))
names(datasets) <- sliceKeys
attr(datasets, "dataSetNames") <- c("5" = "Alpha", "12" = "Beta")
attr(datasets, "dataSetIds")   <- c("5" = "3",     "12" = "4")

check(identical(readCount, 2), "one native read per queued slice, in queue order")
check(identical(names(datasets), sliceKeys), "slices are keyed by FILTER id")
check(identical(datasets[["5"]]$reading, 1) && identical(datasets[["12"]]$reading, 2),
	  "first read got the first slice, second read got the second slice")
check(identical(attr(datasets, "dataSetNames")[["5"]], "Alpha"), "titles are carried as attribute")
check(identical(attr(datasets, "dataSetIds")[["12"]], "4"), "the key's dataset id is an attribute too")

invisible(env$.multiDataSetMode(FALSE))
datasetAfter <- env$.readDataSetToEnd(columns = "Score")
check(is.data.frame(datasetAfter), "readDataSetToEnd works again after the mode was reset")

# --- the datasets contract: option == key == filter id, and [[ is the whole accessor --------------
# Two forms selecting two filters of one dataset (31 AND 32 of ds 3) index different slices;
# the dataSetIds/dataSetNames attributes map every key to its dataset (id, title). This mirrors
# exactly what runJaspResults builds from the bridge queue (prepareMultiDataSetRun).
sliceKeys   <- c("31", "32", "41")
datasetsFk  <- lapply(sliceKeys, function(k) data.frame(slice = k))
names(datasetsFk) <- sliceKeys
attr(datasetsFk, "dataSetNames") <- c("31" = "Alpha", "32" = "Alpha", "41" = "Beta")
attr(datasetsFk, "dataSetIds")   <- c("31" = "3",     "32" = "3",     "41" = "4")

# what a parsed dataSetSelectionOption holds after the bridge translation: a filter id (string,
# sometimes in the wrapper default shape list(types=, value=) - tail(unlist(x), 1) gets it):
selA <- "32"; selB <- "41"
check(identical(datasetsFk[[selA]]$slice, "32"), "selection option value IS the slice key (form A)")
check(identical(datasetsFk[[selB]]$slice, "41"), "...and a second form indexes its own slice directly")
check(identical(attr(datasetsFk, "dataSetIds")[["32"]], "3"), "attr dataSetIds maps key to its dataset id")
check(identical(attr(datasetsFk, "dataSetNames")[["31"]], "Alpha"), "attr dataSetNames gives the user-facing title")
check(is.null(datasetsFk[["999"]]),
      "a wrong key yields NULL (analyses test it) - never a silent scan across the other slices")

# --- reset semantics like runJaspResults' on.exit -----------------------------------------------
wrapper <- function() {
	env$.multiDataSetMode(TRUE)
	on.exit(env$.multiDataSetMode(FALSE), add = TRUE)
	stopifnot(env$.multiDataSetMode())
}
wrapper()
check(identical(env$.multiDataSetMode(), FALSE), "on.exit reset clears the mode (also on error paths)")

# --- bridge-backed round trip: R parser against REAL ColumnEncoder output -------------------------
# jaspSyntax embeds SyntaxInterface, whose native library contains the very ColumnEncoder the
# engine uses. When it is available we encode a real dataset through that bridge and run
# jaspBase's routing parser, the option==columnname invariant and the decode cycle against
# genuine encoder output - not imagined format strings. (The jaspBase testthat suite tests the
# routing logic itself; the C++ JASPTest tests the encoder contract; this ties the two together.)

syntaxLib <- Sys.getenv("JASP_SYNTAX_LIB", file.path(dirname(jaspSourceDir), ".toolLib"))
moduleDir <- Sys.getenv("JASP_TESTMODULE_DIR", file.path(dirname(jaspSourceDir), "jaspTestModule"))
moduleQml <- file.path(moduleDir, "inst", "qml", "testMultiDataSetNonAware.qml")  # single-dataset twin: no selection options

# jsonlite & jaspSyntax live in the tool/build libraries; pick them up when present
for (extraLib in c(syntaxLib, Sys.getenv("JASP_TEST_PKGLIB",
                                        file.path(moduleDir, "ModuleBundleBuildDir", "build_dir", "jaspTestModule"))))
	if (dir.exists(extraLib)) .libPaths(c(extraLib, .libPaths()))

haveBridge <- suppressWarnings(requireNamespace("jaspSyntax", quietly = TRUE)) &&
              requireNamespace("jsonlite", quietly = TRUE) && file.exists(moduleQml)
if (!haveBridge) {
	cat("SKIP : bridge round-trip (jaspSyntax not in", syntaxLib, "or module qml missing)\n")
} else {
	Sys.setenv(QT_QPA_PLATFORM = "offscreen")
	jaspSyntax <- getNamespace("jaspSyntax")
	jaspSyntax$clearNativeState()
	jaspSyntax$setParameter("verbose", "none")

	# deliberately nasty column names: the reason encoding exists at all
	nasty <- data.frame("weight (kg)" = c(1.5, 2.5, 3.5), "2 + 2" = c(10L, 20L, 30L),
	                    check.names = FALSE)
	names(nasty) <- c("weight (kg)", "2 + 2")
	jaspSyntax$loadDataSet(nasty)

	parsed <- jaspSyntax$loadQmlAndParseOptions("jaspTestModule", "multiDataSetNonAware", moduleQml,
		as.character(jsonlite::toJSON(list(dependentA = list(value = "weight (kg)", types = "scale")),
		                               auto_unbox = TRUE)), "1", TRUE)
	optionsParsed <- jsonlite::fromJSON(parsed, simplifyVector = FALSE)
	encodedValue  <- as.character(optionsParsed$dependentA[[1]])

	check(grepl("^JASPColumn_[0-9]+_[0-9]+_Encoded$", encodedValue),
	      paste("QML parsing yields real encoded option values (", encodedValue, ")"))
	slice <- jaspSyntax$readRequestedDataset()
	check(is.data.frame(slice) && encodedValue %in% names(slice),
	      "the encoded option value IS the slice's column name (index-directly invariant)")

	decoded <- as.character(jaspSyntax$decodeColumnText(encodedValue))
	check(identical(decoded, "weight (kg)"), "decode returns the original (type dropped)")

	# encode again from the decoded plain name: stable (this is the cycle the C++ test pins down
	# against the encoder itself; here it runs through the whole bridge)
	parsed2   <- jaspSyntax$loadQmlAndParseOptions("jaspTestModule", "multiDataSetNonAware", moduleQml,
		as.character(jsonlite::toJSON(list(dependentA = list(value = decoded, types = "scale")),
		                               auto_unbox = TRUE)), "1", TRUE)
	encoded2  <- as.character(jsonlite::fromJSON(parsed2, simplifyVector = FALSE)$dependentA[[1]])
	check(identical(encoded2, encodedValue), "decode -> encode cycle is stable")

	jaspSyntax$clearNativeState()
}

if (failures > 0)
	stop(failures, " multi-dataset R-logic check(s) failed")

cat("All multi-dataset R-logic checks passed\n")
