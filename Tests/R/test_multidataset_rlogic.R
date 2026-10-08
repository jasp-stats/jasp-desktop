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
            "readDataSetByVariableTypes",
            ".dataSetIdFromEncodedOne", "dataSetIdFromEncoded", "dataSetNameFromEncoded",
            ".sliceKeyForDataSet", ".sliceKeysForDataSet", "getDataSetFor", "getDataSetColumn",
             "getSliceKey", "getSlice", "sliceDataSetId", "sliceTitle")

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

# --- filter-keyed slices (C8/T5): engine queue keys are FILTER ids, one per distinct slice -------
# Mirrors what runJaspResults now builds from multiDataSetJson: ids = filter ids, and the
# dataSetIds/dataSetNames attributes map every slice key back to its dataset (id, title).
sliceKeys   <- c("31", "32", "41")            # dataset 3 via filters 31 AND 32 (two slices!), dataset 4 via 41
datasetsFk  <- lapply(sliceKeys, function(k) data.frame(slice = k))
names(datasetsFk) <- sliceKeys
attr(datasetsFk, "dataSetNames") <- c("31" = "Alpha", "32" = "Alpha", "41" = "Beta")
attr(datasetsFk, "dataSetIds")   <- c("31" = "3",     "32" = "3",     "41" = "4")

check(identical(env$.sliceKeyForDataSet(datasetsFk, "4"), "41"), ".sliceKeyForDataSet maps dataset id to its filter slice")
errAmb <- tryCatch({ env$.sliceKeyForDataSet(datasetsFk, "3"); "" }, error = function(e) conditionMessage(e))
check(grepl("filtered slices", errAmb), "two slices of one dataset FAIL LOUDLY, never silently first")
check(identical(env$.sliceKeyForDataSet(datasetsFk, "99"), NULL), "...and absent datasets resolve to NULL")
errGet <- tryCatch({ env$getDataSetFor("JASPColumn_3_7_Encoded", datasetsFk); "" }, error = function(e) conditionMessage(e))
check(grepl("filtered slices", errGet), "getDataSetFor refuses the ambiguity too")
check(identical(env$getDataSetFor("JASPColumn_4_2_Encoded", datasetsFk)$slice, "41"), "unambiguous encoded routing works")
check(identical(env$dataSetNameFromEncoded("JASPColumn_4_2_Encoded", datasetsFk), "Beta"),
      "title lookup works through the filter-keyed attrs")

# public slice API (what analysis writers get):
opts <- list(dataSetA = "32", dataSetB = list(types = "unknown", value = ""))
check(identical(env$getSliceKey(opts, datasetsFk, "dataSetA"), "32"),
      "a selection option (filter id) indexes its own slice directly - the option==slicekey invariant")
check(identical(env$getSliceKey(opts, datasetsFk, "dataSetB", column = "JASPColumn_4_2_Encoded"), "41"),
      "an empty selection falls back to the column's dataset when unambiguous")
errFb <- tryCatch({ env$getSliceKey(opts, datasetsFk, "dataSetB", column = "JASPColumn_3_7_Encoded"); "" }, error = function(e) conditionMessage(e))
check(grepl("getSliceKey", errFb), "ambiguous fallback errors and names getSliceKey as the fix")
check(identical(env$sliceDataSetId(datasetsFk, "32"), "3"), "sliceDataSetId maps slice to dataset")
check(identical(env$sliceTitle(datasetsFk, "31"), "Alpha"), "sliceTitle gives the user-facing title")
errSl <- tryCatch({ env$getSlice(datasetsFk, "999"); "" }, error = function(e) conditionMessage(e))
check(grepl("no slice", errSl), "getSlice errors listing the available keys")

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
	check(!is.na(env$dataSetIdFromEncoded(encodedValue)),
	      "jaspBase's parser recovers a dataset id from genuine encoder output")

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
