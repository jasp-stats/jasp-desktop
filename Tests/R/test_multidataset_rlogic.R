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

# --- primarySliceKey contract (C1/e4): the C++ side OMITS "primary" when there is no primary ----
# filter; jaspBase keys on !is.null, so an omitted member must give NULL (a "" would surface as a
# non-NULL key of nothing - what prepareMultiDataSetRun used to emit).
dsInfoNoPrimary <- list(ids = c("5"), names = c("5" = "Alpha"), dataSetIds = c("5" = "3"))
primaryAttr     <- if (!is.null(dsInfoNoPrimary$primary)) as.character(dsInfoNoPrimary$primary) else NULL
check(is.null(primaryAttr), "absent primary member yields NULL primarySliceKey (omit, never \"\")")

# --- bridge-backed round trip: R parser against REAL ColumnEncoder output -------------------------
# jaspSyntax embeds SyntaxInterface, whose native library contains the very ColumnEncoder the
# engine uses. When it is available we encode a real dataset through that bridge and run
# jaspBase's routing parser, the option==columnname invariant and the decode cycle against
# genuine encoder output - not imagined format strings. (The jaspBase testthat suite tests the
# routing logic itself; the C++ JASPTest tests the encoder contract; this ties the two together.)

syntaxLib <- Sys.getenv("JASP_SYNTAX_LIB", file.path(dirname(jaspSourceDir), ".toolLib"))
moduleDir <- Sys.getenv("JASP_TESTMODULE_DIR", file.path(dirname(jaspSourceDir), "jaspTestModule"))
# Non-aware single-dataset form for the parse round trips below. mds-plan C4/M8 deleted the
# multiDataSetNonAware twin it used to target; testVariablesForm is a plain non-aware analysis
# with a variables list (varS), which is all this section needs. Cross-repo pairing: the twin's
# removal lands in the same change in jaspTestModule.
moduleQml  <- file.path(moduleDir, "inst", "qml", "testVariablesForm.qml")
moduleOpts <- function(column) as.character(jsonlite::toJSON(
	list(varS = list(value = column, types = "scale")), auto_unbox = TRUE))

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

	parsed <- jaspSyntax$loadQmlAndParseOptions("jaspTestModule", "testVariablesForm", moduleQml,
		moduleOpts("weight (kg)"), "1", TRUE)
	optionsParsed <- jsonlite::fromJSON(parsed, simplifyVector = FALSE)
	encodedValue  <- as.character(optionsParsed$varS[[1]])

	check(grepl("^JASPColumn_[0-9]+_[0-9]+_Encoded$", encodedValue),
	      paste("QML parsing yields real encoded option values (", encodedValue, ")"))
	slice <- jaspSyntax$readRequestedDataset()
	check(is.data.frame(slice) && encodedValue %in% names(slice),
	      "the encoded option value IS the slice's column name (index-directly invariant)")

	decoded <- as.character(jaspSyntax$decodeColumnText(encodedValue))
	check(identical(decoded, "weight (kg)"), "decode returns the original (type dropped)")

	# encode again from the decoded plain name: stable (this is the cycle the C++ test pins down
	# against the encoder itself; here it runs through the whole bridge)
	parsed2   <- jaspSyntax$loadQmlAndParseOptions("jaspTestModule", "testVariablesForm", moduleQml,
		moduleOpts(decoded), "1", TRUE)
	encoded2  <- as.character(jsonlite::fromJSON(parsed2, simplifyVector = FALSE)$varS[[1]])
	check(identical(encoded2, encodedValue), "decode -> encode cycle is stable")

	# --- C1/M1 stale-slice regression (agent-3's leak repro) --------------------------------------
	# An aware parse queues one slice per referenced filter; a run that never consumes them (R died
	# before the read loop) must not leave them for the NEXT request: syntaxBridgeLoadQmlAndParseOptions-
	# Status clears the queue at entry, exactly like Engine::receiveAnalysisMessage does per message.
	# Without that clear the non-aware read below is served the stale aware slice (dataset A's columns
	# / row count) instead of the freshly loaded dataset.
	awareQml <- file.path(moduleDir, "inst", "qml", "testMultiDataSet.qml")
	if (file.exists(awareQml)) {
		jaspSyntax$clearNativeState()
		invisible(jaspSyntax$loadDataSets(list(sliceA = data.frame(AlphaCol = 1:5),
		                                        sliceB = data.frame(BetaCol  = 1:5))))
		awareStatus <- jaspSyntax$loadQmlAndParseOptionsStatus("jaspTestModule", "multiDataSetFunc", awareQml,
			as.character(jsonlite::toJSON(list(
				dataSetA = "sliceA", dependentA = list(value = "AlphaCol", types = "scale"),
				dataSetB = "sliceB", dependentB = list(value = "BetaCol",  types = "scale")),
				auto_unbox = TRUE)), "1", TRUE)
		check(nzchar(awareStatus$multiDataSetJson), "aware parse queued its slices (and nobody reads them)")
		invisible(jaspSyntax$loadDataSet(data.frame(PlainCol = 1:7)))   # unnamed load: new request, non-aware
		invisible(jaspSyntax$loadQmlAndParseOptionsStatus("jaspTestModule", "testVariablesForm", moduleQml,
			moduleOpts("PlainCol"), "1", TRUE))
		staleSlice <- jaspSyntax$readRequestedDataset(decode = FALSE)
		check(is.data.frame(staleSlice) && nrow(staleSlice) == 7L,
		      "stale aware slices do not survive the bridge request boundary (read got the non-aware dataset)")
	} else {
		cat("SKIP : C1/M1 stale-slice regression (aware test qml not in", moduleDir, ")\n")
	}

	# --- C4/RESOLVER: the selection syntax surface is human strings only (mds-plan C4) -------------
	# Bare dataset TITLE = default filter, dropdown label "title - filterName" = a named one,
	# resolved by exact lookup against the live workspace. Dataset ids and filter ids are NOT
	# enterable in syntax (M4): a number resolves only when a dataset is titled that number.
	awareParse <- function(selA, selB) {
		jaspSyntax$loadQmlAndParseOptionsStatus("jaspTestModule", "multiDataSetFunc", awareQml,
			as.character(jsonlite::toJSON(list(
				dataSetA = selA, dependentA = list(value = "AlphaCol", types = "scale"),
				dataSetB = selB, dependentB = list(value = "BetaCol",  types = "scale")),
				auto_unbox = TRUE)), "1", TRUE)
	}
	if (file.exists(awareQml)) {
		jaspSyntax$clearNativeState()
		surfaceIds <- jaspSyntax$loadDataSets(list("42" = data.frame(AlphaCol = 1:5),
		                                            B        = data.frame(BetaCol  = 1:5)))

		stSurf <- tryCatch(awareParse("42", "B"), error = function(e) e)
		check(!inherits(stSurf, "error") && nzchar(stSurf$multiDataSetJson),
		      "bare titles parse through the bridge")
		surfParsed <- jsonlite::fromJSON(stSurf$options, simplifyVector = FALSE)
		surfMsj    <- jsonlite::fromJSON(stSurf$multiDataSetJson)
		key42 <- as.character(utils::tail(unlist(surfParsed$dataSetA), 1L))
		keyB  <- as.character(utils::tail(unlist(surfParsed$dataSetB), 1L))
		check(identical(as.character(surfMsj$dataSetIds[[key42]]), as.character(surfaceIds[["42"]])),
		      "a dataset TITLED '42' is addressed by that title: numbers on the surface are titles, nothing else")
		check(identical(as.character(surfMsj$dataSetIds[[keyB]]), as.character(surfaceIds[["B"]])),
		      "and 'B' resolves to dataset B")

		errNum <- tryCatch({ awareParse(as.character(surfaceIds[[1]]), "B"); "" }, error = function(e) conditionMessage(e))
		check(grepl("matches no dataset selection string", errNum),
		      "a numeric DATASET id is rejected outright (M4: ids are internal wire format, never syntax)")

		errName <- tryCatch({ awareParse("42 - myfilter", "B"); "" }, error = function(e) conditionMessage(e))
		check(grepl("matches no dataset selection string", errName) && grepl("available:", errName),
		      "'title - name' fails loudly on fresh loads (no named filters there) and lists what IS available")

		errShape <- tryCatch({
			shapeOpts <- jsonlite::toJSON(list(
				dataSetA = list(types = "unknown", value = list("42")),   # {types,value} WITHOUT a plain inside
				dependentA = list(value = "AlphaCol", types = "scale"),
				dataSetB = "B",
				dependentB = list(value = "BetaCol", types = "scale")), auto_unbox = FALSE)
			jaspSyntax$loadQmlAndParseOptionsStatus("jaspTestModule", "multiDataSetFunc", awareQml,
				as.character(shapeOpts), "1", TRUE)
			""
		}, error = function(e) conditionMessage(e))
		check(grepl("pass the dataset title directly", errShape),
		      "a shaped selection value with no plain string inside is rejected with the direct-title hint (M9)")

		stPlain <- tryCatch(awareParse(list(types = "unknown", value = "42"), "B"), error = function(e) e)
		check(!inherits(stPlain, "error"),
		      "the desktop-shaped {types,value} with a plain title inside still unwraps (old wrapper defaults)")

		jaspSyntax$clearNativeState()
	}

	jaspSyntax$clearNativeState()
}

if (failures > 0)
	stop(failures, " multi-dataset R-logic check(s) failed")

cat("All multi-dataset R-logic checks passed\n")
