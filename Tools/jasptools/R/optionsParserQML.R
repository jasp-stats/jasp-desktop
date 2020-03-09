.readQML <- function(file) {
  regularFields <- c(
    "IntegerField",
    "DoubleField",
    "PercentField",
    "CIField",
    "TextField",
    "TextArea",
    "CheckBox",
    "Slider",
    "AssignedVariablesList",
    "repeatedMeasuresFactorsList",
    "Chi2TestTableView",
    "DropDown"
  ) # the button group requires additional parsing

  ignoreWhenParsingRegularFields <- c(
    "RadioButtonGroup",
    "VariablesForm",
    "RadioButton",
    "ExpanderButton",
    "BayesFactorType",
    "SubjectivePriors",
    "ContrastsList",
    "SetSeed",
    "LD.LDOptions",
    "LD.LDGenerateDisplayData"
  )

  fileSize <- file.info(file)$size 
  fileContents <- readChar(file, nchars=fileSize)
  fileContents <- gsub("//.*?(\\r\\n|\\r|\\n)", "", fileContents) # remove comments
  fileContents <- gsub('[[:blank:]]|\\"', "", fileContents) # strip whitespaces
  fileContents <- gsub("(\\r\\n|\\r|\\n)", ";", fileContents) # replace newline characters with ;
  fileContents <- gsub("^.*?Form;*?\\{", "", fileContents) # remove everything up to the actual form
  fileContents <- gsub("(?<={);+|;(?={)|(?<=});+|;(?=})", "", fileContents, perl=TRUE) # remove all ; around {}
  fileContents <- gsub("(?<=\\[);+|;(?=\\[)|(?<=\\]);+|;(?=\\])", "", fileContents, perl=TRUE) # remove all ; around []

  regExpr <- paste0("(", regularFields, "\\{.*?)(?=", paste0(c(regularFields, ignoreWhenParsingRegularFields, "$"), collapse="|"), ")", collapse="|")
  qmlElements <- stringr::str_extract_all(fileContents, regExpr)[[1]]
  fileContents <- stringr::str_replace_all(fileContents, regExpr, "") # remove everything we can readily use

  regExpr <- "RadioButtonGroup\\{.*?(?=RadioButtonGroup\\{|$)"
  qmlElements <- c(qmlElements, stringr::str_extract_all(fileContents, regExpr)[[1]])

  options <- .qmlElementsToOptionsList(qmlElements)
  optionsOfStaticElements <- .staticElementsToOptions(fileContents)
  options <- c(options, optionsOfStaticElements)
  return(options)
}


.qmlElementsToOptionsList <- function(qmlElements) {
  opts <- list()
  for (qmlElement in qmlElements) {
    opts <- c(opts, extractData(qmlElement))
  }
  return(opts)
}


.staticElementsToOptions <- function(fileContents) {
  result <- list()
  
  result[["plotWidth"]] <- 480
  result[["plotHeight"]] <- 320

  regMatch <- "BayesFactorType\\{"
  if (grepl(regMatch, fileContents)) {
    result[["bayesFactorType"]] <- "BF10"
  }
  
  regMatch <- "ContrastsList\\{"
  if (grepl(regMatch, fileContents)) {
    result[["contrast"]] <- "none"
  }
  
  regMatch <- "SetSeed\\{"
  if (grepl(regMatch, fileContents)) {
    result[["setSeed"]] <- FALSE
    result[["seed"]] <- 1
  }
  
  regMatch <- "SubjectivePriors\\{"
  if (grepl(regMatch, fileContents)) {
    subjectivePriors <- list(
      priorWidth = 0.707,
      informativeCauchyLocation = 0,
      informativeCauchyScale = 0.707,
      informativeNormalMean = 0,
      informativeNormalStd = 0.707,
      informativeTLocation = 0,
      informativeTScale = 0.707,
      informativeTDf = 1,
      uniformDienesLowerBound = 0.707,
      uniformDienesUpperBound = 0.707,
      halfNormalDienesStd = 0.707,
      normalDienesMean = 0.707,
      normalDienesStd = 0.707,
      effectSize = "standardized",
      effectSizeStandardized = "default",
      defaultStandardizedEffectSize = "cauchy",
      informativeStandardizedEffectSize = "cauchy",
      dienesEffectSize = "uniform"
    )
    result <- c(result, subjectivePriors)
  }
  
  regMatch <- "LD.LDOptions\\{"
  if (grepl(regMatch, fileContents)) {
    LDoption <- .parseLDOption(fileContents)
    result <- c(result, LDoption)
  }
  
  regMatch <- "LD.LDGenerateDisplayData\\{"
  if (grepl(regMatch, fileContents)) {
    LDGenerateDisplayData <- list(
      sampleSize = 1,
      simulateNow = FALSE,
      variable = c(),
      summary = TRUE,
      moments = FALSE,
      momentsUpTo = 2,
      histogram = TRUE,
      histogramBins = 30,
      ecdf = FALSE
    )
    result <- c(result, LDGenerateDisplayData)
  }
  
  return(result)
}

.makeExprForOptionParam <- function(param) {
  return(paste0("[\\{;]", param, ":(.*?)[;\\}]"))
}

.optionHasParam <- function(option, param) {
  expr <- .makeExprForOptionParam(param)
  return(grepl(expr, option))
}

.getOptionParamValue <- function(option, param, default = NULL) {
  value <- default
  if (.optionHasParam(option, param)) {
    match <- stringr::str_match(option, .makeExprForOptionParam(param))[2]
    if (tolower(match) == "true")
      value <- TRUE
    else if (tolower(match) == "false")
      value <- FALSE
    else if (!is.na(suppressWarnings(as.numeric(match))))
      value <- as.numeric(match)
    else
      value <- match
  }
  return(value)
}

.parseLDOption <- function(fileContents) {
  LDOption <- stringr::str_extract(fileContents, "LDOptions\\{.*?\\}")
  
  negativeValues <- .getOptionParamValue(LDOption, "negativeValues", default = TRUE)
  min            <- .getOptionParamValue(LDOption, "min",            default = ifelse(negativeValues, -Inf, 0))
  max            <- .getOptionParamValue(LDOption, "max",            default = Inf)
  
  return(list(
    min_x                = .getOptionParamValue(LDOption, "rangeMinX",         default = ifelse(min == -Inf, -3, min)),
    max_x                = .getOptionParamValue(LDOption, "rangeMaxX",         default = ifelse(max == Inf, 3, max)),
    min                  = .getOptionParamValue(LDOption, "intervalMinmaxMin", default = 0),
    max                  = .getOptionParamValue(LDOption, "intervalMinmaxMax", default = 1),
    lower_max            = .getOptionParamValue(LDOption, "intervalLowerMax",  default = 0),
    upper_min            = .getOptionParamValue(LDOption, "intervalUpperMin",  default = 0),
    highlightDensity     = FALSE,
    highlightProbability = FALSE,
    highlightType        = "minmax"
  ))
}


extractData <- function (element, ...) {
  regMatch <- "^(.*?)\\{.*?[\\};]"
  fieldClassTable <- stringr::str_match(element, regMatch)
  if (length(fieldClassTable) != 2) {
    stop("Could not locate type of the field")
  }
  fieldClass <- fieldClassTable[, 2]
  class(element) <- fieldClass
  UseMethod("extractData", element)
}


extractData.IntegerField <- function(element, defaultValue = 0) {
  regMatch <- "default.*?:([+-]?([0-9]*[.])?[0-9]+)"
  matchTable <- stringr::str_match(element, regMatch)
  default <- as.numeric(matchTable[2])
  if (is.na(default)) {
    default <- defaultValue
  }
  extractData.default(element, default)
}


extractData.DoubleField <- function(element, defaultValue = 0) {
  extractData.IntegerField(element, defaultValue)
}


extractData.PercentField <- function(element, defaultValue = 50) {
  regMatch <- "default.*?:([+-]?([0-9]*[.])?[0-9]+)"
  matchTable <- stringr::str_match(element, regMatch)
  default <- as.numeric(matchTable[2])
  if (is.na(default)) {
    default <- defaultValue
  }
  default <- default / 100
  extractData.default(element, default)
}


extractData.CIField <- function(element, defaultValue = 95) {
  extractData.PercentField(element, defaultValue)
}


extractData.AssignedVariablesList <- function(element) {
  regMatch <- "name:(.*?)[;\\}]"
  matchTable <- stringr::str_match(element, regMatch)
  name <- matchTable[2]
  if (is.na(name)) {
    name <- "variables" # default behaviour of variablesform.qml
  }

  result <- list()
  regSingleItem <- "singleVariable:true"
  isSingleItem <- grepl(regSingleItem, element)
  if (isSingleItem) {
    result[[name]] <- ""
  } else {
    result[[name]] <- list()
  }
  return(result)
}

extractData.repeatedMeasuresFactorsList <- function(element) {
  return(extractData.AssignedVariablesList(element))
}

extractData.CheckBox <- function(element) {
  regMatch <- "checked.*?:(true)"
  matchTable <- stringr::str_match(element, regMatch)
  if (is.na(matchTable[2])) {
    checked <- FALSE
  } else {
    checked <- TRUE
  }
  extractData.default(element, checked)
}

extractData.Chi2TestTableView <- function(element) {
  extractData.default(element, list())
}


extractData.Slider <- function(element) {
  regMatch <- "value.*?:(\\d+)"
  matchTable <- stringr::str_match(element, regMatch)
  value <- as.numeric(matchTable[2])
  extractData.default(element, value)
}


extractData.DropDown <- function(element) {
  regMatches <- c("indexDefaultValue:(\\d+)", "values:\\[(?!\\{)(.*?)\\]", "values:\\[(.*?)\\]")
  matchTable <- stringr::str_match_all(element, regMatches)
  
  unnamedArray <- matchTable[[2]][2]
  if (!is.na(unnamedArray)) {
    values <- unlist(strsplit(unnamedArray, ","))
  } else {
    namedArray <- matchTable[[3]][2]
    regMatchValue <- "value:(.*?)[;\\}]"
    matchTableValues <- stringr::str_match_all(namedArray, regMatchValue)
    values <- matchTableValues[[1]][, 2]
  }
  
  index <- as.numeric(matchTable[[1]][2])
  if (!is.na(index)) {
    index <- index + 1 # zero based qml vs one based R array
  } else {
    index <- 1
  }
  
  value <- NA
  if (length(values) >= index) {
    value <- values[index]
  } else {
    stop("Index value exceeds array size for element", element)
  }

  extractData.default(element, value)
}


extractData.RadioButtonGroup <- function(element) {
  regMatch <- "RadioButton\\{[^\\}]*?checked:true"
  matchTable <- stringr::str_match(element, regMatch)
  regMatchValue <- "value:(.*?)[;\\}]"
  matchTableValue <- stringr::str_match(matchTable, regMatchValue)
  value <- matchTableValue[2]
  extractData.default(element, value)
}


extractData.default <- function(element, value = NA) {
  regMatch <- "name:(.*?)[;\\}]"
  matchTable <- stringr::str_match(element, regMatch)
  name <- matchTable[2]
  
  result <- NULL
  if (!is.na(name)) {
    result <- list()
    result[[name]] <- ""
    if (!identical(value, NA)) {
      result[[name]] <- value
    }
  }
  return(result)
}
