.analysisOptionsFromQMLFile <- function(analysis) {
  file <- .pathToOptionsFile(analysis, "qml")
  options <- .readQML(file)
  return(options)
}


.readQML <- function(file) {
  regularFields <- c(
    "IntegerField",
    "DoubleField",
    "PercentField",
    "TextField",
    "CheckBox",
    "Slider",
    "AssignedVariablesList",
    "repeatedMeasuresFactorsList",
    "DropDown"
  ) # the button group requires additional parsing

  ignoreWhenParsingRegularFields <- c(
    "RadioButtonGroup",
    "VariablesForm",
    "RadioButton",
    "ExpanderButton",
    "BayesFactorType",
    "SubjectivePriors"
  )

  fileSize <- file.info(file)$size 
  fileContents <- readChar(file, nchars=fileSize)
  fileContents <- gsub("//.*?(\\r\\n|\\r|\\n)", "", fileContents) # remove comments
  fileContents <- gsub('[[:blank:]]|\\"', "", fileContents) # strip whitespaces
  fileContents <- gsub("(\\r\\n|\\r|\\n)", ";", fileContents) # replace newline characters with ;
  fileContents <- gsub("^.*?Form;*?\\{", "", fileContents) # remove everything up to the actual form
  fileContents <- gsub("(?<={);+|;(?={)|(?<=});+|;(?=})", "", fileContents, perl=TRUE) # remove all ; around {}

  regExpr <- paste0("(", regularFields, "\\{.*?)(?=", paste0(c(regularFields, ignoreWhenParsingRegularFields, "$"), collapse="|"), ")", collapse="|")
  qmlElements <- stringr::str_extract_all(fileContents, regExpr)[[1]]
  fileContents <- stringr::str_replace_all(fileContents, regExpr, "") # remove everything we can readily use

  regExpr <- "RadioButtonGroup\\{.*?(?=RadioButtonGroup\\{|$)"
  qmlElements <- c(qmlElements, stringr::str_extract_all(fileContents, regExpr)[[1]])

  options <- .qmlElementsToOptionsList(qmlElements)
  optionsOfStaticElements <- .staticElementsToOptions(fileContents)
  options <- c(optionsOfStaticElements, options)
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

  regMatch <- "BayesFactorType\\{\\}"
  if (grepl(regMatch, fileContents)) {
    result[["bayesFactorType"]] <- "BF10"
  }
  
  regMatch <- "SubjectivePriors\\{\\}"
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
  
  return(result)
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


extractData.IntegerField <- function(element) {
  regMatch <- "default.*?:([+-]?([0-9]*[.])?[0-9]+)"
  matchTable <- stringr::str_match(element, regMatch)
  default <- as.numeric(matchTable[2])
  extractData.default(element, default)
}


extractData.DoubleField <- function(element) {
  extractData.IntegerField(element)
}


extractData.PercentField <- function(element) {
  regMatch <- "default.*?:([+-]?([0-9]*[.])?[0-9]+)"
  matchTable <- stringr::str_match(element, regMatch)
  default <- as.numeric(matchTable[2]) / 100
  extractData.default(element, default)
}


extractData.AssignedVariablesList <- function(element) {
  regMatch <- "name:(.*?)[;\\}]"
  matchTable <- stringr::str_match(element, regMatch)
  name <- matchTable[2]
  if (is.na(name)) {
    name <- "variables" # default behaviour of variablesform.qml
  }

  result <- list()
  regSingleItem <- "singleItem:true"
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


extractData.Slider <- function(element) {
  regMatch <- "value.*?:(\\d+)"
  matchTable <- stringr::str_match(element, regMatch)
  value <- as.numeric(matchTable[2])
  extractData.default(element, value)
}


extractData.DropDown <- function(element) {
  regMatches <- c("indexDefaultValue:(\\d+)", "values:\\[(.*?)\\]", "ListElement\\{.*?value:(.*?)\\}")
  matchTable <- stringr::str_match_all(element, regMatches)

  model <- matchTable[[2]][2]
  if (!is.na(model)) {
    values <- unlist(strsplit(model, ","))
  } else {
    values <- matchTable[[3]][, 2]
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
    if (!is.na(value)) {
      result[[name]] <- value
    }
  }
  return(result)
}
