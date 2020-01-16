# TODO(Alexander): Make a generic matchAlternative function
# 
hypTwoSided <- c("two.sided", "twoSided", "two-sided", "equal", #General R
                 "notEqualToTestValue", "groupsNotEqual", #t-tests/binomial
                 "correlated") 
hypPlusSided <- c("greater", "plusSided",
                  "greaterThanTestValue", "groupOneGreater", 
                  "correlatedPositively", "right", "positive")
hypMinSided <- c("less", "minSided",
                 "lessThanTestValue", "groupTwoGreater", 
                 "correlatedNegatively", "left", "negative")

.getBfTitle <- function(bfType = c("BF10", "BF01", "LogBF10"),
                                     alternative) {
  bfType <- match.arg(bfType)
  
  if (bfType == "BF10") {
    if (alternative %in% hypTwoSided) {
      bfTitle <- "BF\u2081\u2080"
    } else if (alternative %in% hypPlusSided) {
      bfTitle <- "BF\u208A\u2080"
    } else {
      bfTitle <- "BF\u208B\u2080"
    }
  } else if (bfType == "LogBF10") {
    if (alternative %in% hypTwoSided) {
      bfTitle <- "Log(\u0042\u0046\u2081\u2080)"
    } else if (alternative %in% hypPlusSided) {
      bfTitle <- "Log(\u0042\u0046\u208A\u2080)"
    } else {
      bfTitle <- "Log(\u0042\u0046\u208B\u2080)"
    }
  } else if (bfType == "BF01") {
    if (alternative %in% hypTwoSided) {
      bfTitle <- "BF\u2080\u2081"
    } else if (alternative %in% hypPlusSided) {
      bfTitle <- "BF\u2080\u208A"
    } else {
      bfTitle <- "BF\u2080\u208B"
    }
  }
  return(bfTitle)
}


.bSelectItems <- function(options) {
  itemNames <- c("n", "stat", "bf", "upperCi", "lowerCi")
  
  if (!options[["reportN"]]) 
    itemNames <- setdiff(itemNames, "n")
  
  if (!options[["reportBayesFactors"]]) 
    itemNames <- setdiff(itemNames, "bf")
  
  if (!options[["ci"]])
    itemNames <- setdiff(itemNames, c("lowerCi", "upperCi"))
  
  return(itemNames)
}

.bfFlagKey <- function(options) {
  bfTitle <- .getBfTitle("bfType"=options[["bayesFactorType"]], "alternative"=options[["alternative"]])
  bfKey <- list("LogBF10"=list("*"=paste0(bfTitle, " > log(10)"), 
                               "**"=paste0(bfTitle, " > log(30)"), 
                               "***"=paste0(bfTitle, " > log(100)")),
                "BF01"=list("*"=paste(bfTitle, " < 0.1"), 
                            "**"=paste(bfTitle, " < 0.03"), 
                            "***"=paste(bfTitle, " < 0.01")),
                "BF10"=list("*"=paste(bfTitle, " > 10"), 
                            "**"=paste(bfTitle, " > 30"), 
                            "***"=paste(bfTitle, " > 100"))
  )
  return(bfKey[[options[["bayesFactorType"]]]])
}

.bfFlagTableFootnote <- function(options) {
  keyText <- .bfFlagKey(options)
  flagText <- purrr::map2_chr(c("", "**", "***"), keyText, paste)
  return(paste(flagText, collapse=", "))
}

.getBfTableSidedFootnote <- function(alternative, analysis) {
  # Note(Alexander): Add footnote can't add NULL messages
  # 
  # message <- NULL
  
  if (analysis=="correlation") {
    effectSize <- "correlation"
    if (alternative=="greater") {
      message <- gettextf("For all tests, the alternative hypothesis specifies that the %s is positive.", effectSize)
    } else if (alternative=="less") {
      message <- gettextf("For all tests, the alternative hypothesis specifies that the %s is negative", effectSize)
    }
  }
  
  return(message)
}

.bfPlotTitles <- list(plotScatter         = gettext("Scatterplot"), 
                      plotPriorPosterior  = gettext("Prior and Posterior"), 
                      plotBfRobustness    = gettext("Bayes Factor Robustness Check"), 
                      plotBfSequential    = gettext("Sequential Analysis"))

# if (options[["alternative"]]=="greater") 
#   corBayesTable$addFootnote(message="For all tests, the alternative hypothesis specifies that the correlation is positive.",
#                             symbol="<i>Note</i>.")
# 
# if (options[["alternative"]]=="less") 
#   corBayesTable$addFootnote(message="For all tests, the alternative hypothesis specifies that the correlation is negative.", 
#                             symbol="<i>Note</i>.")