#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' The plan of this file is to successively abstract functionality
#' from the individual t-tests into a common interface to reduce clutter
#'
.ttestReadData <- function(dataset, options, type) {
  if (!is.null(dataset))
    return(dataset)
  else {
    groups  <- options$groupingVariable
    if (!is.null(groups) && groups == "") 
      groups <- NULL
    if(type %in% c("one-sample", "independent"))
      depvars <- unlist(options$variables)
    else if (type == 'paired') {
      depvars <- unlist(options$pairs)
      depvars <- depvars[depvars != ""]
    }
    exclude <- NULL
    if (options$missingValues == "excludeListwise")
      exclude <- depvars
    return(.readDataSetToEnd(columns.as.numeric  = depvars,
                             columns.as.factor   = groups,
                             exclude.na.listwise = exclude))
  }
}

.ttestCheckErrors <- function(dataset, options, type) {
  if(type == "paired")
    for (pair in options$pairs) {
      if(pair[[1]] == "" || pair[[2]] == "")
        next
      p1 <- pair[[1]]
      p2 <- pair[[2]]
      if(is.null(p1) || is.null(p2))
        return()
      datasetErrorCheck <- data.frame(dataset[[.v(p1)]] - dataset[[.v(p2)]])
      colnames(datasetErrorCheck) <- .v(paste0("Difference between ", p1, " and ", p2))
      .hasErrors(datasetErrorCheck, 
                 type = "variance", 
                 exitAnalysisIfErrors = TRUE)
    }
  else if(type == "independent") {
    if (length(options$variables) != 0 && options$groupingVariable != '')
      .hasErrors(dataset,
                 type = 'factorLevels',
                 factorLevels.target  = options$groupingVariable, 
                 factorLevels.amount  = '!= 2',
                 exitAnalysisIfErrors = TRUE)
      }
}

.ttestOptionsList <- function(options, type){
  optionsList <- list()
  optionsList$wantsEffect     <- options$effectSize
  optionsList$wantsConfidenceEffSize <- (options$effSizeConfidenceIntervalCheckbox && options$effectSize)
  optionsList$wantsStudents   <- options$students
  optionsList$wantsDifference <- options$meanDifference
  optionsList$wantsConfidenceMeanDiff <- (options$meanDiffConfidenceIntervalCheckbox && options$meanDifference)
  
  if(type == "paired") {
    optionsList$wantsWilcox <- options$wilcoxonSignedRank
    optionsList$whichTests  <- c("Student", "Wilcoxon")[c(optionsList$wantsStudents, optionsList$wantsWilcox)]
  }
  else if(type == "one-sample"){
    optionsList$wantsZtest  <- options$zTest
    optionsList$wantsWilcox <- options$mannWhitneyU
    optionsList$whichTests  <- c("Student", "Wilcoxon", "Z")[c(optionsList$wantsStudents, optionsList$wantsWilcox, optionsList$wantsZtest)]
  }
  optionsList$wantsConfidenceEffSize    <- (options$effSizeConfidenceIntervalCheckbox && options$effectSize)
  if(type %in% c("paired", "one-sample")) {
    optionsList$percentConfidenceEffSize  <- options$effSizeConfidenceIntervalPercent
    optionsList$percentConfidenceMeanDiff <- options$meanDiffConfidenceIntervalPercent
  } else if(type == "independent") {
    optionsList$wantsWelchs <- options$welchs
    optionsList$wantsWilcox <- options$mannWhitneyU
    optionsList$whichTests  <- c("Student", "Welch", "Mann-Whitney")[c(optionsList$wantsStudents, optionsList$wantsWelchs, optionsList$wantsWilcox)]
    optionsList$percentConfidenceEffSize  <- options$descriptivesEffectSizeConfidenceIntervalPercent
    optionsList$percentConfidenceMeanDiff <- options$descriptivesMeanDiffConfidenceIntervalPercent
  }
  optionsList$allTests <- c(optionsList$wantsStudents, optionsList$wantsWilcox)
  if(type == "one-sample")
    optionsList$allTests <- c(optionsList$allTests, optionsList$wantsZtest)
  if(type == "independent")
    optionsList$allTests <- c(optionsList$allTests, optionsList$wantsWelchs)
  optionsList$onlyTest <- sum(optionsList$allTests) == 1  
  
  return(optionsList)
}

.ttestRowIsNewGroup <- function(test, tests) {
  return(length(tests) > 1 && test == tests[1])
}

.ttestAssumptionCheckContainer <- function(jaspResults, options, type) {
  if(type == "independent")
    if(!options$normalityTests && !options$equalityOfVariancesTests)
      return()
  else if (type %in% c("paired", "one-sample"))
    if(!options$normalityTests)
      return()
  if (is.null(jaspResults[["AssumptionChecks"]])) {
    container <- createJaspContainer(gettext("Assumption Checks"))
    dependList <- c("variables", "groupingVariable", "pairs", "missingValues",
                    "normalityTests", "equalityOfVariancesTests")
    container$dependOn(dependList)
    container$position <- 2
    jaspResults[["AssumptionChecks"]] <- container
  }
}

.ttestDescriptivesContainer <- function(jaspResults, options) {
  if(!options$descriptives && !options$descriptivesPlots) return()
  if (is.null(jaspResults[["ttestDescriptives"]])) {
    container <- createJaspContainer(gettext("Descriptives"))
    container$dependOn(c("descriptives", "descriptivesPlots", 
                         "descriptivesPlotsConfidenceInterval", "missingValues",
                         "variables", "pairs", "groupingVariable"))
    container$position <- 3
    jaspResults[["ttestDescriptives"]] <- container
  }
}

.ttestVovkSellke <- function(table, options) {
  if (options$VovkSellkeMPR) {
    message <-"Vovk-Sellke Maximum <em>p</em>-Ratio: Based on a two-sided <em>p</em>-value, 
    the maximum possible odds in favor of H\u2081 over H\u2080 equals 1/(-e <em>p</em> log(<em>p</em>)) 
    for <em>p</em> \u2264 .37 (Sellke, Bayarri, & Berger, 2001)."
    table$addFootnote(message, symbol = "\u002A")
    table$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number")
  }
}

.ttestMainGetDirection <- function(hypothesis) {
  if (hypothesis == "groupOneGreater")
    return("greater")
  else if (hypothesis == "groupTwoGreater")
    return("less")
  else
    return("two.sided")
}

.confidenceLimitsEffectSizes <- function(ncp, df, conf.level = .95, alpha.lower = NULL, 
                                         alpha.upper = NULL, t.value, tol = 1e-9, ...) {
  # This function comes from the MBESS package, version 4.6, by Ken Kelley
  # https://cran.r-project.org/web/packages/MBESS/index.html
  # Note this function is new in version 4, replacing what was used in prior versions.
  # Internal functions for the noncentral t distribution; two appraoches.
  ###########
  
  
  # General stop checks.
  if(!is.null(conf.level) && is.null(alpha.lower) && is.null(alpha.upper)) {
    alpha.lower <- (1 - conf.level) / 2
    alpha.upper <- (1 - conf.level) / 2
  }
  
  .conf.limits.nct.M1 <- function(ncp, df, conf.level = NULL, alpha.lower, alpha.upper, tol = 1e-9, ...) {
    
    min.ncp <- min(-150, -5 * ncp)
    max.ncp <- max(150,   5 * ncp)
    
    # Internal function for upper limit.
    # Note the upper tail is used here, as we seek to find the NCP that has, in its upper tail (alpha.lower, 
    # for the lower limit), the specified value of the observed t/ncp.
    ###########################
    
    .ci.nct.lower <- function(val.of.interest, ...)
      (qt(p = alpha.lower, df = df, ncp = val.of.interest, lower.tail = FALSE, log.p = FALSE) - ncp)^2
    ###########################
    
    # Internal function for lower limit.
    # Note the lower tail is used here, as we seek to find the NCP that has, in its lower tail (alpha.upper, 
    # for the upper limit), the specified value of the observed t/ncp.
    ###########################
    .ci.nct.upper <- function(val.of.interest, ...)
      (qt(p = alpha.upper, df = df, ncp=val.of.interest, lower.tail = TRUE, log.p = FALSE) - ncp)^2
    
    if(alpha.lower != 0) 
      Low.Lim <- suppressWarnings(optimize(f = .ci.nct.lower, interval = c(min.ncp, max.ncp),
                                           alpha.lower = alpha.lower, df = df, ncp = ncp, 
                                           maximize = FALSE, tol = tol))
    
    if(alpha.upper != 0) {
      Up.Lim <- suppressWarnings(optimize(f = .ci.nct.upper, interval = c(min.ncp, max.ncp), 
                                          alpha.upper = alpha.upper, df = df, ncp = ncp, 
                                          maximize = FALSE, tol = tol))
    }
    
    if(alpha.lower == 0) 
      Result <- list(Lower.Limit = -Inf, Prob.Less.Lower = 0, Upper.Limit = Up.Lim$minimum, 
                     Prob.Greater.Upper = pt(q = ncp, ncp = Up.Lim$minimum, df = df))
    if(alpha.upper == 0) 
      Result <- list(Lower.Limit = Low.Lim$minimum, 
                     Prob.Less.Lower = pt(q = ncp, ncp = Low.Lim$minimum, df = df, lower.tail = FALSE), 
                     Upper.Limit = Inf, Prob.Greater.Upper = 0)
    if(alpha.lower != 0 && alpha.upper != 0) 
      Result <- list(Lower.Limit = Low.Lim$minimum, 
                     Prob.Less.Lower = pt(q = ncp, ncp = Low.Lim$minimum, df = df, lower.tail = FALSE), 
                     Upper.Limit = Up.Lim$minimum, 
                     Prob.Greater.Upper = pt(q = ncp, ncp = Up.Lim$minimum, df = df))
    
    return(Result)
  }
  ################################################
  .conf.limits.nct.M2 <- function(ncp, df, conf.level = NULL, alpha.lower, alpha.upper, tol = 1e-9, ...) {
    
    # Internal function for upper limit.
    ###########################
    .ci.nct.lower <- function(val.of.interest, ...)
      (qt(p = alpha.lower, df = df, ncp = val.of.interest, lower.tail = FALSE, log.p = FALSE) - ncp)^2
    
    # Internal function for lower limit.
    ###########################
    .ci.nct.upper <- function(val.of.interest, ...)
      (qt(p = alpha.upper, df = df, ncp = val.of.interest, lower.tail = TRUE, log.p = FALSE) - ncp)^2
    
    Low.Lim <- suppressWarnings(nlm(f = .ci.nct.lower, p = ncp, ...))
    Up.Lim  <- suppressWarnings(nlm(f = .ci.nct.upper, p = ncp, ...))
    
    if(alpha.lower == 0) 
      Result <- list(Lower.Limit = -Inf, Prob.Less.Lower = 0, Upper.Limit = Up.Lim$estimate, 
                     Prob.Greater.Upper = pt(q = ncp, ncp = Up.Lim$estimate, df = df))
    if(alpha.upper == 0) 
      Result <- list(Lower.Limit = Low.Lim$estimate, 
                     Prob.Less.Lower = pt(q = ncp, ncp = Low.Lim$estimate, df = df, lower.tail = FALSE), 
                     Upper.Limit = Inf, Prob.Greater.Upper = 0)
    if(alpha.lower != 0 & alpha.upper != 0) 
      Result <- list(Lower.Limit = Low.Lim$estimate, 
                     Prob.Less.Lower = pt(q = ncp, ncp = Low.Lim$estimate, df = df, lower.tail = FALSE), 
                     Upper.Limit = Up.Lim$estimate, 
                     Prob.Greater.Upper = pt(q = ncp, ncp = Up.Lim$estimate, df = df))
    
    return(Result)
  }
  # Now, use the each of the two methods.
  Res.M1 <- Res.M2 <- NULL
  try(Res.M1 <- .conf.limits.nct.M1(ncp = ncp, df = df, conf.level = NULL, 
                                    alpha.lower = alpha.lower, 
                                    alpha.upper = alpha.upper, tol = tol), silent = TRUE)
  if(length(Res.M1) != 4) 
    Res.M1 <- NULL
  try(Res.M2 <- .conf.limits.nct.M2(ncp = ncp, df = df, conf.level = NULL, 
                                    alpha.lower = alpha.lower,
                                    alpha.upper = alpha.upper, tol = tol), silent = TRUE)
  if(length(Res.M2) != 4) 
    Res.M2 <- NULL
  
  # Now, set-up the test to find the best method.
  Low.M1        <- Res.M1$Lower.Limit
  Prob.Low.M1   <- Res.M1$Prob.Less.Lower
  Upper.M1      <- Res.M1$Upper.Limit
  Prob.Upper.M1 <- Res.M1$Prob.Greater.Upper
  
  Low.M2        <- Res.M2$Lower.Limit
  Prob.Low.M2   <- Res.M2$Prob.Less.Lower
  Upper.M2      <- Res.M2$Upper.Limit
  Prob.Upper.M2 <- Res.M2$Prob.Greater.Upper
  
  # Choose the best interval limits:
  ##Here low
  Min.for.Best.Low <- min((c(Prob.Low.M1, Prob.Low.M2) - alpha.lower)^2)
  
  if(!is.null(Res.M1))
    if(Min.for.Best.Low == (Prob.Low.M1 - alpha.lower)^2) 
      Best.Low <- 1
  if(!is.null(Res.M2))
    if(Min.for.Best.Low == (Prob.Low.M2 - alpha.lower)^2) 
      Best.Low <- 2
  ##Here high
  Min.for.Best.Up <- min((c(Prob.Upper.M1, Prob.Upper.M2) - alpha.upper)^2)
  
  if(!is.null(Res.M1))
    if(Min.for.Best.Up == (Prob.Upper.M1 - alpha.upper)^2) 
      Best.Up <- 1
  if(!is.null(Res.M2))
    if(Min.for.Best.Up == (Prob.Upper.M2 - alpha.upper)^2) 
      Best.Up <- 2
  #####################################
  
  if(is.null(Res.M1)) 
    Low.M1 <- Prob.Low.M1 <- Upper.M1 <- Prob.Upper.M1 <- NA
  if(is.null(Res.M2)) 
    Low.M2 <- Prob.Low.M2 <- Upper.M2 <- Prob.Upper.M2 <- NA
  
  Result <- list(Lower.Limit        = c(Low.M1, Low.M2)[Best.Low], 
                 Prob.Less.Lower    = c(Prob.Low.M1, Prob.Low.M2)[Best.Low], 
                 Upper.Limit        = c(Upper.M1, Upper.M2)[Best.Up], 
                 Prob.Greater.Upper = c(Prob.Upper.M1, Prob.Upper.M2)[Best.Up])
  
  return(Result)
}
