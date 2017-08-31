#
# Copyright (C) 2013-2017 University of Amsterdam
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

ClassicalMetaAnalysis <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ..., DEBUG=0) {
  
  # Restore previous computation state and detect option changes
  run <- perform == "run"
  state <- .retrieveState()
  diff <- if (!is.null(state)) .diff(options, state$options) else NULL
  
  ### This file interfaces the metafor::rma function and associated diagnostic functions
  ### (but it restricts its multipurposeness to one central purpose: fitting a 
  ### meta-regression to effect sizes and their standard errors and providing diagnostics.)
  
  ### options contains:
  ## essentials:
  #   effectsize:  string (maps to 'yi'), name of the variable that contains the effect sizes (ES)
  #   stderr:      string (maps to 'sei'), name of the variable containing the ES standard errors
  #   intercept:   logical (maps to 'intercept'), intercept in the model?
  #
  ## optional:
  #   covariates:  string array (maps to 'mods'), names of continuous predictor variables
  #   factors:     string array (maps to 'mods'), names of nominal/ordinal predictor variables
  ## plotting:
  #   studylabels:    string (maps to 'slab'), name of variable that contains label for a forrest plot
  #   forrestPlot:    logical, make this plot?
  #   funnelPlot:     logical,
  ## advanced analysis:
  #   method:         string, one of ["DL", "HE", "SJ", "ML", "REML", "EB", "HS", "GENQ"] (see ?rma)
  #  -
  #   test:           string, one of ["z", "knha"]
  #   btt:            numeric, vector of indices specifying which coefficients to include in the omnibus test of moderators
  #  - 
  #   ...:            numeric, -3 ≤ value ≤ 3, distribution used is dt((tStatistic - Tlocation) / Tscale, TDf)
  
  #********
  # debuging:
  #  options = list(effectsize = "yi", stderr = "sei", intercept=TRUE,
  #                 covariates = NULL, factors = NULL, studylabels = NULL, 
  #                 forrestPlot = TRUE, funnelPlot = FALSE, method="FE")
  #  readJaspMsg <- function(connection=pipe('pbpaste')) rjson::fromJSON(paste(readLines(connection),collapse = "\n"))
  #  readOptions <- function(connection=pipe('pbpaste')) if (is.list) connection$optiones else readJaspMsg(connection)$options
  #  dd = function(options, ...){x=list(...); options[names(x)] = x; options}
  
  # eval(parse(text = gsub("\\s+\\#","",readLines(pipe('pbpaste')))))
  #dataset = metafor::escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=metafor::dat.bcg)
  #dataset = transform(dataset, sei = vi^0.5)
  #********
  
  #### Compute results of the analysis	####
  
  ## Get data.
  
  effsizeName <- options$effectsize
  stderrName <- options$stderr
  weightsName <- options$wlsWeights
  covarNames <- unlist(options$covariates)
  factNames <- unlist(options$factors)
  
  effsizeName <- unlist(options$dependent)
  stderrName <- unlist(options$wlsWeight)
  covarNames <- NULL
  factNames <- NULL
  if (length(options$covariates) > 0){
    covarNames <- unlist(options$covariates)
  }
  
  list.variables <- c(effsizeName, covarNames)
  list.variables <- list.variables[list.variables != ""]
  to.be.read.variables.numeric <- c(effsizeName, covarNames, stderrName)
  to.be.read.variables.numeric <- to.be.read.variables.numeric [ to.be.read.variables.numeric  != ""]
  
  if (length(options$factors) > 0)
    factNames <- unlist(options$factors)
  
  if (is.null(dataset)) {
    .readDS <- switch(perform, run = .readDataSetToEnd, .readDataSetHeader)
    dataset <- .readDS(
      columns.as.factor = factNames, 
      columns.as.numeric = to.be.read.variables.numeric,
      #columns.as.factor   = factNames, 
      #columns.as.numeric  = c(effsizeName, stderrName, covarNames, weightsName),
      exclude.na.listwise = c()	# let metafor::rma deal with NA's
    )
  }

  ## Run the analysis.
  
  rma.fit <- structure(list('b' = numeric(),'se' = numeric(),'ci.lb' = numeric(),'ci.ub' = numeric(), 
                  'zval' = numeric(),'pval' = numeric()), class = c("dummy", "rma"))
  
  can.run = all(c(effsizeName, stderrName) != "")
  if (run && can.run) {
    #.vmodelTerms = b64(options$modelTerms) # map true names to base64
    .vmodelTerms = options$modelTerms
    dataset = d64(dataset)
    formula.rhs <- as.formula(as.modelTerms(.vmodelTerms))
    if (is.null(formula.rhs)) formula.rhs = ~1
    #rma.fit <- metafor::rma(yi = get(.v(effsizeName)), sei = get(.v(stderrName)), data = dataset,
    rma.fit <- metafor::rma(yi = get(effsizeName), sei = get(stderrName), data = dataset,
                            method=options$method, mods = formula.rhs, test = options$test)
    rma.fit <- d64(rma.fit)
  }
  
  
  if (DEBUG == 1) return(rma.fit)
  
  #### Define the Output: meta, table(s), plot(s), and results objects ####
  
  ### Output Meta Description
  
  meta <- list()
  meta[[1]] <- list(name = "table", type = "table")
  meta[[2]] <- list(name = "qtests", type = "table")
  meta[[3]] <- list(name = "plots", type = "object",
                    meta = list(
                      list(name = "diagnosticPlot", type = "image"),
                      list(name = "forrestPlot", type = "image"),
                      list(name = "funnelplot", type = "image")
                    )
                  )
  
  
  
  
  
  
  
  ### Prepare Coefficients Table Output
  
  table <- list()
  table[["title"]] <- "Coefficients"
  
  # Define table schema
  fields <- list(
    list(name = "name", type = "string", title = " "),
    list(name = "estimate", type = "number", format = "sf:4;dp:3", title = "Estimate"),
    list(name = "se", type = "number", format = "sf:4;dp:3", title = "Std. Error"),
    list(name = "zval", type = "number", format = "sf:4;dp:3", title = "z value"),
    list(name = "pval", type = "number", format = "dp:3;p:.001", title = "Pr(>|z|)"),
    list(name = "ci.lb", type = "number", format = "sf:4;dp:3", title = "Lower Bound"),
    list(name = "ci.ub", type = "number", format = "sf:4;dp:3", title = "Upper Bound")
  )
  table[["schema"]] <- list(fields = fields) 
  
  # Populate the table
  table[["data"]] <- list(
    list(name = "", estimate = ".", se = ".", zval = ".", pval = ".", ci.lb = ".", ci.ub = ".")
  )
  if (run && can.run) {
    coeftable <- .clean(coef(summary(rma.fit)))
    coeftable <- cbind(name = rownames(coeftable), coeftable); 
    coeftable <- lapply(1:nrow(coeftable), function(i) as.list(coeftable[i,])) # apply converts all to string
    table[["data"]]  <- structure(coeftable, .Names=NULL)
  }
  if (DEBUG == 2) return(table)
  
  # Add footnotes to the analysis result
  footnotes <- .newFootnotes()
  footnote.text <- switch(options$test, z = "Wald test.", "Wald test.")
  .addFootnote(footnotes, symbol = "<em>Note.</em>", text = footnote.text)
  table[["footnotes"]] <- as.list(footnotes)
  
  # Add citation reference list
  table[["citation"]] <- list(
    "Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of 
     Statistical Software, 36(3), 1-48. URL: http://www.jstatsoft.org/v36/i03/")
  
  
  
  ### Prepare Q-tests table output
  
  qtesttable <- qTestsTable(rma.fit)
  
  # footnotes
  qfootnotes <- .newFootnotes()
  .addFootnote(qfootnotes, symbol = "<em>Note.</em>", text = "<em>p</em>-values are approximate.")
  qtesttable[["footnotes"]] <- as.list(qfootnotes)
  
  # citation
  qtesttable[["citation"]] <- list(
    "Hedges, L. V., & Olkin, I. (1985). Statistical methods for meta-analysis. San Diego, CA: Academic Press."
  )
  
  
  
  
  ### Prepare Plot Output
  
  # plot 1
  diagnosticPlot.image <- rmaDiagnosticPlot(rma.fit, 820, 820)
  plot1 <- list(title = "Diagnostic plots", width = 820, height = 820, data = diagnosticPlot.image)

  # plot 2
  forestPlot.image <- rmaForestPlot(rma.fit, 520, 520)
  plot2 <- list(title="Forest plot", width = 500, height = 500, data = forestPlot.image)

  
  
  
  ### Create results object to be returned
  
  results <- list()
  results[[".meta"]] <- meta
  results[["title"]] <- analysisTitle(rma.fit)
  results[["table"]] <- table
  results[["qtests"]] <- qtesttable
  results[["plots"]] <- list(title = "Plot", diagnosticPlot = plot1, forrestPlot = plot2)
  

  
  
  
  
  
  ### Finish off: collect objects to keep; update status and state
  
  keep <- NULL
  for (plot in c()) {
    keep <- c(keep, plot$data)
  }
  
  if (run) {
    status <- "complete"
    state <- list(options = options, dataset = dataset)
  } else {
    status <- "inited"
  }
  
  return(list(results = results,
              status = status,
              state = state,
              keep = keep)
  )
  
  
}


as.modelTerms <- function(object) structure(object, class = "modelTerms")

formula.modelTerms <- function(modelTerms, env = parent.frame()) {
  # Converts a modelTerms list into a one-side R formula
  #
  # Args:
  #   modelTerms:  A list of interaction terms, each term being a list of variable names involved in the interaction
  #   env:         An environement associated with the variables in the formula, see ?as.formula
  #
  # Value:
  #   A formula. See ?formula
  #
  terms = sapply(modelTerms, function(x) paste0(unlist(x), collapse = ":"))
  terms = terms[terms != ""]
  formula.rhs = paste(terms, collapse = " + ")
  if (formula.rhs != "") as.formula(paste(" ~ ", formula.rhs), env = env)
}

analysisTitle <- function(object) {
  title <- capture.output(print(object, showfit = F, signif.legend = F))[2]
  title <- gsub("numeric\\(0\\)", "", title)
  title <- gsub("tau\\^2", '&tau;<sup style="font-size:small">2</sup>', title)
  title
}

qTestsTable <- function(object) {
  # Return the ANOVA table with Q-tests for model significance and residual heterogeneity
  #print.output <- capture.output(print(object))
  table <- list()
  
  # Define table schema
  fields <- list(
    list(name = "name", type = "string", title = " "),
    list(name = "qstat", type = "number", format = "sf:4;dp:3", title = "Q statistic"),
    list(name = "df", type = "integer", format = "sf:4;dp:3", title = "Df"),
    list(name = "pval", type = "number", format = "dp:3;p:.001", title = "Pr(>|&chi;&sup2;|)")
  )
  
  # Empty table.
  table[["schema"]] <- list(fields = fields) 
  table[["title"]] <- "Heterogeneity analysis"
  table[["data"]] <- list(list(name="", qstat=".", df=".", pval = "."))
  
  # Return empty table if no fit has been done
  if (is.null(object$QE)) return(table)
  
  # Fill table
  qstat  <- unlist(object[c('QE','QM')])
  df <- c(object$k - object$p, object$m)
  pval <- unlist(object[c('QEp','QMp')])
  name <- c("Test of Residual Heterogeneity","Omnibus test of Model Coefficients")
  qtable <- data.frame(name, qstat, df, pval)
  table[["data"]] <- lapply(1:2, function(i) as.list(qtable[i,]))
  table
}

rmaDiagnosticPlot <- function(object, width = 500, height = 500) {
  image.reference <- .beginSaveImage(width, height)
  plot(object)
  .endSaveImage(image.reference)
}

rmaForestPlot <- function(object, width = 500, height = 500) {
  image.reference <- .beginSaveImage(520,520)
  cmaForest(object)
  .endSaveImage(image.reference)
}
cmaForest <- function(x, ...) UseMethod("cmaForest")
cmaForest.dummy <- function(x, ...) {} # do nothing
cmaForest.rma <- function(x, ...) metafor::forest(x, ...)

print.dummy <- function(...) {
}

plot.dummy <- function(object, ...) {
  plot(1:10, type = 'b', col=1:10)
}

forest.dummy <- function(object, ...) {
  # do nothing
}

b64.rma <- function(x, ...) {
  ## Translate names in object x to base64 
  idx = c('b', 'beta', 'call')
  x[idx] = b64(x[idx])
  x
}
d64.rma <- function(x, ...) {
  ## Untranslate names in object x from base64 
  idx = c('b', 'beta', 'call')
  x[idx] = d64(x[idx])
  x
}

if(interactive()) {
  # For debug purposes these are reset because the ones defined in 'common.R' only work properly 
  # from within JASP. Make sure to source('common.R') first!
  .beginSaveImage <- function(...) {}
  .endSaveImage <- function(...){}
  .requestTempFileNameNative <- function(ext) tempfile(fileext = ext)
}