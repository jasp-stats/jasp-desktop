#
# Copyright (C) 2017 University of Amsterdam
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

ReliabilityAnalysis <- function(dataset = NULL, options, perform = "run",
                                callback = function(...) 0,  ...) {
  
  variables <- unlist(options$variables)
  
  if (is.null(dataset)) {
    
    if (perform == "run") {
      
      dataset <- .readDataSetToEnd(columns.as.numeric=variables, columns.as.factor=NULL, exclude.na.listwise=NULL)
      
    } else {
      
      dataset <- .readDataSetHeader(columns.as.numeric=variables, columns.as.factor=NULL)
      
    }
    
  } else {
    
    dataset <- .vdf(dataset, columns.as.numeric=NULL, columns.as.factor=variables)
    
  }
  
  ## Retrieve State
  
  state <- .retrieveState()
  
  resultsAlpha <- NULL
  
  if ( ! is.null(state)) {  # is there state?
    
    diff <- .diff(options, state$options)  # compare old and new options
    
    if (is.list(diff) && diff[['variables']] == FALSE && diff[['reverseScaledItems']] == FALSE) {
      
      resultsAlpha <- state$resultsAlpha
      
    }
    
  }
  
  # Store results
  
  results <- list()
  
  results[["title"]] <- "Reliability Analysis"
  
  meta <- list(list(name="reliabilityScale", type="table"),
               list(name="reliabilityItemsObj", type="object", meta=list(list(name="reliabilityItems", type="table"))))
  
  results[[".meta"]] <- meta
  
  errorList = NULL
  
  if (is.null(resultsAlpha)) {
    
    # check for errors
    errCheck <- .reliabilityErrCheck(
      dataset, variables, perform,
      checks = c("var(x) > 0", "!is.na(x)")
    )
    
    doUpdate <- errCheck$go
    
    if (doUpdate) {
      
      resultsAlpha <- .reliabilityResults(dataset, options, variables, perform)
      
    } else if (!is.null(errCheck$checkMat)) { # if true implies that checkData was actually run
      
      # generate eror message
      errMessage <- .reliabilityMakeErrorMessage(
        checkMat = errCheck$checkMat, 
        messages = c("has 0 variance.", "contains missing values."),
        variables = variables
      )
      
      errorList = list(errorType = "badData", errorMessage = errMessage)
      
    }
    
  } else { # if previous data is retrieved from the state
    
    doUpdate = TRUE
    
  }
  
  results[["reliabilityScale"]] <- .reliabalityScaleTable(resultsAlpha, dataset, options, variables, perform)
  results[["reliabilityScale"]][["error"]] <- errorList
  
  if (doUpdate && options$alphaItem || options$gutmannItem || options$itemRestCor || options$meanItem || options$sdItem) {
    results[["reliabilityItemsObj"]] <- list(title="Item Statistics", reliabilityItems=.reliabalityItemsTable(resultsAlpha, options, variables, perform))
  } else {
    results[["reliabilityItemsObj"]] <- NULL
  }
  
  # Save state
  
  state[["options"]] <- options
  state[["resultsAlpha"]] <- resultsAlpha
  
  if (perform == "init") {
    
    return(list(results=results, status="inited", state=state))
    
  } else {
    
    return(list(results=results, status="complete", state=state))
    
  }
}

.reliabilityErrCheck <- function(dataset, variables, perform, checks) {
  
  output <- list(go = FALSE, checkMat = NULL)
  
  if (perform == "run" && !is.null(variables) && length(variables) > 1) {
    
    d1 <- as.matrix(dataset)
    
    checks <- lapply(checks, function(x) c(is.numeric, list(x)))
    d2 <- .reliabalityCheckData(d1, delete = FALSE, checks)
    
    output$go <- all(d2$checkVec)
    output$checkMat <- d2$checkMat
    
  } 
  
  return(output)
  
}

.reliabilityMakeErrorMessage <- function(checkMat, messages, variables) {
  
  # ensure that variable names in options match columns names of data
  varV <- sapply(variables, .v)
  index <- sapply(varV, function(x, checkMat) which(x == colnames(checkMat)), checkMat = checkMat)
  
  idx <- which(!checkMat, arr.ind = TRUE) # find error/ column match
  varNames <- names(index[match(idx[, 2], index)]) # correct variable names of found errors
  varNames <- paste0("'", varNames, "'")
  addMsg <- paste("Variable", varNames, messages[idx[, 1]], collapse = '\n')
  
  # subject to change
  if (nrow(idx) == 1) {
    defMsg <- "The following error was encountered:\n"
  } else {
    defMsg <- "The following errors were encountered:\n"
  }
  
  return(paste0(defMsg, addMsg, collapse = ''))
}

.reliabilityResults <- function (dataset, options, variables, perform) {
  
  if (perform == "run" && !is.null(variables) && length(variables) > 1) {
    
    d <- as.matrix(dataset)
    
    if (all(options$variables %in% options$reverseScaledItems)) {
      r <- psych::alpha(d)
    } else {
      r <- psych::alpha(d, key = .v(unlist(options$reverseScaledItems)))
    }
    
  } else {
    
    r <- NULL
    
  }
  
  return(r)
}

.reliabalityScaleTable <- function (r, dataset, options, variables, perform) {
  
  table <- list()
  
  table[["title"]] <- "Scale Reliability Statistics"
  
  fields = list(list(name="case", title="", type="string"))
  
  if (options$meanScale)
    fields[[length(fields) + 1]] <- list(name="mu", title="mean", type="number", format="sf:4;dp:3")
  
  if (options$sdScale)
    fields[[length(fields) + 1]] <- list(name="sd", title="sd", type="number", format="sf:4;dp:3")
  
  if (options$alphaScale)
    fields[[length(fields) + 1]] <- list(name="alpha", title="Cronbach's \u03B1", type="number", format="sf:4;dp:3")
  
  if (options$gutmannScale)
    fields[[length(fields) + 1]] <- list(name="lambda", title="Gutmann's \u03BB6", type="number", format="sf:4;dp:3")
  
  if (options[["averageInterItemCor"]])
    fields[[length(fields) + 1]] <- list(name="rho", title="Average interitem correlation", type="number", format="sf:4;dp:3")
  
  
  table[["schema"]] <- list(fields = fields)
  
  data <- list()
  
  if (!is.null(r)) {
    
    footnotes <- .newFootnotes()
    
    # is.null can be removed once options[["listwise"]] exists.
    if (!is.null(options[["listwise"]])) {
      exclwise = ifelse(options[["listwise"]], " listwise", " pairwise")
    } else {
      exclwise = ""
    }
    
    nObs = nrow(dataset)
    nExcluded = 0L
    nValid = nObs - nExcluded
    
    # message <- paste("Scale consists of items ", paste0(variables, collapse = ", "))
    message <- sprintf("Of the observations, %d were used, %d were excluded%s, and %d were provided.",
                       nValid, nExcluded, exclwise, nObs)
    
    .addFootnote(footnotes, symbol = "<em>Note.</em>", text=message)
    
    table[["footnotes"]] <- as.list(footnotes)
    
    
    alpha <- NULL
    lambda <- NULL
    mu <- NULL
    sd <- NULL
    rho <- NULL
    
    if (options$alphaScale)
      alpha <- .clean(r$total$raw_alpha)
    
    if (options$gutmannScale)
      lambda <- .clean(r$total[["G6(smc)"]])
    
    if (options$meanScale)
      mu <- .clean(r$total$mean)
    
    if (options$sdScale)
      sd <- .clean(r$total$sd)
    
    if (options[["averageInterItemCor"]])
      rho <- .clean(r[["total"]][["average_r"]])
    
    data[[1]] <- list(case="scale", alpha=alpha, lambda=lambda, rho=rho, mu=mu, sd=sd)
    
    table[["status"]] <- "complete"
    
  } else {
    
    data[[1]] <- list(case="scale", alpha=".", lambda=".", rho =".", mean=".", sd=".")
    
  }
  
  table[["data"]] <- data
  
  return(table)
  
}

.reliabalityItemsTable <- function (r, options, variables, perform) {
  
  table <- list()
  
  table[["title"]] <- "Item Reliability Statistics"
  
  overTitle <- paste0("If item dropped")
  
  fields = list(list(name="case", title="", type="string", combine=TRUE))
  
  if (options$meanItem)
    fields[[length(fields) + 1]] <- list(name="mu", title="mean", type="number", format="sf:4;dp:3")
  
  if (options$sdItem)
    fields[[length(fields) + 1]] <- list(name="sd", title="sd", type="number", format="sf:4;dp:3")
  
  if (options$itemRestCor)
    fields[[length(fields) + 1]] <- list(name="itemRestCor", title="item-rest correlation", type="number", format="sf:4;dp:3")
  
  if (options$alphaItem)
    fields[[length(fields) + 1]] <- list(name="alpha", title="Cronbach's \u03B1", type="number", format="sf:4;dp:3", overTitle = overTitle)
  
  if (options$gutmannItem)
    fields[[length(fields) + 1]] <- list(name="lambda", title="Gutmann's \u03BB6", type="number", format="sf:4;dp:3", overTitle = overTitle)
  
  table[["schema"]] <- list(fields = fields)
  
  data <- list()
  
  footnotes <- .newFootnotes()
  
  if (length(options$reverseScaledItems) > 0) {
    message <- "reverse-scaled item"
    .addFootnote(footnotes, symbol = "\u207B", text=message)
  }
  
  rowNames <- gsub("-","", rownames(r$alpha.drop))
  
  # print("rowNames = ")
  # print(rowNames)
  
  
  if (!is.null(r)) {
    
    # print("variables = ")
    # print(variables)
    
    for (var in variables) {
      
      varV <- .v(var)
      index <- which(varV == rowNames)
      
      # print("varV = ")
      # print(varV)
      # print("index = ")
      # print(index)
      
      
      alpha <- NULL
      lambda <- NULL
      itemRestCor <- NULL
      mu <- NULL
      sd <- NULL
      
      if (var %in% options$reverseScaledItems) {
        case <- paste0(var,"\u207B")
      } else {
        case <- var
      }
      
      if (options$alphaItem)
        alpha <- .clean(r$alpha.drop[index,"raw_alpha"])
      
      if (options$gutmannItem)
        lambda <- .clean(r$alpha.drop[index, "G6(smc)"])
      
      if (options$itemRestCor)
        itemRestCor <- .clean(r$item.stats[index,"r.drop"])
      
      if (options$meanItem)
        mu <- .clean(r$item.stats[index,"mean"])
      
      if (options$sdItem)
        sd <- .clean(r$item.stats[index,"sd"])
      
      data[[length(data) + 1]] <- list(case=case, alpha=alpha, lambda=lambda, itemRestCor=itemRestCor, mu=mu, sd=sd)
    }
    
    table[["status"]] <- "complete"
    
  } else {
    
    variablesTemp <- variables
    
    if (is.null(variables))
      variablesTemp <- "..."
    
    for (var in variablesTemp) {
      
      data[[length(data) + 1]] <- list(case=var, alpha=".", lambda=".", itemRestCor=".", mu=".", sd=".")
      
    }
  }
  
  table[["data"]] <- data
  
  table[["footnotes"]] <- as.list(footnotes)
  
  return(table)
  
}


# function for generic error checking -- perhaps move to common?
# returns a list of indices of columns that remain and a matrix with which column
# passed/failed which check. The former is for checking is there an error, the latter
# for checking where is the error.
.reliabalityCheckData <- function(data, delete = FALSE, ...) {
  # helpfunction: executes check y on data x, returns list of TRUE/ FALSE
  doCheck <- function(x, y) {
    if (is.character(y)) {
      f <- function(x, y) all(eval(parse(text = y))) 
    } else {
      f <- function(x, y) all(do.call(y, list(x)))
    }
    return(lapply(x, f, y))
  }
  
  # cast to dataframe in order for sapply to work on columns
  if (!is.data.frame(data)) data <- as.data.frame(data)
  dots <- list(...)
  # every element in dots should be a list with as element 1 a column identifier
  # (indices or function) and as element 2 a list of checks for that those columns.
  if (is.list(dots[[1]][[1]])) dots <- dots[[1]]
  # instead of using passing multiple checks as multiple arguments,
  # they can also be passed as multiple checks in one list
  omit <- rep(TRUE, ncol(data))
  # omit is a vector with initialy true for each column.
  # everytime checks are done, omit & check gets executed.
  # if a check it not met the given column value will be FALSE (indicating delete)
  omitMat <- matrix(TRUE, length(dots), ncol(data))
  colnames(omitMat) <- colnames(data)
  rownames(omitMat) <- unlist(lapply(dots, `[[`, 2))
  # a matrix where checks are stored
  for (i in seq_along(dots)) { # loop over checks
    lst <- dots[[i]]
    # find columns by function
    if (is.function(lst[[1]])) {
      ind <- unname(sapply(data, lst[[1]]))
    } else { # find columns by given index
      ind <- rep(FALSE, ncol(data))
      ind[lst[[1]]] <- TRUE
    }
    # ind is a logical vector indicating column indices to do the check on 
    # checking here which of ind & omit match avoids doing multiple checks on columns
    # that are already set to FALSE (indicating delete).
    ind <- ind & omit
    if (!any(ind)) next() # If no indices remain to be checked goto next check
    # Map over each column of data and do checks in lst - returns list
    checks <- Map(doCheck, 
                  x = list(data[, ind, drop = FALSE]), # data
                  y = lst[[2]]) # list of checks
    # Make checks a matrix - length(checks) corresponds to the number of done on the data
    checks <- matrix(unlist(checks, use.names = FALSE), ncol = length(checks))
    # evaluate per row if all checks are met
    checks <- apply(checks, 1, all)
    # isTRUE() turns potential logical(0), NA, and NaN to FALSE
    checks <- sapply(checks, isTRUE)
    # evaluate checks with prior values of omit
    omit[ind] <- omit[ind] & checks
    # update omit matrix
    omitMat[i, ind] <- omit[ind]
  }
  if (delete) {
    # if all columns failed the checks
    if (sum(omit) == ncol(data)) {
      return(NULL)
    } else { # return data without said columns
      return(data[, omit, drop = FALSE])
    }
  } else { # return logical matrix with columns that passed/ failed checks
    # omit: logical vector with columns that passed checks
    # omitMat: logical matrix where columns correspond to columns of the data
    # and rows correspond to checks
    return(list(checkVec = omit, checkMat = omitMat))
  }
}
