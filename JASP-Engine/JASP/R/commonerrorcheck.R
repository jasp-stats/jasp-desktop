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

.quitAnalysis <- function(message) {
  # Function to gracefully exit an analysis when continuing to run is nonsensical.
  # Comparable to stop(message), except this raises an validationError.
  # Arg message: String with the reason why the analysis has ended.
  
  e <- structure(class = c('validationError', 'error', 'condition'),
                 list(message=message, call=sys.call(-1)))
  stop(e)
}


.addStackTrace <- function(e) {
  # Adds a stacktrace to the error object when an exception is encountered.
  # Includes up to the latest 10 system calls; the non-informational system calls are omitted.
  # Arg e: error object.

  stack <- ''
  if (! is.null(sys.calls()) && length(sys.calls()) >= 9) {
    
    stack <- sys.calls()  
    stack <- head(stack[7:length(stack)], -2)
    if (length(stack) > 10) {
      stack <- tail(stack, 10)
    }
    
  }
  e$stackTrace <- stack
  signalCondition(e)
}


.generateErrorMessage <- function(type, opening=FALSE, concatenate=NULL, grouping=NULL, ...) {
  # Generic function to create an error message (mostly used by .hasErrors() but it can be called directly).
  # Args:
  #   type: String containing either (1) check type consistent with a message type in commmonmessages.R, or (2) an error message.
  #   opening: Boolean, indicate if there should be a general opening statement (TRUE) or only the specific error message (FALSE).
  #   concatenate: String, include if you want to append the error message to an already existing message.
  #   grouping: String vector indicating the grouping variables. This will add the line 'after grouping on...' to the message.
  #   ...: Each error message can have any number of variables, denoted by {{}}'s. Add these as arg=val pairs.
  #
  # Returns:
  #   String containing the error message.
  
  if (! is.character(type) || length(type) == 0) {
    stop('Non-valid type argument provided')
  }
  
  # Retrieve the error message; spaces indicate that it is already an error message.
  if (grepl(' ', type, fixed=TRUE) == TRUE) {
    message <- type
  } else {
    args <- c(list(grouping=grouping, class='error', type=type), list(...))
    message <- do.call(.messages, args)
  }
  
  # Turn the message in a html list item
  if (! is.null(concatenate) || opening == TRUE) {
    message <- paste0('<li>', message, '</li></ul>')
  }
  
  # See if we should concatenate it with something.
  if (is.character(concatenate) && length(concatenate) == 1) {
    endOfString <- substr(concatenate, nchar(concatenate)-4, nchar(concatenate))
    if (endOfString == '</ul>') {
      concatenate <- substr(concatenate, 1, nchar(concatenate)-5)
    }
    message <- paste0(concatenate, message)
  }
  
  # See if we should add an opening line.
  if (opening == TRUE) {
    openingMsg <- .messages('error', 'opening')
    if (grepl(openingMsg, message, fixed=TRUE) == FALSE) {
      message <- paste0(openingMsg, '<ul>', message)
    }
  }

  # See if we should add the variables causing the error (so far only for perfect correlations).
  if (type == "varCovData" && ! is.null(args$variables)) {
    nPairs <- length(args$variables$var1) # Could take either var1 or var2 as length is identical
    if (nPairs > 10) {
      msg<- sprintf("<ul><li> Note: There are %d pairs of variables perfectly correlated. The first 10 are: ", nPairs)
    } else {
      msg<- "<ul><li> Note: The following pair(s) of variables is/are perfectly correlated: "
    }
    message <- paste0(message, msg)
    for (i in seq_len(nPairs)) {
      message <- paste0(message, args$variables$var1[[i]], " and ", args$variables$var2[[i]])
      if (i < length(args$variables$var1)) {
        message <- paste0(message, "; ")
      } else if (i == length(args$variables$var1)) {
        message <- paste0(message, ". Note that if you have specified a weights variable, the correlations are computed for the weighted variables.</li></ul>")
      }
    }
  }
  
  return(message)
}


.hasErrors <- function(dataset=NULL, type=NULL, custom=NULL, message='default', exitAnalysisIfErrors=FALSE, ...) {
  # Generic error checking function.
  # Args:
  #   dataset: Normal JASP dataset.
  #   type: List/vector of strings containing check types.
  #   message: 'short' or 'default' should only the first failure of a check be reported in footnote style ('short'), or should every check failure be mentioned in multi-line form.
  #   exitAnalysisIfErrors: Boolean, should the function simply return its results (FALSE), or abort the entire analysis when a failing check is encountered (TRUE).
  #   custom: A function that performs some check and returns an error message, or a list containing multiple (named) check functions.
  #   ...: Each check may have required and optional arguments, they are specified in the error check subfunctions.
  #
  # Returns:
  #   FALSE if no errors were found or a named list specifying for each check which variables violated it as well as a general error message.
  
  if (length(type) == 0 && length(custom) == 0)
    return(FALSE)
  
  if (exitAnalysisIfErrors && message == 'short')
    message <- 'default'
  
  # Error checks definition.
  checks <- list()
  checks[['infinity']] <- list(callback=.checkInfinity, addGroupingMsg=FALSE)
  checks[['factorLevels']] <- list(callback=.checkFactorLevels)
  checks[['variance']] <- list(callback=.checkVariance, addGroupingMsg=TRUE)
  checks[['observations']] <- list(callback=.checkObservations, addGroupingMsg=TRUE)
  checks[['observationsPairwise']] <- list(callback=.checkObservationsPairwise, addGroupingMsg=TRUE)
  checks[['varCovMatrix']] <- list(callback=.checkVarCovMatrix, addGroupingMsg=FALSE)
  checks[['limits']] <- list(callback=.checkLimits, addGroupingMsg=FALSE)
  checks[['varCovData']] <- list(callback=.checkVarCovData, addGroupingMsg=TRUE)
  checks[["modelInteractions"]] <- list(callback=.checkModelInteractions, addGrouping=FALSE)
  checks[["negativeValues"]] <- list(callback=.checkNegativeValues, addGroupingMsg=TRUE)
  checks[["missingValues"]] <- list(callback=.checkMissingValues, addGroupingMsg=TRUE)
  checks[["duplicateColumns"]] <- list(callback=.checkDuplicateColumns, addGroupingMsg=TRUE)
  
  args <- c(list(dataset=dataset), list(...))
  errors <- list(message=NULL)
  
  # Add info about the custom check functions to the type and checks objects.
  if (length(custom) > 0) {
    if (is.function(custom)) {
      checks[['_custom']] <- list(callback=custom, isCustom=TRUE, hasNamespace=FALSE)
      type <- c(type, '_custom')
    } else if (is.list(custom)) {
      
      if (is.null(names(custom))) {
        names(custom) <- paste0('_custom', seq(length(custom)))
        namespace <- FALSE
      } else {
        namespace <- TRUE
      }
      
      for (i in 1:length(custom)) {
        if (is.function(custom[[i]])) {
          checks[[ names(custom)[i] ]] <- list(callback=custom[[i]], isCustom=TRUE, hasNamespace=namespace)
          type <- c(type, names(custom)[i])
        }
      }
      
    }
  }
  
  for (i in 1:length(type)) {
    
    check <- checks[[ type[[i]] ]]
    if (is.null(check))
      stop('Unknown check type provided: "', type[[i]], '"')
    
    isCustom <- ! is.null(check[['isCustom']]) # Is it an analysis-specific check?
    hasNamespace <- ! isCustom || check[['hasNamespace']] == TRUE # Is it a named check?
    
    # Check the arguments provided/required for this specific check.
    funcArgs <- NULL
    if (hasNamespace) {
      funcArgs <- base::formals(check[['callback']])
      funcArgs['...'] <- NULL
      if (length(funcArgs) > 0) {
        # Attach the check specific prefix, except for the dataset arg.
        argsExceptDataset <- names(funcArgs) != 'dataset'
        names(funcArgs)[argsExceptDataset] <- paste0(type[[i]], '.', names(funcArgs)[argsExceptDataset])
        
        # Fill in the 'all.*' arguments for this check
        argsAllPrefix <- args[startsWith(names(args), 'all.')]
        if (length(argsAllPrefix) > 0) {
          for (a in names(argsAllPrefix)) {
            funcArg <- gsub('all', type[[i]], a, fixed=TRUE)
            if (funcArg %in% names(funcArgs)) {
              args[[funcArg]] <- args[[a]]
            }
          }
        }
        
        # See if this check expects target variables and if they were provided, if not add all variables.
        funcTargetVars <- paste0(type[[i]], '.target')
        if (funcTargetVars %in% names(funcArgs) && !funcTargetVars %in% names(args))
          args[[funcTargetVars]] <- .unv(names(dataset))
        
        # Obtain an overview of required and optional check arguments.
        optArgs <- list()
        reqArgs <- list()
        for (a in 1:length(funcArgs)) {
          if (is.symbol(funcArgs[[a]])) # Required args' value is symbol.
            reqArgs <- c(reqArgs, funcArgs[a])
          else
            optArgs <- c(optArgs, funcArgs[a])
        }
        
        if (length(reqArgs) > 0 && all(names(reqArgs) %in% names(args)) == FALSE) {
          missingArgs <- reqArgs[! names(reqArgs) %in% names(args)]
          stop('Missing required argument(s): "', paste(names(missingArgs), collapse=','), '"')
        }
        
        # If .hasErrors() was called when the analysis is not ready, don't perform this error check.
        # Not ready is defined as the dataset being NULL, or being an unnamed data.frame (.readDatasetToEnd() returns this when no variables are specified).
        if (length(reqArgs) > 0 && ("dataset" %in% names(reqArgs) && is.null(dataset) || length(names(dataset)) == 0))
          next 
        
        if (length(optArgs) > 0 && all(names(optArgs) %in% names(args)) == FALSE)
          args <- c(args, optArgs[! names(optArgs) %in% names(args)])
      }
      
    }

    # Perform the actual error check.
    if (hasNamespace && length(funcArgs) > 0) {
      callingArgs <- args[names(funcArgs)]
      names(callingArgs) <- gsub(paste0(type[[i]], '.'), '', names(callingArgs), fixed=TRUE)
      checkResult <- try(base::do.call(check[['callback']], callingArgs))
    } else {
      checkResult <- try(check[['callback']]())
    }
    
    # See if the check itself terminated with an exception (oh dear).
    if (isTryError(checkResult))
      next
    
    # If we don't have an error we can go to the next check.
    if ((! isCustom && checkResult[['error']] != TRUE) || (isCustom && (! is.character(checkResult) || checkResult == '')))
      next
    
    # Create the error message.
    if (! (message == 'short' && ! is.null(errors[['message']]))) {
      opening <- FALSE
      if (is.null(errors[['message']]) && message != 'short')
        opening <- TRUE
      
      varsToAdd <- NULL
      if (! isCustom)
        varsToAdd <- checkResult[['errorVars']]
      
      grouping <- NULL
      if (! is.null(check[['addGroupingMsg']]) && check[['addGroupingMsg']] == TRUE && 
          ! is.null(args[[ paste0(type[[i]], '.grouping') ]]) )
        grouping <- args[[ paste0(type[[i]], '.grouping') ]]
      
      msgType <- type[[i]]
      if (isCustom)
        msgType <- checkResult
      
      errors[['message']] <- base::do.call(.generateErrorMessage, c(list(type=msgType, 
        opening=opening, concatenate=errors[['message']], variables=varsToAdd, grouping=grouping), 
        args))
    }
    
    if (! hasNamespace) {
      next  # We won't add info of the error to the list if we don't have a name.
    }
    
    # Add the error (with any offending variables, or TRUE if there were no variables) to the list.
    if (is.list(checkResult) && ! is.null(checkResult[['errorVars']]))
      errors[[ type[[i]] ]] <- checkResult[['errorVars']]
    else
      errors[[ type[[i]] ]] <- TRUE
    
  } # End for-loop.
  
  if (is.null(errors[['message']]))
    return(FALSE)
  
  if (exitAnalysisIfErrors == TRUE)
    .quitAnalysis(errors[['message']])
  
  return(errors) 
}


.applyOnGroups <- function(func, dataset, target, grouping, levels=NULL) {
  # Convenience function to apply a check on a specific level of the dependent, or on all subgroups.
  # Args:
  #   func: Function to perform on the subgroup(s).
  #   dataset: JASP dataset.
  #   target: Single string with the dependent variable.
  #   grouping: String vector indicating the grouping variables.
  #   levels: Vector indicating the level of each of the grouping variables.
  #
  # Returns:
  #   Result of the func in vector form when no levels were supplied, otherwise as a single value.

  
  if (length(levels) > 0) {
    
    if (length(grouping) != length(levels)) {
      stop('Each grouping variable must have a level specified')
    }
    
    # The levels vector may be a 'mix' of numeric and characters, we need to add additional quotation marks around characters.
    if (is.character(levels)) {
      levels <- vapply(levels, function(x) {
        if (suppressWarnings(is.na(as.numeric(x)))) {
          paste0("\"", x, "\"")
        } else {
          x
        }
      }, character(1))
    }
    
    expr <- paste(.v(grouping), levels, sep='==', collapse='&')
    dataset <- subset(dataset, eval(parse(text=expr)))
    result <- func(dataset[[.v(target)]])
    
  } else {

    vgrouping <- .v(grouping)
    vtarget   <- .v(target)

    result <- plyr::ddply(dataset, vgrouping,
      function(data, vtarget) {
        if (anyNA(data[vgrouping]) == FALSE) {
          func(data[[vtarget]])
        }
      },  vtarget)
    result <- result[[ncol(result)]] # The last column holds the func results.
    
  }
  
  return(result)
}


.checkInfinity <- function(dataset, target, grouping=NULL, groupingLevel=NULL) {
  # Check for infinity in the dataset. 
  # Args:
  #   dataset: JASP dataset.
  #   target: String vector indicating the target variables.
  #   grouping: String vector indicating the grouping variables.
  #   groupingLevel: Vector indicating the level of each of the grouping variables.
  
  result <- list(error=FALSE, errorVars=NULL)
  
  findInf <- function(x) {
    return(any(is.infinite(x)))
  }
  
  for (v in target) {
    
    if (is.factor(dataset[[.v(v)]])) { # Coerce factor to numeric.
      dataset[[.v(v)]] <- as.numeric(as.character(dataset[[.v(v)]]))
    } 
    
    if (length(grouping) > 0 && length(groupingLevel) > 0) { 
      hasInf <- .applyOnGroups(findInf, dataset, v, grouping, groupingLevel)
    } else { # Makes no sense to check all subgroups for infinity rather than the entire variable at once.
      hasInf <- findInf(dataset[[.v(v)]])
    }
    
    if (hasInf) {
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  return(result)
}


.checkFactorLevels <- function(dataset, target, amount) {
  # Check if there are the required amount of levels in factors.
  # Args:
  #   dataset: JASP dataset.
  #   target: String vector indicating the target variables.
  #   amount: String vector indicating the amount to check for (e.g. '< 2', or '!= 2').
  
  result <- list(error=FALSE, errorVars=NULL)

  for (v in target) {
    
    levelsOfVar <- length(unique(na.omit(dataset[[.v(v)]])))
    for (checkAmount in amount) {
      expr <- paste(levelsOfVar, checkAmount)
      if (eval(parse(text=expr))) {
        result$error <- TRUE
        result$errorVars <- c(result$errorVars, v)
        break
      }
    }
    
  }
  return(result)
}


.checkVariance <- function(dataset, target, equalTo=0, grouping=NULL, groupingLevel=NULL) {
  # Check for a certain variance in the dataset. 
  # Args:
  #   dataset: JASP dataset.
  #   target: String vector indicating the target variables.
  #   equalTo: Single numeric.
  #   grouping: String vector indicating the grouping variables.
  #   groupingLevel: Vector indicating the level of each of the grouping variables.
  
  result <- list(error=FALSE, errorVars=NULL)
  
  getVariance <- function(x) {
    validValues <- x[is.finite(x)]
    variance <- -1 # Prevents the function from returning NA's
    if (length(validValues) > 1) {
      variance <- stats::var(validValues)
    }
    return(variance)
  }
  
  for (v in target) {
    
    if (length(grouping) > 0) {
      variance <- .applyOnGroups(getVariance, dataset, v, grouping, groupingLevel)
    } else {
      variance <- getVariance(dataset[[.v(v)]])
    }
    
    if (any(variance == equalTo)) {
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  return(result)
}


.checkObservations <- function(dataset, target, amount, grouping=NULL, groupingLevel=NULL) {
  # Check the number of observations in the dependent(s).
  # Args:
  #   dataset: JASP dataset.
  #   target: String vector indicating the target variables.
  #   amount: String vector indicating the amount to check for (e.g. '< 2', or '> 4000').
  #   grouping: String vector indicating the grouping variables.
  #   groupingLevel: Vector indicating the level of each of the grouping variables.
  
  result <- list(error=FALSE, errorVars=NULL)
  
  getObservations <- function(x) {
    return(length(na.omit(x)))
  }

  for (v in target) {
    
    if (length(grouping) > 0) {
      obs <- .applyOnGroups(getObservations, dataset, v, grouping, groupingLevel)
    } else {
      obs <- getObservations(dataset[[.v(v)]])
    }
    
    for (checkAmount in amount) {
      expr <- paste(obs, checkAmount)
      if (any(sapply(expr, function(x) eval(parse(text=x))))) { # See if any of the expressions is true.
        result$error <- TRUE
        result$errorVars <- c(result$errorVars, v)
        break
      }
    }
    
  }
  return(result)
}

.checkObservationsPairwise <- function(dataset, target, amount, grouping=NULL, groupingLevel=NULL) {
  # Check the number of observations in the dependent(s).
  # Args:
  #   dataset: JASP dataset.
  #   target: String vector indicating the target variables.
  #   amount: String vector indicating the amount to check for (e.g. '< 2', or '> 4000').
  #   grouping: String vector indicating the grouping variables.
  #   groupingLevel: Vector indicating the level of each of the grouping variables.
  result <- list(error=FALSE, errorVars=NULL)

  dataPairs <- dataset[, .v(target)]
  
  if (sum(!apply(dataPairs, 1, function(x){any(is.na(x))})) <= amount) { # See if any of the expressions is true.
    result$error <- TRUE
    result$errorVars <- c(result$errorVars, target)
  }

  return(result)
}

# Check if data set is variance-covariance matrix
.checkVarCovMatrix <- function(dataset,nrow=TRUE,symm=TRUE,posdef=TRUE,...){

  # as matrix:
  dataMatrix <- as.matrix(dataset)
  
  # number of rows equal to number of columns?
  if (nrow && nrow(dataset) != ncol(dataset)){
    return(list(error=TRUE,reason="Dataset is not a square matrix"))
  }
  
  # Symmetrical?
  if (symm && !all(round(dataset,10) == t(round(dataset,10)))){
    return(list(error=TRUE,reason="Matrix is not symmetrical"))
  }
  
  # Positive-definite?
  if (posdef && any(round(eigen(dataset)$values,10) <= 0)){
    vars <- .checkForPerfectCorrelations(dataset)
    return(list(error=TRUE,reason="Matrix is not positive-definite", vars=vars))
  }
  
  return(list(error=FALSE, reason = ""))
}

.checkForPerfectCorrelations <- function(dataset) {
  
  vars <- NULL
  # if not already a correlation matrix
  if (! all(diag(dataset) == 1)) {
    dataset <- cov2cor(dataset)
  }
  idx <- which(lower.tri(dataset, diag = FALSE), arr.ind = TRUE) # index for the lower triangle of the matrix
  bad <- (1 - abs(dataset[idx])) <= sqrt(.Machine$double.eps)    # check if correlations are awfully close to -1 or 1
  if (identical(any(bad), TRUE)) {
    pairsIdxCol <- idx[bad, , drop = FALSE][, "col"] # index for colnames of pairs of highly correlated variables
    pairsIdxRow <- idx[bad, , drop = FALSE][, "row"] # index for rownames of pairs of highly correlated variables
    vars <- list("var1" = .unvf(colnames(dataset)[pairsIdxCol]), "var2" = .unvf(rownames(dataset)[pairsIdxRow]))
  }
  return(vars)
}

.checkLimits <- function(dataset, target, min=-Inf, max=Inf) {
  # Check if the variable is between certain limits
  # Args:
  #   dataset: JASP dataset.
  #   target: String vector indicating the target variables.
  #   min: Number indicating minimum allowed (inclusive)
  #   max: Number indicating maximum allowed (inclusive)
  
  result <- list(error=FALSE, errorVars=NULL)

  for (v in target) {
    
    rangeOfVar <- range(na.omit(dataset[[.v(v)]]))
    
    if (rangeOfVar[1] < min || rangeOfVar[2] > max) {
      result$error <- TRUE
      result$errorVars <- c(result$errorvars, v)
    }
    
  }
  return(result)
}

.checkVarCovData <- function(dataset, target, corFun = NULL, grouping=NULL, corArgs = NULL, ...) {
  # Check if the matrix returned by corFun is positive definite. 
  # internally uses .checkVarCovMatrix
  # Args:
  #   dataset: JASP dataset.
  #   target: String vector indicating the target variables.
  #   corFun: a function that calculates a correlation matrix or covariance matrix (e.g. cor or cov are recommended)
  #   corArgs: more arguments to corFun, i.e. use = "pairwise".
  #   grouping: String vector indicating the grouping variables.
  
  result <- list(error=FALSE, errorVars=NULL)
  
  if (is.null(corFun))
    corFun <- stats::cor

  if (is.null(grouping)) {
    dataset <- list(dataset[, .v(target)])
  } else {
    groupingData <- dataset[, .v(grouping)]
    dataset <- dataset[, .v(target)]
    dataset <- split(dataset, groupingData)
  }
  for (d in seq_along(dataset)) {
    
    cormat <- do.call(corFun, c(list(dataset[[d]]), corArgs))
    err <- .checkVarCovMatrix(cormat, nrow = FALSE, symm = FALSE)
    result$error <- err$error
    result$message <- err$reason
    result$errorVars <- err$vars
    if (result$error) # stop at first error
      break
  }
  
  return(result)
}

.checkModelInteractions <- function(modelTerms) {
  # In case of interactions, check whether all main effects and lower-order interaction terms are in the model
  # Args:
  #   modelTerms: a list of models terms, generated by specifying `listViewType: "Interaction"` in an AssignedVariablesList

  result <- list(error=FALSE, errorVars=NULL)

  if (length(modelTerms) == 0)
    return(result)

  for (term in modelTerms) {

    if (is.null(names(term)) || !"components" %in% names(term))
      stop("the modelterms list must contain the named item `components`")
      
    components <- term$components
    if (length(components) < 2)
      next

    numChildren <- 2^length(components) - 1
    inclusion <- sapply(modelTerms, 
      function(termToFind) {
          componentsToFind <- termToFind$components
          return(sum(componentsToFind %in% components) == length(componentsToFind))
      }
    )

    if (sum(inclusion) != numChildren)
      result$error <- TRUE

  }

  return(result)
}

.checkNegativeValues <- function(dataset, target, grouping=NULL, groupingLevel=NULL) {
  # Check for negative values in the dataset. 
  # Args:
  #   dataset: JASP dataset.
  #   target: String vector indicating the target variables.
  #   grouping: String vector indicating the grouping variables.
  #   groupingLevel: Vector indicating the level of each of the grouping variables.
  
  result <- list(error=FALSE, errorVars=NULL)
  
  findNegativeValues <- function(x) {
    return(any(na.omit(x) < 0))
  }
  
  for (v in target) {
    
    if (is.factor(dataset[[.v(v)]])) { # Coerce factor to numeric.
      dataset[[.v(v)]] <- as.numeric(as.character(dataset[[.v(v)]]))
    } 
    
    if (length(grouping) > 0 && length(groupingLevel) > 0) { 
      hasNegativeValues <- .applyOnGroups(findNegativeValues, dataset, v, grouping, groupingLevel)
    } else {
      hasNegativeValues <- findNegativeValues(dataset[[.v(v)]])
    }
    
    if (hasNegativeValues) {
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  
  return(result)
}

.checkMissingValues <- function(dataset, target, grouping=NULL, groupingLevel=NULL) {
  # Check for missing values in the dataset. 
  # Args:
  #   dataset: JASP dataset.
  #   target: String vector indicating the target variables.
  #   grouping: String vector indicating the grouping variables.
  #   groupingLevel: Vector indicating the level of each of the grouping variables.
  
  result <- list(error=FALSE, errorVars=NULL)
  
  findMissingValues <- function(x) {
    return(anyNA(x))
  }
  
  for (v in target) {

    if (length(grouping) > 0 && length(groupingLevel) > 0) { 
      hasMissingValues <- .applyOnGroups(findMissingValues, dataset, v, grouping, groupingLevel)
    } else {
      hasMissingValues <- findMissingValues(dataset[[.v(v)]])
    }
    
    if (hasMissingValues) {
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  
  return(result)
}

.checkDuplicateColumns <- function(dataset, target = NULL, grouping = NULL) {
  # Check for duplicate columns in the dataset.
  # Args:
  #   dataset: JASP dataset.
  #   target: String vector indicating the target variables. Duplicates not in this vector do not trigger an error. NULL implies all variables in the dataset are checked for duplicates.
  #   grouping: String vector indicating the grouping variables.

  result <- list(error=FALSE, errorVars=NULL)

  findDuplicates <- function(data) {

    nms <- names(data)
    duplicatedVars <- character()
    nc <- length(nms)
    for (i in 1:(nc - 1L))
      for (j in (i + 1L):nc)
        if (identical(data[[nms[i]]], data[[nms[j]]]))
          duplicatedVars <- c(duplicatedVars, nms[i], nms[j])

    if (identical(duplicatedVars, character())) {
      return(duplicatedVars)
    } else {
      return(.unv(unique(duplicatedVars)))
    }
  }

  if (is.null(target)) {
    targetB64 <- colnames(dataset)
  } else {
    targetB64 <- .v(target)
  }

  if (length(target) > 1L) {

    if (is.null(grouping)) {
      dataset <- list(dataset[, targetB64])
    } else {
      groupingData <- dataset[, .v(grouping)]
      dataset <- dataset[, targetB64]
      dataset <- split(dataset, groupingData)
    }

    duplicatedVars <- character()
    for (d in seq_along(dataset))
      duplicatedVars <- c(duplicatedVars, findDuplicates(dataset[[d]]))

    if (length(duplicatedVars) > 0L) {
      result$error <- TRUE
      result$errorVars <- unique(duplicatedVars)
    }
  }

  return(result)
}
